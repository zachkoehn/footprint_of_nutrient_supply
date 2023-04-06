# ===============================================================================
# Pulls from New USDA database and merge with existing data (Food Data Central)
#
#
# Name: J. Zachary (Zach) Koehn
# Email: zkoehn@gmail.com
# Date started: 08/31/2022
# Revised: 09/06/2022
# ===============================================================================

library(tidyverse)
library(here)
library(jsonlite)
library(pbapply)
library(data.table)

#author's API key
source(here("scripts","usda_api_key.R")) 
# others using this script will need to create one, labelling it
# "api.key" for use with this script. For more info on how to do this, see:
# https://fdc.nal.usda.gov/api-guide.html



# ________________________________________________________________________________________________
# Scrape USDA database for nutrient information to integrate with existing GENuS-FISH
# : broken down by metadata summary, macronutrients and micronutrients, fatty acids
# : because of limits on free use, had to break down the API calls, which was done by nutrient groups
# ________________________________________________________________________________________________

# gathered from all the food.csv's using manual search (in vegetables), only found in SR Legacy and FNDSS (codes included below for that one)
original_land <- read_csv(here("data","food_categories_Nov_2022.csv")) %>%
  rename(paper_category=SPAM_super) %>%
  drop_na(fdc) %>%
  select(item_code:fdc)

fdc_id_unique <- original_land$fdc



usda_api_call <- function(x) {
  url <- paste0("https://api.nal.usda.gov/fdc/v1/food/",fdc_id_unique[x],"?api_key=",api.key,"&nutrients=")
  raw <- fromJSON(txt=url,flatten = TRUE)
  return(raw)
}

# need to complete the calls in two steps due to API 
store_list <- list()

usda_request <- pblapply(seq_along(fdc_id_unique),function(x) {
  tryCatch (
    
    store_list<- append(store_list,usda_api_call(x)) ,
    error = function(e){
      message(paste("An error occurred for item", x, 404,":\n"), e)
      
    })
}
)
# items with bad calls
fdc_id_unique[103];fdc_id_unique[108]
original_land %>%
  filter(fdc %in% c(31101010,11116000))

extract_nutrient_data_from_list <- function(list_obj) {
  # list_obj <- hour_1_request[[600]]
  if(is.null(list_obj)==TRUE) {
    df <- data.frame(
      fdc_id=NA,
      food_description=NA,
      scientific_name=NA,
      nutrient_name=NA,
      nutrient_unit=NA,
      nutrient_value=NA
      )
  } else {
    df <- data.frame(
      fdc_id=ifelse(is.null(list_obj$fdcId)==TRUE,"none",list_obj$fdcId),
      food_description=ifelse(is.null(list_obj$description)==TRUE,"none",list_obj$description),
      scientific_name=ifelse(is.null(list_obj$scientificName)==TRUE,"none",list_obj$scientificName),
      nutrient_name=if(is.null(list_obj$foodNutrients$nutrient.name)==FALSE){list_obj$foodNutrients$nutrient.name} else {"none"},
      nutrient_unit=if(is.null(list_obj$foodNutrients$nutrient.unitName)==FALSE){list_obj$foodNutrients$nutrient.unitName} else {"none"},
      nutrient_value=if(is.null(list_obj$foodNutrients$amount)==FALSE){list_obj$foodNutrients$amount} else {"none"}
      )
  }  
  return(df)
}


usda_list_simplified <- pblapply(1:length(usda_request), function(x) extract_nutrient_data_from_list(list_obj = usda_request[[x]]))

usda_df <- rbindlist(usda_list_simplified) %>%
  # filter(nutrient_name %in% c("Energy"))
  mutate(
  nutrient_value=as.numeric(nutrient_value),
  nutrient_name=str_replace_all(nutrient_name,"Energy \\(Atwater General Factors\\)","Energy_general"),
  nutrient_name=ifelse(nutrient_name=="Energy",paste0(nutrient_name,"_",nutrient_unit),nutrient_name),
  nutrient_name=str_replace_all(nutrient_name,"PUFA 20:5 n-3 \\(EPA\\)","EPA"),
  nutrient_name=str_replace_all(nutrient_name,"PUFA 22:6 n-3 \\(DHA\\)","DHA")
  ) %>%
  filter(
    nutrient_name %in% c(
      "EPA",
      "DHA",
      "Fiber, total dietary",
      "Energy_general",
      "Energy_kcal",
      "Energy_kJ",
      "Folate, total",
      "Calcium, Ca",
      "Iron, Fe",
      "Magnesium, Mg",
      "Potassium, K",
      "Protein",
      "Riboflavin",
      "Thiamin",
      "Vitamin A, RAE",
      "Vitamin B-12",
      "Selenium, Se",
      "Zinc, Zn"
    )
  ) %>% mutate(
    nutrient_name=str_replace_all(nutrient_name,"Fiber, total dietary","Fibre"),
    nutrient_name=str_replace_all(nutrient_name,"Folate, total","Folate"),
    nutrient_name=str_replace_all(nutrient_name,"Iron, Fe","Iron"),
    nutrient_name=str_replace_all(nutrient_name,"Vitamin B-12","Vit. B12"),
    nutrient_name=str_replace_all(nutrient_name,"Calcium, Ca","Calcium"),
    nutrient_name=str_replace_all(nutrient_name,"Magnesium, Mg","Magnesium"),
    nutrient_name=str_replace_all(nutrient_name,"Potassium, K","Potassium"),
    nutrient_name=str_replace_all(nutrient_name,"Selenium, Se","Selenium"),
    nutrient_name=str_replace_all(nutrient_name,"Vitamin A, RAE","Vit. A RAE"),
    nutrient_name=str_replace_all(nutrient_name,"Zinc, Zn","Zinc")
  ) 

usda_extract <- usda_df %>%
  select(fdc_id,nutrient_name,nutrient_value) %>%
  distinct() %>%
  pivot_wider(names_from = nutrient_name,values_from = nutrient_value) %>%
  mutate(
    DHA_EPA=DHA+EPA,
    Energy_kcal=ifelse(is.na(Energy_kcal),Energy_general,Energy_kcal)
  ) %>%
  select(-c(DHA,EPA,Energy_general,Energy_kJ)) %>%
  pivot_longer(Protein:DHA_EPA,names_to = "nutrient_name",values_to="nutrient_value") %>%
  rename(fdc=fdc_id) %>%
  left_join(original_land,by="fdc") %>%
  select(Item,paper_category,nutrient_name,nutrient_value) %>%
  rename(product=Item) %>%
  mutate(type="terrestrial") %>%
  select(product,paper_category,nutrient_name,nutrient_value,type)

  

write.csv(usda_extract,file=here("outputs","data","usda_api_extract.csv"),row.names = FALSE)

