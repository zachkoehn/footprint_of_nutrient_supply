# read and clean
library(tidyverse)
library(here)


production <- read_csv(here("data","all_groups_tonnes_prot_kcal.csv")) %>%
  rename(paper_category=product) %>%
  mutate(
    paper_category=str_replace_all(paper_category,"bana","banana"),
    paper_category=str_replace_all(paper_category,"barl","barley"),
    paper_category=str_replace_all(paper_category,"Benthic","benthic"),
    paper_category=str_replace_all(paper_category,"bivalves","bivalve"),
    paper_category=str_replace_all(paper_category,"buffaloes_milk","buffalo milk"),
    paper_category=str_replace_all(paper_category,"cass","cassava"),
    paper_category=str_replace_all(paper_category,"coco","cocoa"),
    paper_category=str_replace_all(paper_category,"chickens_eggs","chicken eggs"),
    paper_category=str_replace_all(paper_category,"chickens_meat","chicken meat"),
    paper_category=str_replace_all(paper_category,"cows_meat","cattle meat"),
    paper_category=str_replace_all(paper_category,"cows_milk","cow milk"),
    paper_category=str_replace_all(paper_category,"cnut","coconuts"),
    paper_category=str_replace_all(paper_category,"Demersal","demersal"),
    paper_category=str_replace_all(paper_category,"goats_meat","goat meat"),
    paper_category=str_replace_all(paper_category,"goats_milk","goat milk"),
    paper_category=str_replace_all(paper_category,"forage_fish","forage fish"),
    paper_category=str_replace_all(paper_category,"Large pelagic","large pelagic"),
    paper_category=str_replace_all(paper_category,"maiz","maize"),
    paper_category=str_replace_all(paper_category,"marine-fish-general","marine fish general"),
    paper_category=str_replace_all(paper_category,"Medium pelagic","medium pelagic"),
    paper_category=str_replace_all(paper_category,"ocer","cereal other"),
    paper_category=str_replace_all(paper_category,"ofib","fibres"),
    paper_category=str_replace_all(paper_category,"oilp","oilpalm"),
    paper_category=str_replace_all(paper_category,"orts","roots other"),
    paper_category=str_replace_all(paper_category,"othr","other"),
    paper_category=str_replace_all(paper_category,"pigs_meat","pig meat"),
    paper_category=str_replace_all(paper_category,"plnt","plantain"),
    paper_category=str_replace_all(paper_category,"pota","potato"),
    paper_category=str_replace_all(paper_category,"Reef-associated","reef"),
    paper_category=str_replace_all(paper_category,"rice","rice"),
    paper_category=str_replace_all(paper_category,"sheep_meat","sheep meat"),
    paper_category=str_replace_all(paper_category,"sheep_milk","sheep milk"),
    paper_category=str_replace_all(paper_category,"salmonids","salmon"),
    paper_category=str_replace_all(paper_category,"shrimps_prawns","shrimp"),
    paper_category=str_replace_all(paper_category,"Small pelagic","small pelagic"),
    paper_category=str_replace_all(paper_category,"sorg","sorghum"),
    paper_category=str_replace_all(paper_category,"soyb","soybean"),
    paper_category=str_replace_all(paper_category,"spis","spices"),
    paper_category=str_replace_all(paper_category,"sugb","sugarbeet"),
    paper_category=str_replace_all(paper_category,"sugc","sugarcane"),
    paper_category=str_replace_all(paper_category,"swpo","sweet potato"),
    paper_category=str_replace_all(paper_category,"tnut","tree nut"),
    paper_category=str_replace_all(paper_category,"vege","vegetables"),
    paper_category=str_replace_all(paper_category,"whea","wheat"),
    paper_category=str_replace_all(paper_category,"xfru","fruit"),
    paper_category=str_replace_all(paper_category,"xmil","millet"),
    paper_category=str_replace_all(paper_category,"xoil","oil crops"),
    paper_category=str_replace_all(paper_category,"xpul","pulses"),
    paper_category=str_replace_all(paper_category,"yams","yams"),
    paper_category=ifelse(paper_category=="fish","freshwater fish",paper_category)
  )

# read nutrient data
nutrient_afcd <- read_csv(file=here("outputs","data","afcd_extract.csv"))
nutrient_usda <- read_csv(file=here("outputs","data","usda_api_extract.csv"))
# read nutrient data
pressure_proportional_data <- readxl::read_xlsx(path = here("data","41893_2022_965_MOESM2_ESM.xlsx"),sheet="Supplementary Data 8") %>%
  rename(
    paper_category='Food Name',
    proportional_contribution='Proportional Contribution',
    pressure='Pressure'
  ) %>%
  mutate(
    paper_category=str_to_lower(paper_category),
    paper_category=str_replace_all(paper_category,"bivalves","bivalve"),
    paper_category=str_replace_all(paper_category,"buffaloes milk","buffalo milk"),
    paper_category=str_replace_all(paper_category,"chickens eggs","chicken eggs"),
    paper_category=str_replace_all(paper_category,"chickens meat","chicken meat"),
    paper_category=str_replace_all(paper_category,"coconut","coconuts"),
    paper_category=str_replace_all(paper_category,"cows milk","cow milk"),
    paper_category=str_replace_all(paper_category,"forage","forage fish"),
    paper_category=str_replace_all(paper_category,"goats meat","goat meat"),
    paper_category=str_replace_all(paper_category,"goats milk","goat milk"),
    paper_category=str_replace_all(paper_category,"large-pelagic","large pelagic"),
    paper_category=str_replace_all(paper_category,"marine-fish-general","marine fish general"),
    paper_category=str_replace_all(paper_category,"medium-pelagic","medium pelagic"),
    paper_category=str_replace_all(paper_category,"other cereals","cereal other"),
    paper_category=str_replace_all(paper_category,"other fruits","fruit"),
    paper_category=str_replace_all(paper_category,"other oil crops","oil crops"),
    paper_category=str_replace_all(paper_category,"other roots","roots other"),
    paper_category=str_replace_all(paper_category,"pigs meat","pig meat"),
    paper_category=str_replace_all(paper_category,"salmonids","salmon"),
    paper_category=str_replace_all(paper_category,"small-pelagic","small pelagic"),
    paper_category=str_replace_all(paper_category,"tree nuts","tree nut")
  )  %>%
  distinct() %>%
  group_by(paper_category) %>%
  summarize(
    proportion_total_pressure=sum(proportional_contribution)
  )

country_pressure_data <- readxl::read_xlsx(path = here("data","41893_2022_965_MOESM2_ESM.xlsx"),sheet="Supplementary Data 3") %>%
  rename(
    paper_category=Product,
    pressure_cumulative='Cumulative pressure',
    tonnes='Tonnes',
    pressure_per_tonne='Pressure per tonne',
    country='Country',
    pressure_per_million_kcal='Pressure per million kcal',
    kcal_million='Million kcal',
    isoc3=ISO3c
    ) %>%
  mutate(
    paper_category=str_to_lower(paper_category),
    paper_category=str_replace_all(paper_category,"bana","banana"),
    paper_category=str_replace_all(paper_category,"barl","barley"),
    paper_category=str_replace_all(paper_category,"Benthic","benthic"),
    paper_category=str_replace_all(paper_category,"buffaloes_milk","buffalo milk"),
    paper_category=str_replace_all(paper_category,"bivalves","bivalve"),
    paper_category=str_replace_all(paper_category,"cass","cassava"),
    paper_category=str_replace_all(paper_category,"coco","cocoa"),
    paper_category=str_replace_all(paper_category,"chickens_eggs","chicken eggs"),
    paper_category=str_replace_all(paper_category,"chickens_meat","chicken meat"),
    paper_category=str_replace_all(paper_category,"cattle_meat","cattle meat"),
    paper_category=str_replace_all(paper_category,"cows_milk","cow milk"),
    paper_category=str_replace_all(paper_category,"cnut","coconuts"),
    paper_category=str_replace_all(paper_category,"Demersal","demersal"),
    paper_category=str_replace_all(paper_category,"forage_fish","forage fish"),
    paper_category=str_replace_all(paper_category,"goats_meat","goat meat"),
    paper_category=str_replace_all(paper_category,"goats_milk","goat milk"),
    paper_category=str_replace_all(paper_category,"large-pelagic","large pelagic"),
    paper_category=str_replace_all(paper_category,"maiz","maize"),
    paper_category=str_replace_all(paper_category,"marine-fish-general","marine fish general"),
    paper_category=str_replace_all(paper_category,"medium-pelagic","medium pelagic"),
    paper_category=str_replace_all(paper_category,"ocer","cereal other"),
    paper_category=str_replace_all(paper_category,"ofib","fibres"),
    paper_category=str_replace_all(paper_category,"oilp","oilpalm"),
    paper_category=str_replace_all(paper_category,"orts","roots other"),
    paper_category=str_replace_all(paper_category,"othr","other"),
    paper_category=str_replace_all(paper_category,"pigs_meat","pig meat"),
    paper_category=str_replace_all(paper_category,"plnt","plantain"),
    paper_category=str_replace_all(paper_category,"pota","potato"),
    paper_category=str_replace_all(paper_category,"Reef-associated","reef"),
    paper_category=str_replace_all(paper_category,"rice","rice"),
    paper_category=str_replace_all(paper_category,"sheep_meat","sheep meat"),
    paper_category=str_replace_all(paper_category,"sheep_milk","sheep milk"),
    paper_category=str_replace_all(paper_category,"salmonids","salmon"),
    paper_category=str_replace_all(paper_category,"shrimps_prawns","shrimp"),
    paper_category=str_replace_all(paper_category,"small-pelagic","small pelagic"),
    paper_category=str_replace_all(paper_category,"sorg","sorghum"),
    paper_category=str_replace_all(paper_category,"soyb","soybean"),
    paper_category=str_replace_all(paper_category,"spis","spices"),
    paper_category=str_replace_all(paper_category,"sugb","sugarbeet"),
    paper_category=str_replace_all(paper_category,"sugc","sugarcane"),
    paper_category=str_replace_all(paper_category,"swpo","sweet potato"),
    paper_category=str_replace_all(paper_category,"tnut","tree nut"),
    paper_category=str_replace_all(paper_category,"vege","vegetables"),
    paper_category=str_replace_all(paper_category,"whea","wheat"),
    paper_category=str_replace_all(paper_category,"xfru","fruit"),
    paper_category=str_replace_all(paper_category,"xmil","millet"),
    paper_category=str_replace_all(paper_category,"xoil","oil crops"),
    paper_category=str_replace_all(paper_category,"xpul","pulses"),
    paper_category=str_replace_all(paper_category,"yams","yams"),
    paper_category=str_replace_all(paper_category,"_"," ")
  ) %>%
  # filter(
  #   str_detect(paper_category,"feed")==FALSE
  # ) %>%
  select(country,isoc3,paper_category,pressure_per_tonne,pressure_cumulative,tonnes,kcal_million,pressure_per_million_kcal)
  



nutrients <- nutrient_afcd %>%
  # rename(product=taxa_to_match,nutrient_name=nutrient,nutrient_value=value) %>%
  select(product,paper_category,nutrient_name,nutrient_value,type) %>%
  rbind(nutrient_usda) %>%
  mutate(
    percent_rdi=case_when(
      # all RDI values and sources available in supplementary information
      nutrient_name=='Protein' ~ nutrient_value/50, #IOM
      nutrient_name=='Calcium' ~ nutrient_value/1000, #USDA
      nutrient_name=='DHA_EPA' ~ nutrient_value/0.45, #Murray et al 2020
      nutrient_name=='Fibre' ~ nutrient_value/25, #USDA
      nutrient_name=='Folate' ~ nutrient_value/400, #USDA
      nutrient_name=='Iron' ~ nutrient_value/18, #IOM
      nutrient_name=='Magnesium' ~ nutrient_value/320, #USDA
      nutrient_name=='Potassium' ~ nutrient_value/2600, #USDA
      nutrient_name=='Riboflavin' ~ nutrient_value/1.1, #USDA
      nutrient_name=='Selenium' ~ nutrient_value/55, #from Harvard Nutrition Source
      nutrient_name=='Thiamin' ~ nutrient_value/1.5, #USDA
      nutrient_name=='Vit. A RAE' ~ nutrient_value/700, #IOM
      nutrient_name=='Vit. B12' ~ nutrient_value/2.4, #IOM
      nutrient_name=='Zinc' ~ nutrient_value/8.3, #IOM
      nutrient_name=='Energy_kcal' ~ nutrient_value/2000
    ),
    paper_category=str_replace_all(paper_category,"bana","banana"),
    paper_category=str_replace_all(paper_category,"barl","barley"),
    paper_category=str_replace_all(paper_category,"Benthic","benthic"),
    paper_category=str_replace_all(paper_category,"buffaloes_milk","buffalo milk"),
    paper_category=str_replace_all(paper_category,"bivalves","bivalve"),
    paper_category=str_replace_all(paper_category,"cass","cassava"),
    paper_category=str_replace_all(paper_category,"coco","cocoa"),
    paper_category=str_replace_all(paper_category,"chickens_eggs","chicken eggs"),
    paper_category=str_replace_all(paper_category,"chickens_meat","chicken meat"),
    paper_category=str_replace_all(paper_category,"cows_meat","cattle meat"),
    paper_category=str_replace_all(paper_category,"cows_milk","cow milk"),
    paper_category=str_replace_all(paper_category,"cnut","coconuts"),
    paper_category=str_replace_all(paper_category,"Demersal","demersal"),
    paper_category=str_replace_all(paper_category,"forage_fish","forage fish"),
    paper_category=str_replace_all(paper_category,"goats_meat","goat meat"),
    paper_category=str_replace_all(paper_category,"goats_milk","goat milk"),
    paper_category=str_replace_all(paper_category,"Large pelagic","large pelagic"),
    paper_category=str_replace_all(paper_category,"maiz","maize"),
    paper_category=str_replace_all(paper_category,"marine_fish_general","marine fish general"),
    paper_category=str_replace_all(paper_category,"Medium pelagic","medium pelagic"),
    paper_category=str_replace_all(paper_category,"ocer","cereal other"),
    paper_category=str_replace_all(paper_category,"ofib","fibres"),
    paper_category=str_replace_all(paper_category,"oilp","oilpalm"),
    paper_category=str_replace_all(paper_category,"orts","roots other"),
    paper_category=str_replace_all(paper_category,"othr","other"),
    paper_category=str_replace_all(paper_category,"pigs_meat","pig meat"),
    paper_category=str_replace_all(paper_category,"plnt","plantain"),
    paper_category=str_replace_all(paper_category,"pota","potato"),
    paper_category=str_replace_all(paper_category,"Reef-associated","reef"),
    paper_category=str_replace_all(paper_category,"rice","rice"),
    paper_category=str_replace_all(paper_category,"sheep_meat","sheep meat"),
    paper_category=str_replace_all(paper_category,"sheep_milk","sheep milk"),
    paper_category=str_replace_all(paper_category,"salmonids","salmon"),
    paper_category=str_replace_all(paper_category,"shrimps_prawns","shrimp"),
    paper_category=str_replace_all(paper_category,"Small pelagic","small pelagic"),
    paper_category=str_replace_all(paper_category,"sorg","sorghum"),
    paper_category=str_replace_all(paper_category,"soyb","soybean"),
    paper_category=str_replace_all(paper_category,"spis","spices"),
    paper_category=str_replace_all(paper_category,"sugb","sugarbeet"),
    paper_category=str_replace_all(paper_category,"sugc","sugarcane"),
    paper_category=str_replace_all(paper_category,"swpo","sweet potato"),
    paper_category=str_replace_all(paper_category,"tnut","tree nut"),
    paper_category=str_replace_all(paper_category,"vege","vegetables"),
    paper_category=str_replace_all(paper_category,"whea","wheat"),
    paper_category=str_replace_all(paper_category,"xfru","fruit"),
    paper_category=str_replace_all(paper_category,"xmil","millet"),
    paper_category=str_replace_all(paper_category,"xoil","oil crops"),
    paper_category=str_replace_all(paper_category,"xpul","pulses"),
    paper_category=str_replace_all(paper_category,"yams","yams")
    )  %>%
  # group_by(paper_category) %>%
  distinct() %>%
  mutate(
    nutrient_value_per_tonne=nutrient_value*10000
  )


# food group categories used
food_group_categories <- nutrients %>%
  select(paper_category,type) %>%
  distinct() %>%
  rename(sector=type) %>%
  mutate(
    food_system=case_when(
      sector %in% c("terrestrial") ~ "terrestrial",
      sector %in% c("mariculture","fisheries") ~ "aquatic"
    ),
    food_sector=case_when(
      paper_category %in% c(
        "banana","barley","cassava","cereal other","cocoa","coconuts","fibres",
        "fruit","maize","millet","oil crops","oilpalm","other","plantain","potato",
        "pulses","rice","roots other","sorghum","soybean","spices",
        "sugarbeet","sugarcane","sweet potato","tree nut","vegetables","wheat","yams"
      ) ~ "crops",
      paper_category %in% c(
        "buffalo milk","cattle meat","chicken eggs","chicken meat",
        "cow milk","goat meat","pig meat","sheep meat","sheep milk"
      )~ "livestock",
      paper_category %in% c(
        "benthic","demersal","forage fish","freshwater fish",
        "large pelagic","medium pelagic","reef","small pelagic"
      ) ~ "marine and freshwater fisheries",
      paper_category %in% c(
        "bivalve","crustaceans","marine fish general",
        "salmon","shrimp","tuna"
      ) ~ "mariculture"
    )
  ) %>%
  select(paper_category,food_sector,food_system) %>%
  # add foods not in the nutrient dataset
  rbind(data.frame(
    paper_category=c(
      "cocoa","oilpalm","sugarcane","goat milk",
      "feed buffalo milk","feed chicken eggs","feed chicken meat","feed cattle meat","feed cow milk","feed goat meat",
      "feed goat milk","feed pig meat","feed sheep meat","feed sheep milk",
      "feed marine fish general meat","feed salmon meat","feed shrimp meat","feed tuna meat",
      "feed crustaceans meat"
    ),
    food_sector=c(
      "crops","crops","crops","livestock",
      "livestock","livestock","livestock","livestock","livestock","livestock",
      "livestock","livestock","livestock","livestock",
      "marine and freshwater fisheries","marine and freshwater fisheries","marine and freshwater fisheries","marine and freshwater fisheries",
      "marine and freshwater fisheries"
      
    ),
    food_system=c(
      "terrestrial","terrestrial","terrestrial","terrestrial",
      "terrestrial","terrestrial","terrestrial","terrestrial","terrestrial","terrestrial",
      "terrestrial","terrestrial","terrestrial","terrestrial",
      "aquatic","aquatic","aquatic","aquatic",
      "aquatic"
    )
  )
  )

paper_category_nutrient_richness <- nutrients %>%
  select(paper_category,product,nutrient_name,percent_rdi) %>%
  left_join(food_group_categories,by="paper_category") %>%
  group_by(paper_category,food_sector,food_system,product,nutrient_name) %>%
  pivot_wider(names_from=nutrient_name,values_from = percent_rdi) %>%
  # commented out text calculates missingness by food sector and by nutrient
  # and outputs as a pdf for supplemental figure
  # select(paper_category,food_sector,food_system,product,Energy_kcal,everything(.),-"NA") %>%
  # pivot_longer(Calcium:Zinc,names_to="nutrient_name",values_to="values") %>%
  # mutate(
  #   is_na=ifelse(is.na(values),1,0),
  #   dummy=1
  # ) %>%
  # group_by(food_sector,nutrient_name) %>%
  # summarise(
  #   sum_na=sum(is_na),
  #   sum_total=sum(dummy)
  # ) %>%
  # ungroup() %>%
  # mutate(perc_na=sum_na/sum_total) %>% 
  # group_by(food_sector,nutrient_name) %>%
  # summarise(perc_missing=sum(perc_na)) %>%
  # ggplot(aes(x=perc_missing,y=food_sector,fill=food_sector)) +
  # geom_bar(stat="identity")+
  # ylab("") +
  # xlab("% missing data") +
  # theme_bw() +
  # theme(
  #   legend.position = "top",
  #   axis.text.y = element_blank(),
  #   axis.ticks = element_blank(),
  #   strip.background = element_rect(fill="grey90")
  # ) +
  # scale_fill_manual(
  #   values = c(
  #     "mariculture"="#6667AB",
  #     "marine and freshwater fisheries"="#70aead",
  #     "crops"="#a7982f",
  #     "livestock"="#B3832F"
  #   )
  # ) +
  # scale_x_continuous(labels = scales::percent) +
  # facet_wrap(~nutrient_name)
  # ggsave(
  #   paper_category_nutrient_richness,
  #   filename=here("outputs","plots","suppl missingness by sector.pdf"),
  #   width=8.5,height = 6,  units = "in")
  mutate(
    across(Calcium:Zinc,function(x) ifelse(is.na(x==TRUE),0,x)),
    across(Calcium:Zinc,function(x) ifelse(x>=1,1,x)),
    rdi_sum=rowSums(across(Calcium:Zinc)),
    nutrient_density=rdi_sum/14
  ) %>%
  # View()
  ungroup() %>%
  select(-product) %>%
  distinct() %>%
  group_by(paper_category) %>%
  summarize(nutrient_density=median(nutrient_density,na.rm=TRUE)) %>%
  select(paper_category,nutrient_density)
# add in nutrient richness
nutrients <- nutrients %>%
  left_join(paper_category_nutrient_richness)


# Cleaning data of global cumulative proportion data for direct and feed
# source: Halpern et al SI, sheet "Supplementary Data 8"
proportion_data <- readxl::read_xlsx(path = here("data","41893_2022_965_MOESM2_ESM.xlsx"),sheet="Supplementary Data 8") %>%
  rename(
    paper_category='Food Name',
    pressure=Pressure,
    global_proportion='Proportional Contribution'
    ) %>%
  mutate(
    paper_category=str_to_lower(paper_category),
    paper_category=str_replace_all(paper_category,"bivalves","bivalve"),
    paper_category=str_replace_all(paper_category,"buffaloes milk","buffalo milk"),
    paper_category=str_replace_all(paper_category,"chickens eggs","chicken eggs"),
    paper_category=str_replace_all(paper_category,"chickens meat","chicken meat"),
    paper_category=str_replace_all(paper_category,"coconut","coconuts"),
    paper_category=str_replace_all(paper_category,"cows milk","cow milk"),
    paper_category=str_replace_all(paper_category,"forage","forage fish"),
    paper_category=str_replace_all(paper_category,"goats meat","goat meat"),
    paper_category=str_replace_all(paper_category,"goats milk","goat milk"),
    paper_category=str_replace_all(paper_category,"large-pelagic","large pelagic"),
    paper_category=str_replace_all(paper_category,"marine-fish-general","marine fish general"),
    paper_category=str_replace_all(paper_category,"medium-pelagic","medium pelagic"),
    paper_category=str_replace_all(paper_category,"other cereals","cereal other"),
    paper_category=str_replace_all(paper_category,"other fruits","fruit"),
    paper_category=str_replace_all(paper_category,"other oil crops","oil crops"),
    paper_category=str_replace_all(paper_category,"other roots","roots other"),
    paper_category=str_replace_all(paper_category,"pigs meat","pig meat"),
    paper_category=str_replace_all(paper_category,"salmonids","salmon"),
    paper_category=str_replace_all(paper_category,"small-pelagic","small pelagic"),
    paper_category=str_replace_all(paper_category,"tree nuts","tree nut")
  ) 


rm(nutrient_afcd,nutrient_usda)


