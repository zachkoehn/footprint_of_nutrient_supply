library(here)
library(tidyverse)
library(AFCD)
library(viridis)

original_fisheries <- read_csv(here("data","fisheries_spp_groups.csv")) %>%
  rename(paper_category=species_class_final)

original_mariculture <- read_csv(here("data","mariculture_spp_groups.csv")) %>%
  rename(paper_category=aq_group)

original_land <- read_csv(here("data","food_categories_Nov_2022.csv")) %>%
  rename(paper_category=SPAM_super)

all_fao <- read_csv(here("data","fishstatj_all_production.csv"))


aquatic_common <- str_to_lower(unique(original_mariculture$species))
aquatic_scinames <- unique(original_fisheries$TaxonName)

afcd_scinames <- unique(afcd$sciname)
afcd_common <- unique(afcd$common_name)


# clean freshwater
names(all_fao)[1:12] <- c(
  "iso3c",
  "country",
  "asfis_taxon_code",
  "asfis_species_common",
  "asfis_species_isscaap_group",
  "asfis_species_sci",
  "asfis_species_3a",
  "asfis_species_isscaap_division",
  "fao_fishing_area",
  "production_source",
  "unit_name",
  "unit"
)

original_freshwater <- all_fao %>%
  mutate(
    across("[1950]":"S...154",as.numeric)
  ) %>%
  pivot_longer("[1950]":"S...154",names_to="year") %>%
  drop_na(value) %>%
  mutate(
    year=str_replace_all(year,"\\[",""),
    year=str_replace_all(year,"\\]",""),
    year=as.numeric(year)
  ) %>%
  filter(
    year %in% c(1997:2014),
    asfis_species_isscaap_division %in% c("Diadromous fishes","Freshwater fishes"),
    str_detect(fao_fishing_area,"Inland")
  )

fresh_clean <- data.frame(
  paper_category="freshwater fish",
  taxa_to_match=unique(original_freshwater$asfis_species_sci),
  type="fisheries"
  )

# clean mariculture
mari_clean <- original_mariculture %>%
  rename(taxa_to_match=family) %>%
  select(paper_category,taxa_to_match) %>%
  distinct() %>%
  mutate(
    type="mariculture"
  )
# clean fisheries
fish_clean <- original_fisheries %>%
  rename(taxa_to_match=TaxonName) %>%
  select(paper_category,taxa_to_match)%>%
  distinct() %>%
  mutate(
    type="fisheries"
  )

aquatic_clean <- rbind(mari_clean,fish_clean,fresh_clean) %>%
  mutate(taxa_to_match=str_to_lower(taxa_to_match))

aquatic_afcd <- species_nutrients(
  sci_name = aquatic_clean$taxa_to_match,
  nut= c(
    "Calcium",
    "DHA_EPA",
    "Energy_total_combined",
    "Fibre, total; gravimetrically determined",
    "Folate, total",
    "Iron, total",
    "Magnesium",
    "Potassium",
    "Protein, total; calculated from total nitrogen",
    "Riboflavin",
    "Selenium",
    "Thiamin",
    "Vitamin A; sum of retinol/carotenoids, expressed as retinol activity equivalent (RAE)",
    "Vitamin B12",
    "Zinc"),
  part = "muscle tissue",
  prep = "raw"
  ) %>%
  rename(taxa_to_match=species) %>%
  filter(taxa_match %in% c("family","genus","species"))

mariculture_afcd <- mari_clean %>%
  mutate(
    taxa_to_match=str_to_lower(taxa_to_match)
  ) %>%
  left_join(aquatic_afcd,by="taxa_to_match") 

fisheries_afcd <- fish_clean %>%
  mutate(
    taxa_to_match=str_to_lower(taxa_to_match)
  ) %>%
  left_join(aquatic_afcd,by="taxa_to_match") 

freshwater_afcd <- fresh_clean %>%
  mutate(
    taxa_to_match=str_to_lower(taxa_to_match)
  ) %>%
  left_join(aquatic_afcd,by="taxa_to_match") 

afcd_all <- rbind(mariculture_afcd,fisheries_afcd,freshwater_afcd)  %>%
  rename(product=taxa_to_match,nutrient_name=nutrient,nutrient_value=value) %>%
  select(product,paper_category,nutrient_name,nutrient_value,type) %>%
  mutate(
    nutrient_name=str_replace_all(nutrient_name,"Energy_total_combined","Energy_kcal"),
    nutrient_name=str_replace_all(nutrient_name,"Fibre, total; gravimetrically determined","Fibre"),
    nutrient_name=str_replace_all(nutrient_name,"Folate, total","Folate"),
    nutrient_name=str_replace_all(nutrient_name,"Iron, total","Iron"),
    nutrient_name=str_replace_all(nutrient_name,"Vitamin B12","Vit. B12"),
    
    nutrient_name=str_replace_all(nutrient_name,"Protein, total; calculated from total nitrogen","Protein"),
    nutrient_name=str_replace_all(nutrient_name,"Vitamin A\\; sum of retinol/carotenoids, expressed as retinol activity equivalent \\(RAE\\)","Vit. A RAE")
  )
  

write_csv(afcd_all,file=here("outputs","data","afcd_extract.csv"))



afcd_all %>%
  group_by(taxa_match,nutrient) %>%
  count() %>%
  pivot_wider(names_from=taxa_match,values_from = n) %>%
  rename('no_match'='NA') %>%
  select(nutrient,species,genus,family,order,class,no_match) %>%
  mutate(
    nutrient=str_replace_all(nutrient,"Fibre, total; gravimetrically determined","Fibre"),
    nutrient=str_replace_all(nutrient,"Folate, total","Folate"),
    nutrient=str_replace_all(nutrient,"Iron, total","Iron"),
    nutrient=str_replace_all(nutrient,"Protein, total; calculated from total nitrogen","Protein"),
    nutrient=str_replace_all(nutrient,"Vitamin A\\; sum of retinol/carotenoids, expressed as retinol activity equivalent \\(RAE\\)","Vit. A RAE")
  ) %>%
  mutate(
    total=species+genus+family+class+order+no_match,
    species=species/total,
    genus=genus/total,
    family=family/total,
    class=class/total,
    order=order/total,
    no_match=no_match/total
  ) %>%
  select(-total) %>%
  pivot_longer(species:no_match,names_to = 'taxa_match',values_to = 'perc') %>%
  mutate(
    taxa_match=ifelse(taxa_match=="no_match",NA,taxa_match),
    taxa_match=factor(taxa_match,levels = c('species','genus','family','order','class',NA))
  ) %>%
  ggplot(aes(fill=taxa_match,x=perc,y=nutrient)) +
  geom_bar(position="fill", stat="identity") +
  scale_fill_viridis(
    option="turbo",discrete=TRUE,name="Taxonomic\nmatch",na.value="gray50"
  ) +
  scale_x_continuous(labels = scales::percent)+
  ylab("") + xlab("") +
  theme_bw() +
  ggtitle("Nutrients matched by taxonomic level in AFCD")
