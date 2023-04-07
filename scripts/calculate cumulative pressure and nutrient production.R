library(here)
source(here("scripts","read_clean_data.R"))
library(viridis)
library(ggrepel)


#######################################################
# Calculations related to global nutrient production
#______________________________________________________
production_nutrients <- nutrients %>%
  drop_na(nutrient_value) %>%
  left_join(food_group_categories,by="paper_category") %>%
  group_by(paper_category,food_sector,food_system,nutrient_name) %>%
  summarize(median_nutrient_value_per_tonne=median(nutrient_value_per_tonne)) %>%
  full_join(country_pressure_data,by="paper_category") %>%
  distinct() %>%
  group_by(paper_category,food_system,food_sector,nutrient_name) %>%
  mutate(
    total_tonnes = sum(tonnes,na.rm=TRUE),
    nutrient_production=median_nutrient_value_per_tonne*total_tonnes
  ) %>%   
  select(paper_category,food_system,food_sector,nutrient_name,median_nutrient_value_per_tonne,nutrient_production) %>%
  distinct()


production_nutrients %>%
  filter(nutrient_production>0) %>%
  ggplot(aes(y=paper_category,x=nutrient_production,fill=food_sector)) +
  geom_bar(stat="identity") +
  facet_wrap(~nutrient_name,scales = "free") 


production_nutrients %>%
  group_by(nutrient_name) %>%
  mutate(
    simplified_bf_paper_category=ifelse(food_system=="aquatic","blue foods",paper_category)
  ) %>%
  mutate(
    total_nutrient_production=sum(nutrient_production),
    proportional_nutrient_production=nutrient_production/total_nutrient_production
  ) %>%
  ungroup() %>%
  distinct() %>%
  # View()
  drop_na(proportional_nutrient_production) %>%
  group_by(simplified_bf_paper_category,nutrient_name) %>%
  summarize(proportional_nutrient_production=sum(proportional_nutrient_production)) %>%
  rename(paper_category=simplified_bf_paper_category) %>%
  left_join(food_group_categories,by="paper_category") %>%
  # View()
  mutate(
    food_sector=ifelse(is.na(food_sector),"fisheries & aquaculture",food_sector),
    food_system=ifelse(is.na(food_system),"aquatic",food_system)
  ) %>%
  # View()
  filter(proportional_nutrient_production>0.005) %>%
  ggplot(aes(y=paper_category,x=proportional_nutrient_production,fill=food_sector)) + 
    geom_bar(stat="identity") +
    geom_vline(xintercept=0.1) +
  scale_fill_manual(
    values = c(
      "fisheries & aquaculture"="#85A0A9",
      "crops"="#a7982f",
      "livestock"="#B3832F"
    )
  ) +
  xlab("proportion of total nutrient production met by each sector") +
  ylab("") +
  scale_x_continuous(labels = scales::percent) +
  theme_bw() +
  theme(
    legend.position = "top",
    axis.text.y = element_text(size=8),
    legend.title = element_blank()
        ) +
  facet_wrap(~nutrient_name,scales = "free") 

# selecting nutrients 
select_bf_nutrient_props <- production_nutrients %>%
  group_by(nutrient_name) %>%
  mutate(
    total_nutrient_production=sum(nutrient_production),
    proportional_nutrient_production=nutrient_production/total_nutrient_production
  ) %>%
  ungroup() %>%
  distinct() %>%
  # View()
  filter(
    paper_category %in% c("salmon","bivalve","crustaceans","small pelagic","forage fish","freshwater fish")
    # nutrient_name %in% c("DHA_EPA","Selenium","Vit. B12")
    ) %>%
  group_by(nutrient_name) %>%
  summarize(sum_contribution=sum(proportional_nutrient_production))
  
  
  


#######################################################
# Calculations related to proportion of global cumulative pressure
#______________________________________________________
perc_total_pressure <- country_pressure_data %>%
  group_by(paper_category) %>%
  summarize(
    pressure_cumulative=sum(pressure_cumulative)
  ) %>%
  # left_join(production,by="paper_category") %>%
  mutate(
    total_pressure=sum(pressure_cumulative,na.rm=TRUE),
    percent_total_pressure=pressure_cumulative/total_pressure
  ) %>%
  # drop_na(percent_total_pressure) %>%
  left_join(food_group_categories,by="paper_category")
perc_total_pressure %>%
     group_by(food_system) %>%
     summarize(percent_total_pressure=sum(percent_total_pressure))


perc_total_pressure %>%
  group_by(food_system) %>%
  summarize(percent_total_pressure=sum(percent_total_pressure))

perc_total_pressure %>%
  filter(paper_category %in% c("shrimp","demersal")) %>%
  select(paper_category,percent_total_pressure)

perc_total_pressure %>%
  filter(
    paper_category %in% c("salmon","bivalve","crustaceans","small pelagic","forage fish","freshwater fish")
    ) %>%
  select(paper_category,percent_total_pressure)



perc_total_pressure %>%
  ggplot(aes(x=percent_total_pressure,y=fct_reorder(paper_category,percent_total_pressure))) +
  scale_x_continuous(labels = scales::percent) +
  ylab("") + xlab("% total pressure") +
  geom_bar(stat="identity") +
  facet_wrap(~food_sector,scales = "free")


#######################################################
# Calculations related to global nutrient production
#______________________________________________________
# proportion of aquaculture production that is prawns (by tonnage)
country_pressure_data %>%
  left_join(food_group_categories) %>%
  filter(food_sector=="mariculture") %>%
  group_by(paper_category,food_sector) %>%
  summarize(
    food_group_total=sum(tonnes)
  ) %>%
  ungroup() %>%
  mutate(
    total_weight=sum(food_group_total),
    perc_total=food_group_total/total_weight
  ) 
# proportion of capture production that is demersals (by tonnage)
country_pressure_data %>%
  left_join(food_group_categories) %>%
  filter(food_sector=="marine and freshwater fisheries") %>%
  group_by(paper_category,food_sector) %>%
  summarize(
    food_group_total=sum(tonnes)
  ) %>%
  ungroup() %>%
  mutate(
    total_weight=sum(food_group_total),
    perc_total=food_group_total/total_weight
  ) 

# proportion of all aquatic food production that is 
# demersal and shrimp
country_pressure_data %>%
  left_join(food_group_categories) %>%
  filter(food_system=="aquatic") %>%
  group_by(paper_category,food_sector) %>%
  summarize(
    food_group_total=sum(tonnes)
  ) %>%
  ungroup() %>%
  mutate(
    total_weight=sum(food_group_total),
    perc_total=food_group_total/total_weight
  ) 
