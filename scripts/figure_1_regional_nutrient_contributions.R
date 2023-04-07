library(here)
source(here("scripts","read_clean_data.R"))
library(viridis)
library(ggrepel)
library(countrycode)



global_production_nutrients <- nutrients %>%
  drop_na(nutrient_value) %>%
  left_join(food_group_categories,by="paper_category") %>%
  group_by(paper_category,food_sector,food_system,nutrient_name) %>%
  summarize(median_nutrient_value_per_tonne=median(nutrient_value_per_tonne)) %>%
  left_join(country_pressure_data,by="paper_category") %>%
  group_by(paper_category,food_system,food_sector,nutrient_name) %>%
  mutate(
    total_tonnes = sum(tonnes,na.rm=TRUE),
    nutrient_production=median_nutrient_value_per_tonne*total_tonnes
  ) %>%   
  select(paper_category,food_system,food_sector,nutrient_name,median_nutrient_value_per_tonne,nutrient_production) %>%
  distinct()

# global_production_nutrients %>%
#   drop_na(nutrient_value_per_tonne) %>%
#   distinct() %>%
#   group_by(paper_category,nutrient_name) %>%
#   mutate(
#     median_nutrient_value_per_tonne=median(nutrient_value_per_tonne),
#     mean_nutrient_value_per_tonne=mean(nutrient_value_per_tonne),
#     l_20_nutrient_value_per_tonne=quantile(nutrient_value_per_tonne,probs = 0.2),
#     u_80_nutrient_value_per_tonne=quantile(nutrient_value_per_tonne,probs = 0.8),
#     ) %>%
#   select(
#     paper_category,food_sector,nutrient_name,
#     median_nutrient_value_per_tonne,mean_nutrient_value_per_tonne,
#     l_20_nutrient_value_per_tonne,u_80_nutrient_value_per_tonne) %>%
#   distinct() %>%
#   # filter(nutrient_name %in% c("Fibre")) %>%
#   # View()
#   ggplot(aes(color=food_sector)) +
#   geom_point(aes(y=paper_category,x=median_nutrient_value_per_tonne),shape=17,size=2) +
#   geom_point(aes(y=paper_category,x=mean_nutrient_value_per_tonne),shape=12,size=2) +
#   # geom_segment(aes(y=paper_category,yend=paper_category,x=median_nutrient_value_per_tonne,xend=u_80_nutrient_value_per_tonne))  +
#   # geom_segment(aes(y=paper_category,yend=paper_category,x=l_20_nutrient_value_per_tonne,xend=median_nutrient_value_per_tonne))  +
#   facet_wrap(~nutrient_name,scales="free_x")
  
broad_sector_nutrient_production <- global_production_nutrients %>%
  group_by(nutrient_name) %>%
  mutate(
    total_nutrient_production=sum(nutrient_production),
    proportional_nutrient_production=nutrient_production/total_nutrient_production
  ) %>%
  drop_na(proportional_nutrient_production) %>%
  group_by(food_system,nutrient_name) %>%
  summarize(summed_proportion=sum(proportional_nutrient_production)) %>%
  filter(!nutrient_name %in% c("Fibre","Folate")) %>%
  # View()
  ggplot(aes(y=food_system,x=summed_proportion)) + 
  geom_vline(xintercept=0.1,color="black") +
  xlab("proportion of total nutrient production met by each sector") +
  ylab("") +
  geom_bar(stat="identity") +
  # scale_fill_manual(
  #   values = c(
  #     "aquatic"="steelblue2",  
  #     "terrestrial"="springgreen2"
  #     ),
  #   name=""
  # ) +
  scale_x_continuous(labels = scales::percent) +
  theme_bw() +
  theme(
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(size=10),
    axis.text.x=element_text(size=7),
    strip.background = element_rect(fill="grey90"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank()
    ) +
  facet_wrap(
    ~nutrient_name,
    nrow=4
    ) +
  ggtitle("a)")

broad_sector_nutrient_production
  ggsave(
    broad_sector_nutrient_production,
    filename=here("outputs","plots","fig_1_a_broad_sector_nutrient_production.pdf"),
    units="in",width=6,height=5
    )

region_production_nutrients <- nutrients %>%
  drop_na(nutrient_value) %>%
  group_by(paper_category,nutrient_name) %>%
  summarize(
    nutrient_value_per_tonne=median(nutrient_value_per_tonne)
  ) %>%
  full_join(country_pressure_data,by="paper_category") %>%
  select(
    paper_category,country,isoc3,nutrient_name,nutrient_value_per_tonne,tonnes
  ) %>%
  distinct() %>%
  mutate(
    region_specific=countrycode(isoc3,"iso3c","un.regionsub.name"),
    region_broad=countrycode(isoc3,"iso3c","un.region.name"),
    region_broad=ifelse(country=="Taiwan","Asia",region_broad),
    region_specific=ifelse(country=="Taiwan","Eastern Asia",region_specific),
    region_broad=ifelse(country=="Kosovo","Europe",region_broad),
    region_specific=ifelse(country=="Kosovo","Southern Europe",region_specific),
    region_broad=ifelse(country=="High Seas","High Seas",region_broad),
    region_specific=ifelse(country=="High Seas","High Seas",region_specific),
    region_broad=ifelse(country=="Madeira Island","Europe",region_broad),
    region_specific=ifelse(country=="Madeira Island","Southern Europe",region_specific),
    food_group_nutrient_production=nutrient_value_per_tonne*tonnes
    ) %>%
  select(paper_category,region_broad,region_specific,nutrient_name,food_group_nutrient_production) %>%
  distinct() %>%
  group_by(paper_category,region_broad,region_specific,nutrient_name) %>%
  summarize(
    food_group_sum_nutrient_production=sum(food_group_nutrient_production)
  ) %>%
  group_by(region_broad,region_specific,nutrient_name) %>%
  mutate(
    total_nutrient_production=sum(food_group_sum_nutrient_production)
  ) %>%
  ungroup() %>%
  mutate(
    perc_nutrient_production = food_group_sum_nutrient_production/total_nutrient_production,
    region_specific=str_replace_all(region_specific,"Australia","Aus."),
    region_specific=str_replace_all(region_specific,"New Zealand","N.Z."),
    region_specific=str_replace_all(region_specific,"and","&"),
    region_specific=str_replace_all(region_specific,"Eastern","E."),
    region_specific=str_replace_all(region_specific,"South-eastern","S.E."),
    region_specific=str_replace_all(region_specific,"Southern","S."),
    region_specific=str_replace_all(region_specific,"Western","W."),
    region_specific=str_replace_all(region_specific,"Northern","N."),
    region_specific=str_replace_all(region_specific,"Central","C."),
    region_specific=str_replace_all(region_specific,"the Caribbean","Carib.")
  ) %>%
  left_join(food_group_categories,by="paper_category") 

nutrients %>%
  left_join(food_group_categories,by="paper_category") %>%
  select(food_sector,product) %>%
  distinct() %>%
  group_by(food_sector) %>%
  tally()

region_order <- region_production_nutrients %>%
  select(region_broad,region_specific) %>%
  drop_na() %>%
  distinct() %>%
  arrange(region_broad)

region_select_nutrients_plot <- region_production_nutrients %>%
  filter(
    nutrient_name %in% c("Vit. A RAE","Vit. B12","Selenium"),
    region_broad!="High Seas",
    food_system=="aquatic"
    ) %>%
  select(food_system,region_specific,region_broad,nutrient_name,perc_nutrient_production) %>%
  group_by(food_system,region_specific,region_broad,nutrient_name) %>%
  summarize(perc_nutrient_production=sum(perc_nutrient_production)) %>%
  # View()
  drop_na(nutrient_name) %>%
  ggplot(
    aes(
      x=perc_nutrient_production,
      y=fct_relevel(region_specific,region_order$region_specific),
      fill=region_broad
      )
    ) +
  geom_vline(xintercept=0.1,color="black") +
  xlab("proportion of total nutrient production in each region by the aquatic food sector") +
  ylab("") +
  geom_bar(stat="identity") +
  scale_x_continuous(labels = scales::percent) +
  scale_fill_viridis(option="turbo",discrete = TRUE,name="") +
  theme_bw() +
  theme(
    axis.title.x = element_text(size=9),
    axis.text.x=element_text(size=8),
    axis.text.y=element_text(size=8),
    strip.background = element_rect(fill="grey90"),
    legend.position = "none"
  ) +
  facet_wrap(
    ncol=3,
    ~nutrient_name)

region_select_nutrients_plot

ggsave(
  region_select_nutrients_plot,
  filename=here("outputs","plots","fig_1_geographic_nutrient_production.pdf"),
  units="in",width=8.5,height=4
)

region_all_nutrients_plot <- region_production_nutrients %>%
  filter(
    nutrient_name !="Energy_kcal",
    # nutrient_name %in% c("Vit. B12","Selenium", "DHA_EPA"),
    region_broad!="High Seas",
    food_system=="aquatic"
  ) %>%
  # filter(
  #   region_specific=="Micronesia"
  # ) %>%
  select(food_system,region_specific,region_broad,nutrient_name,perc_nutrient_production) %>%
  group_by(food_system,region_specific,region_broad,nutrient_name) %>%
  summarize(perc_nutrient_production=sum(perc_nutrient_production)) %>%
  drop_na(nutrient_name) %>%
  # View()
  mutate(
    nutrient_name=ifelse(nutrient_name=="DHA_EPA","DHA + EPA",nutrient_name),
    nutrient_name=factor(nutrient_name,levels=c(
      "Folate","Riboflavin","Thiamin","Vit. A RAE","Vit. B12", # Vitamins
      "Calcium","Iron","Magnesium","Potassium","Selenium","Zinc",# Minerals
      "Fibre","DHA + EPA","Protein"
    ))
  ) %>%
  ggplot(
    aes(
      x=perc_nutrient_production,
      y=fct_relevel(region_specific,region_order$region_specific),
      fill=region_broad
    )
  ) +
  geom_vline(xintercept=0.1,color="black") +
  xlab("proportion of total nutrient production in each region by the aquatic food sector") +
  ylab("") +
  geom_bar(stat="identity") +
  scale_x_continuous(labels = scales::percent) +
  scale_fill_viridis(option="turbo",discrete = TRUE,name="") +
  theme_bw() +
  theme(
    axis.title.x = element_text(size=9),
    axis.text.x=element_text(size=8),
    axis.text.y=element_text(size=8),
    strip.background = element_rect(fill="grey90"),
    strip.text = element_text(size=10),
    legend.position = "none"
  ) +
  facet_wrap(
    ncol=3,
    ~nutrient_name) 
region_all_nutrients_plot

ggsave(
  region_all_nutrients_plot,
  filename=here("outputs","plots","fig_a1_all_geographic_nutrient_production.pdf"),
  units="in",width=8.5,height=11
)

