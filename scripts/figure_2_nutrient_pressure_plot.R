library(here)
source(here("scripts","read_clean_data.R"))
library(viridis)
library(ggrepel)


fig_2_nutrient_calorie_panel_data <- country_pressure_data %>%
  left_join(food_group_categories,by="paper_category") %>%
  left_join(nutrients,by="paper_category") %>%
  filter(nutrient_name=="Energy_kcal") %>%
  group_by(paper_category) %>%
  mutate(
    pressure_per_tonne_median=median(pressure_per_tonne,na.rm=TRUE),
    nutrient_density_median=median(nutrient_density,na.rm=TRUE),
    kcal_perc_median=median(percent_rdi,na.rm=TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    pressure_per_tonne_median_all=median(pressure_per_tonne,na.rm=TRUE)
  ) %>%
  select(
    paper_category,food_sector,
    pressure_per_tonne_median,nutrient_density_median,kcal_perc_median,
    pressure_per_tonne_median_all
    ) %>%
  distinct() %>%
  pivot_longer(
    nutrient_density_median:kcal_perc_median,names_to = "metric",values_to="value"
    ) %>%
  distinct() %>%
  mutate(
    metric=str_replace_all(metric,"nutrient_density_median","Nutrient richness"),
    metric=str_replace_all(metric,"kcal_perc_median","Calories"),
    pressure_per_tonne_median_all=median(pressure_per_tonne_median)
  ) %>%
  group_by(metric) %>%
  mutate(
    value_median=median(value)
  ) %>%
  ungroup()

fig_2_nutrient_calorie_panel <- fig_2_nutrient_calorie_panel_data %>%
  mutate(
    metric=ifelse(metric=="Calories","a)",metric),
    metric=ifelse(metric=="Nutrient richness","b)",metric)
  ) %>%
  ggplot(
    aes(x=value,y=pressure_per_tonne_median)
    ) + 
  geom_hline(aes(yintercept=pressure_per_tonne_median_all),color="grey40") +
  geom_vline(aes(xintercept=value_median),color="grey40") +
  geom_point(aes(color=food_sector),size=3,alpha=0.85) +
  geom_label_repel(
    aes(label=paper_category,fill=food_sector,segment.color=food_sector),
    color="white",show.legend =FALSE,max.overlaps=20,size=3,label.padding=0.15
  ) +
  scale_color_manual(
    values = c(
      "mariculture"="#6667AB",
      "marine and freshwater fisheries"="#70aead",
      "crops"="#a7982f",
      "livestock"="#B3832F"
    ),
    aesthetics = c("segment.color","color")
  ) +
  scale_fill_manual(
    values = c(
      "mariculture"="#6667AB",
      "marine and freshwater fisheries"="#70aead",
      "crops"="#a7982f",
      "livestock"="#B3832F"
    )
  ) +
  scale_x_continuous(labels = scales::percent) +
  ylab("Pressure per tonne") + 
  xlab("% of RDI") +
  ylim(c(-0.001,0.013)) +
  theme_bw() +
  theme(
    legend.position = "top",
    strip.background = element_rect(fill="white",color="white"),
    strip.text = element_text(size=10,hjust=0),
    legend.title = element_blank()
  ) +
  facet_wrap(~metric,nrow=2)


ggsave(
  fig_2_nutrient_calorie_panel,
  filename = here("outputs","plots","fig_2_nutrient_calorie_panel.pdf"),
  units = "in",
  height=11,width=8.5
)
