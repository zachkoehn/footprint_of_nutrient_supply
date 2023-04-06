library(here)
source(here("scripts","read_clean_data.R"))
library(viridis)
library(ggrepel)

nutrient_richness <- nutrients %>%
  select(paper_category,nutrient_density) %>%
  distinct()

fig_2_nutrient_proportion_data <- proportion_data %>%
  group_by(paper_category) %>%
  summarize(global_proportion=sum(global_proportion)) %>%
  left_join(food_group_categories,by="paper_category") %>%
  left_join(nutrient_richness,by="paper_category") %>%
  select(food_system,food_sector,paper_category,global_proportion,nutrient_density) %>%
  mutate(
    pressure_proportion_median=median(global_proportion,na.rm=TRUE),
    nutrient_density_median=median(nutrient_density,na.rm=TRUE)
  ) 

fig_2_nutrient_proportion <- fig_2_nutrient_proportion_data %>%
  ggplot(
    aes(x=nutrient_density,y=global_proportion)
  ) + 
  geom_hline(aes(yintercept=pressure_proportion_median),color="grey40") +
  geom_vline(aes(xintercept=nutrient_density_median),color="grey40") +
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
  scale_y_continuous(labels = scales::percent) +
  ylab("% of global cumulative pressure") + 
  xlab("% of RDI") +
  # ylim(c(-0.001,0.013)) +
  theme_bw() +
  theme(
    legend.position = "top",
    strip.background = element_rect(fill="grey90"),
    legend.title = element_blank()
  ) 

ggsave(
  fig_2_nutrient_proportion,
  filename = here("outputs","plots","fig_a2_nutrient_proportion.pdf"),
  units = "in",
  height=7,width=8.5
)