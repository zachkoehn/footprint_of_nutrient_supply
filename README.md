# footprint_of_nutrient_supply
Script and data used here reproduce the results reported in "Nutrition should count when assessing the footprint of aquatic foods." This project couples nutrition data with the cumulative pressures dataset developed by Halpern et al (2022) to determine the footprint of nutrient production and where aquatic foods may play a role. 

## Description of scripts 

**extract_terrestrial_nutrients_usda.R** -
Runs API to download USDA nutrient data for livestock and crops. Note, any user will need to create their own API key and add to the script in order to run this file. 

**extract_aquatic_nutrients_afcd.R** - 
Downloads and cleans aquatic food nutrient data from the Aquatic Food Composition Database (AFCD). This file also looks at overlaps between the FAO food groups used in Halpern et al. and AFCD. 

**read_clean_data.R** - 
This script reads and cleans, ensuring all food group categories follow the same naming structure. Outputs from this file are used by subsequent scripts to create findings in text as well as figures from the main content and supplementary information. 

**calculate cumulative pressure and nutrient production.R** -
Connects nutrient data to production data (from Halpern et al) to determine the proportion of nutrients produced in each food group sector. Also calculates global cumulative pressures associated with specific food group sectors.

**figure_1_regional_nutrient_contributions.R** - Figure 1 in main text. Determines contribution of food groups to nutrient supply by region.  

**figure_2_nutrient_pressure_plot.R** - Figure 2 in main text. Creates scatter plot relating food group cumulative pressure per ton of food produced relative to caloric richness and nutrient density. Non-feed only. 

**figure_a2_global_proportion.R** - Figure A2 in supplementary information. Creates scatter plot relating global proportion of cumulative pressure for each food group relative to nutrient density. Non-feed only. 

## Data
Data on cumulative pressures was downloaded from supplementary information in Halpern et al. (2022). Nutrition data was sourced from USDA (crops and livestock) and from the Aquatic Food Composition Databse (mariculture and fisheries). Data will be added to this folder after submitting the manuscript as a journal article (in preparation). 