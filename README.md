# footprint_of_nutrient_supply
Script and data used here reproduce the results reported in "Nutrition should count when assessing the footprint of aquatic foods." This project couples nutrition data with the cumulative pressures dataset developed by Halpern et al (2022) to determine the footprint of nutrient production and where aquatic foods may play a role. 

## Description of scripts 

*usda_fdc_api.R*
Runs API to download USDA nutrient data for livestock and crops. Note, any user will need to create their own API key and add to the script in order to run this file. 
Input - USDA API key
Output - "usda_api_extract.csv"

*read_clean_data.R* - 
This script reads and cleans, ensuring all food group categories follow the same naming structure. 
Input - supplementary data file from Halpern et al (2022)
Output - a series of object files pulled in by subsequent scripts.

*calculate cumulative pressure and nutrient production.R* _

*figure_1_regional_nutrient_contributions.R* -

*figure_2_nutrient_pressure_plot.R* - 

*figure_a2_global_proportion.R* - 

## Data
Data on cumulative pressures was downloaded from supplementary information in Halpern et al. (2022). Nutrition data was sourced from USDA (crops and livestock) and from the Aquatic Food Composition Databse (mariculture and fisheries). Data will be added to this folder after submitting the article in preparation. 