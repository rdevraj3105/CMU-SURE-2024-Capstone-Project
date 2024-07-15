#filtered data
library(tidyverse)
library(janitor)
county_data <- read_csv("/Users/jessicalambert/Desktop/CMU Health/CMU Capstone Project/Data/analytic_data2024.csv") |>
  slice(-1) |> 
  clean_names()

county_data <- county_data |> 
  filter(!is.na(county_clustered_yes_1_no_0)) 

county_data%>%clean_names
view(county_data)




county_data_subset <- county_data |> 
  select(food_environment_index_raw_value, child_mortality_raw_value, high_school_graduation_raw_value,
         excessive_drinking_raw_value, physical_inactivity_raw_value, state_abbreviation, name, 
         drug_overdose_deaths_raw_value, teen_births_raw_value, low_birthweight_raw_value) |> 
  mutate(across(ends_with('raw_value'), as.numeric))

county_data_subset |> 
  filter(!is.na(child_mortality_raw_value))
county_data_subset$low_birthweight_raw_value

#food environment index: logistic regression

mean(foodlog$food_environment_index_raw_value) #7.54

county_data_subset <- county_data_subset |> 
  mutate(food_index = ifelse(food_environment_index_raw_value > 7.54, "Yes", "No")) |> 
  mutate(low_birthweight_raw_value = )

food_logit <- glm(child_mortality_raw_value ~ food_index, data = county_data, family = binomial)






