library(tidyverse)
library(janitor)
county_data <- read_csv("/Users/jessicalambert/Desktop/CMU Health/CMU Capstone Project/Data/analytic_data2024.csv") |>
  slice(-1) |> 
  clean_names()
colnames(county_data)

county_data%>%clean_names 


county_data_subset <- county_data |> 
  select(child_mortality_raw_value, low_birthweight_raw_value, excessive_drinking_raw_value, 
         physical_inactivity_raw_value, state_abbreviation, name, 
         drug_overdose_deaths_raw_value, adult_smoking_raw_value, adult_obesity_raw_value, 
         insufficient_sleep_raw_value, ) |> 
  mutate(across(ends_with('raw_value'), as.numeric))

#pairs plot
county_data_subset2 <- na.omit(county_data_subset)

library(GGally)
countypairs <- county_data_subset2 |> 
  select(child_mortality_raw_value, low_birthweight_raw_value, excessive_drinking_raw_value, adult_smoking_raw_value, adult_obesity_raw_value) |> 
  ggpairs()

?ggpairs

#scatterplot child mortality and low birthweight
county_data_subset2 |> 
  ggplot(aes(x = child_mortality_raw_value, y = low_birthweight_raw_value)) + 
  geom_point()
#violin plot
install.packages("ggbeeswarm")
library(ggbeeswarm)
county_data_subset2 |> 
  ggplot(aes(x = adult_smoking_raw_value, y = child_mortality_raw_value)) + 
  geom_violin()

#heatmap
library(reshape2)
library(ggplot2)

colnames(county_data)

view(county_data)


cor(county_data_subset2$low_birthweight_raw_value, county_data_subset2$insufficient_sleep_raw_value)
cor(county_data_subset2$child_mortality_raw_value, county_data_subset2$adult_obesity_raw_value)
cor(county_data_subset2$child_mortality_raw_value, county_data_subset2$adult_smoking_raw_value)
