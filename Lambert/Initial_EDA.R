library(tidyverse)
county_data <- read_csv("/Users/jessicalambert/Desktop/CMU Health/CMU Capstone Project/Data/analytic_data2024.csv") |>
  slice(-1) |> 
  clean_names()
colnames(county_data)
library(janitor)
county_data%>%clean_names 

View(county_data)

cou

numeric_county_data <- county_data |> 
  mutate(food_environment_index_raw_value = as.numeric(food_environment_index_raw_value), 
         child_mortality_raw_value = as.numeric(child_mortality_raw_value))
  
county_data_subset <- county_data |> 
  select(food_environment_index_raw_value, child_mortality_raw_value, high_school_graduation_raw_value,
         excessive_drinking_raw_value, physical_inactivity_raw_value, state_abbreviation, name, 
         drug_overdose_deaths_raw_value, teen_births_raw_value) |> 
  mutate(across(ends_with('raw_value'), as.numeric))
  
#child mortality and food environment (strongest so far)
county_data_subset |> 
  ggplot(aes(x = food_environment_index_raw_value, y = child_mortality_raw_value)) +
  geom_point() +
  stat_smooth(method = lm)


library(broom)


mortality_food_lm <-lm(child_mortality_raw_value ~ food_environment_index_raw_value,
                       data = numeric_county_data)
summary(mortality_food_lm)
plot(mortality_food_lm)
fitted(mortality_food_lm)


library(ggfortify)
mortality_food_lm |> 
  autoplot() +
  theme_light()


#high school graduation and drug overdose deaths raw value
county_data_subset |> 
  ggplot(aes(x = high_school_graduation_raw_value, y = drug_overdose_deaths_raw_value)) +
  geom_point()

graduation_overdose_lm <- lm(high_school_graduation_raw_value ~ drug_overdose_deaths_raw_value, data = county_data_subset)
plot(graduation_overdose_lm)


#teen births and graduation
county_data_subset |> 
  ggplot(aes(x = teen_births_raw_value, y = high_school_graduation_raw_value)) +
  geom_point()

teenbirths_graduation_lm <- lm(teen_births_raw_value ~ high_school_graduation_raw_value, data = county_data_subset)
plot(teenbirths_graduation_lm) #not great

#high school graduation and food environment
county_data_subset |> 
  ggplot(aes(x = food_environment_index_raw_value, y = high_school_graduation_raw_value)) +
  geom_point()

graduation_food_lm <- lm(high_school_graduation_raw_value ~ food_environment_index_raw_value, data = county_data_subset)
plot(graduation_food_lm)

#teen births and child mortality (strong here)
teen_mortality <- county_data_subset |> 
  ggplot(aes(x = teen_births_raw_value, y = child_mortality_raw_value)) +
  geom_point() +
  stat_smooth(method = lm)

teenbirths_mortality_lm <- lm(child_mortality_raw_value ~ teen_births_raw_value, data = county_data_subset)
plot(teenbirths_mortality_lm)
summary(teenbirths_mortality_lm)

#graduation and excessive drinking (passes tests the best)
county_data_subset |> 
  ggplot(aes(x = excessive_drinking_raw_value, y = high_school_graduation_raw_value)) +
  geom_point() +
  stat_smooth(method = lm)

graduation_drinking_lm <- lm(high_school_graduation_raw_value ~ excessive_drinking_raw_value, data = county_data_subset)
plot(graduation_drinking_lm)

#physical inactivity and child mortality
county_data_subset |> 
  ggplot(aes(x = physical_inactivity_raw_value, y = child_mortality_raw_value)) + 
  geom_point() + 
  geom_smooth(method = "lm", linewidth = 2)

?cor

inactivity_mortality_lm <- lm(physical_inactivity_raw_value ~ child_mortality_raw_value, data = county_data_subset)
plot(inactivity_mortality_lm)



library(glmnet)
countydata_reg <- county_data_subset |> 
  select(physical_inactivity_raw_value, food_environment_index_raw_value, teen_births_raw_value)

model_x <- model.matrix(physical_inactivity_raw_value ~ food_environment_index_raw_value ~ teen_births_raw_value, 
                        county_data_subset)

model_x <- county_data_subset |> 
  select(physical_inactivity_raw_value, food_environment_index_raw_value, teen_births_raw_value) |> 
  as.matrix()
#--------------------------------------------------------------------------
#START HERE: 


county_data_subset <- county_data |> 
  filter()
  select(food_environment_index_raw_value, child_mortality_raw_value, high_school_graduation_raw_value,
         excessive_drinking_raw_value, physical_inactivity_raw_value, state_abbreviation, name, 
         drug_overdose_deaths_raw_value, teen_births_raw_value) |> 
  mutate(across(ends_with('raw_value'), as.numeric)) 


#teen births and child mortality scatterplot
teen_mortality <- county_data_subset |> 
  filter(!is.na(teen_births_raw_value)) |> 
  filter(!is.na(child_mortality_raw_value)) |> 
  ggplot(aes(x = teen_births_raw_value, y = child_mortality_raw_value)) +
  geom_point() +
  stat_smooth(method = lm)

county_data_subset |> 
  count(child_mortality_raw_value) |> 
  ggplot(aes(x = child_mortality_raw_value, y = n)) +
  geom_col()

cor(county_data_subset$child_mortality_raw_value,
    county_data_subset$teen_births_raw_value,
    use = "complete.obs")

#food environment and mortality rate scatterplot
county_data_subset |> 
  filter(!is.na(food_environment_index_raw_value)) |> 
  filter(!is.na(child_mortality_raw_value)) |> 
  ggplot(aes(x = food_environment_index_raw_value, y = child_mortality_raw_value)) +
  geom_point() +
  stat_smooth(method = lm)


cor(county_data_subset$child_mortality_raw_value,
    county_data_subset$food_environment_index_raw_value,
    use = "complete.obs")

#physical inactivity and mortality rate scatterplot
county_data_subset |> 
  filter(!is.na(physical_inactivity_raw_value)) |> 
  filter(!is.na(child_mortality_raw_value)) |> 
  ggplot(aes(x = physical_inactivity_raw_value, y = child_mortality_raw_value)) +
  geom_point() +
  stat_smooth(method = lm)


#food environment index: logistic regression

foodlog <- county_data |> 
  mutate(food_environment_index_raw_value = as.numeric(food_environment_index_raw_value)) |> 
  filter(!is.na(food_environment_index_raw_value)) |> 
  filter(!is.na(state_abbreviation))

mean(foodlog$food_environment_index_raw_value) #7.54

foodlog <- foodlog |> 
  mutate(food_environment_index_raw_value > 7.54, "Yes", "No")

food_logit <- glm(food_environment_index_raw_value ~ county, 
                  data = foodlog, family = binomial)
 


county_data_subset


  


