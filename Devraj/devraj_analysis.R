
library(tidyverse)
healthdata <- read_csv("Data/analytic_data2024.csv") |> 
  slice(-1) |> 
  clean_names()
View(healthdata)
tibble(healthdata)
#which adult-health related factors impact low birth weight 

# healthdata |> 
#   slice(-1) |> 
#   select(prematuredeath_num = "Premature Death numerator", prematuredeath_denom = "Premature Death denominator") |> 
#   mutate(prematuredeath_num = as.numeric(prematuredeath_num),
#          prematuredeath_denom = as.numeric(prematuredeath_denom),
#     prop_premature =  prematuredeath_num/prematuredeath_denom)



#Goal...How do different adult health-related practices predict low birth weight?->use lasso regression?

str(healthdata)

healthdata_subset <- healthdata |> 
  select(state_abbreviation, name, release_year, adult_smoking_raw_value, adult_obesity_raw_value, 
         food_insecurity_raw_value, excessive_drinking_raw_value,
         physical_inactivity_raw_value,insufficient_sleep_raw_value, low_birthweight_raw_value, 
         percent_female_raw_value, low_birthweight_asian_pacific_islander, low_birthweight_black, 
         low_birthweight_asian_pacific_islander, low_birthweight_white,low_birthweight_hispanic, 
         sexually_transmitted_infections_raw_value, uninsured_raw_value
         ) |> 
  mutate(across(ends_with('raw_value'), as.numeric)) |> 
  na.omit()


#healthdata_subset <- healthdata_subset[complete.cases(healthdata_subset),]


#Obesity and low birthweight
obesity <- healthdata_subset |> 
  ggplot(aes(x=adult_obesity_raw_value, y=low_birthweight_raw_value))+
  geom_point(alpha =0.3)+
  geom_smooth(method = lm)

#Smoking and low birthweight
smoking <- healthdata_subset |> 
  ggplot(aes(x= adult_smoking_raw_value, y=low_birthweight_raw_value))+
  geom_point(alpha =0.3)+
  geom_smooth(method = lm)
  #geom_violin(fill = "green")

#does not consider age...so I can't use this right?
food <- healthdata_subset |> 
  ggplot(aes(x= food_insecurity_raw_value, y=low_birthweight_raw_value))+
  geom_point(alpha =0.3)+
  geom_smooth(method = lm)

#Excessive drinking and low birthweight
#break down by gender
#alcohol frequency in county by gender 

drinking <- healthdata_subset |> 
  ggplot(aes(x=excessive_drinking_raw_value, y=low_birthweight_raw_value))+
  geom_point(alpha =0.3)  +
  geom_smooth(method = lm)
  #geom_violin(fill = "red")+
  #geom_boxplot()

#physical inactivity and low birthweight
inactivity <- healthdata_subset |> 
  ggplot(aes(x= physical_inactivity_raw_value, y=low_birthweight_raw_value))+
  geom_point(alpha =0.3)+
  geom_smooth(method = lm)
  #geom_violin(fill = "blue")

#insufficient sleep and low birthweight
sleep <- healthdata_subset |>
  ggplot(aes(x= insufficient_sleep_raw_value, y=low_birthweight_raw_value))+
  geom_point(alpha = 0.3) +
  geom_smooth(method = lm)
  #geom_violin(fill = "purple")

sti <- healthdata_subset |> 
  ggplot(aes(x=sexually_transmitted_infections_raw_value, y=low_birthweight_raw_value))+
  geom_point(alpha = 0.3) +
  geom_smooth(method = lm)

uninsured <- healthdata_subset |> 
  ggplot(aes(x=uninsured_raw_value, y=low_birthweight_raw_value))+
  geom_point(alpha = 0.3) +
  geom_smooth(method = lm)

library(cowplot)
plot_grid(smoking,drinking, inactivity, sleep, obesity, food, sti, uninsured)




# Lasso Regression --------------------------------------------------------
#Predicting low birthweight
library(glmnet)
model_predictors <- healthdata_subset |>
  select(adult_smoking_raw_value, adult_obesity_raw_value, 
         food_insecurity_raw_value, excessive_drinking_raw_value,
         physical_inactivity_raw_value,insufficient_sleep_raw_value, 
         sexually_transmitted_infections_raw_value, uninsured_raw_value) |> 
  as.matrix()
  


model_response_birthweight <- healthdata_subset |> 
  pull(low_birthweight_raw_value)

birthweight_lm <- lm(low_birthweight_raw_value ~ adult_smoking_raw_value + adult_obesity_raw_value + 
                       food_insecurity_raw_value + excessive_drinking_raw_value + physical_inactivity_raw_value +
                       insufficient_sleep_raw_value + sexually_transmitted_infections_raw_value + 
                       uninsured_raw_value, data = healthdata_subset)

#difference between predict and lm?

library(broom)
birthweight_lm |> 
  tidy() |> 
  mutate(term = fct_reorder(term, estimate)) |> 
  ggplot(aes(x = estimate, y = term, 
             fill = estimate > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue"))

birthweight_ridge <- glmnet(model_predictors, model_response_birthweight, alpha = 0)
plot(birthweight_ridge, xvar = "lambda")


birthweight_lasso_cv <- cv.glmnet(model_predictors, model_response_birthweight, alpha = 1)

tidy_lasso_coef <- tidy(birthweight_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = birthweight_lasso_cv$lambda.min) +
  geom_vline(xintercept = birthweight_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(birthweight_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = birthweight_lasso_cv$lambda.min) +
  geom_vline(xintercept = birthweight_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()


lasso_final <- glmnet(
  model_predictors, model_response_birthweight,
  alpha = 1,
  lambda = birthweight_lasso_cv$lambda.1se
)
library(vip)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)



