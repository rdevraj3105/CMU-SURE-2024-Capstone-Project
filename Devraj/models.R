library(tidyverse)
read_csv("Devraj/analytic_data2024.csv")

library(ggpubr)
library(janitor)
library(tidyverse)
library(scales)

library(ggExtra)

theme_set(theme_light())
healthdata <- read_csv("Data/analytic_data2024.csv") |> 
  slice(-1) |> 
  clean_names()

View(healthdata)

healthdata_subset <- healthdata |> 
  select(state_abbreviation, name,county_clustered_yes_1_no_0, release_year, adult_smoking_raw_value, adult_obesity_raw_value, 
         food_insecurity_raw_value, excessive_drinking_raw_value, child_mortality_raw_value,
         physical_inactivity_raw_value,insufficient_sleep_raw_value, low_birthweight_raw_value, 
         sexually_transmitted_infections_raw_value, uninsured_raw_value) |> 
  mutate(across(ends_with('raw_value'), as.numeric)) |> 
  filter(county_clustered_yes_1_no_0 %in% c(0,1)) |> 
  na.omit()

View(healthdata_subset)


# Lasso Regression --------------------------------------------------------

#Predicting low birthweight

library(glmnet)

model_predictors <- healthdata_subset |>
  select(adult_smoking_raw_value, adult_obesity_raw_value, food_insecurity_raw_value, 
         excessive_drinking_raw_value, physical_inactivity_raw_value, insufficient_sleep_raw_value, 
         sexually_transmitted_infections_raw_value, uninsured_raw_value) |> 
  as.matrix()

model_response_birthweight <- healthdata_subset |> 
  pull(low_birthweight_raw_value)

birthweight_lm <- lm(low_birthweight_raw_value ~ adult_smoking_raw_value + adult_obesity_raw_value + 
                       food_insecurity_raw_value + excessive_drinking_raw_value + physical_inactivity_raw_value +
                       insufficient_sleep_raw_value + sexually_transmitted_infections_raw_value + 
                       uninsured_raw_value, data = healthdata_subset)

summary(birthweight_lm)

library(broom)
birthweight_lm |> 
  tidy() |> 
  mutate(term = fct_reorder(term, estimate)) |> 
  ggplot(aes(x = estimate, y = term, fill = estimate > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue"))


#summary(birthweight_lm)

#birthweight_ridge <- glmnet(model_predictors, model_response_birthweight, alpha = 0)

#plot(birthweight_ridge, xvar = "lambda")


new_labels <- c("Adult Smoking" = adult_smoking_raw_value, "Adult Obesity" = adult_obesity_raw_value, 
                "Food Insecurity" = food_insecurity_raw_value, "Excessive Drinking" = excessive_drinking_raw_value,
                "Physical Inactivity" = physical_inactivity_raw_value, "Insufficient Sleep" = insufficient_sleep_raw_value, 
                "STIs" = sexually_transmitted_infections_raw_value, "Uninsured" = uninsured_raw_value) 

birthweight_lasso_cv <- cv.glmnet(model_predictors, model_response_birthweight, alpha = 1)


tidy_lasso_coef <- tidy(birthweight_lasso_cv$glmnet.fit)

tidy_lasso_coef |> 
  mutate(term = str_remove(term, "_raw_value")) |> 
  mutate(term = str_replace_all(term, "_", " ")) |> 
  mutate(term = str_to_sentence(term)) |> 
  ggplot(aes(x = lambda, y = estimate, group = term, color = term)) +
  scale_x_log10(labels=scales::comma_format()) +
  geom_line(alpha = 1, size =1) +
  geom_vline(xintercept = birthweight_lasso_cv$lambda.min) +
  geom_vline(xintercept = birthweight_lasso_cv$lambda.1se, linetype = "dashed", color = "black", size =0.5)+
  labs(x = "Lambda",
       y = "Coefficient Estimate",
       color = ""
  )+
  theme(
    text = element_text(family = "Helvetica", size = 10, face = "bold", color = "#2a475e"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    legend.background =  element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    legend.text = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
    
  )

tidy_lasso_cv <- tidy(birthweight_lasso_cv)

tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = birthweight_lasso_cv$lambda.min) +
  geom_vline(xintercept = birthweight_lasso_cv$lambda.1se, linetype = "dashed", color = "red") +
  scale_x_log10()

lasso_final <- glmnet(model_predictors, model_response_birthweight, alpha = 1,lambda = birthweight_lasso_cv$lambda.1se)

library(vip)



lasso_final |> 
  vi() |> 
  mutate(Variable = str_remove(Variable, "_raw_value")) |> 
  mutate(Variable = str_replace_all(Variable, "_", " ")) |> 
  mutate(Variable = str_to_sentence(Variable)) |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |> 
  ggplot(aes(x = Importance, y = Variable, fill = Importance)) +
  geom_col(color = "white", show.legend = FALSE) +
  #geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
  #scale_fill_manual(values = c("darkred", "darkblue")) +
  scale_fill_gradient(low = "darkgrey", high = "brown")+
  labs(x = "Estimate", y = NULL)+
  theme(text = element_text(family = "Helvetica", size = 8, face = "bold", color = "#2a475e"),
        plot.title = element_text(
          family = "Helvetica", 
          size = 10,
          face = "bold",
          color = "#2a475e",
          hjust = 0.5
        ),
        axis.text.y=element_text(color = "#2a475e"))+
  theme(
    axis.ticks = element_blank(),
    axis.line = element_line(colour = "grey50"),
    panel.grid = element_line(color = "#b4aea9"),
    #panel.grid.minor = element_blank(),
    #panel.grid.major.x = element_blank(),
    #panel.grid.major.y = element_blank(),
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    text = element_text(family = "Helvetica", size = 18, 
                        face = "bold", color = "#2a475e"),
    plot.title = element_text(
      family = "Helvetica", 
      size = 20,
      face = "bold",
      color = "#2a475e",
      hjust = 0.5))

#explain how the lambda was picked
#Lambda was picked with cross validation
#lambda min λ is the minimum mean cross-validated error
#largest value of lambda such that error is within 1 standard error of the cross-validated errors for lambda.min.












# Linear Regression -------------------------------------------------------
birthweight_lm <- lm(low_birthweight_raw_value ~ adult_smoking_raw_value + adult_obesity_raw_value + 
                       food_insecurity_raw_value + excessive_drinking_raw_value + physical_inactivity_raw_value +
                       insufficient_sleep_raw_value + sexually_transmitted_infections_raw_value + 
                       uninsured_raw_value, data = healthdata_subset)
summary(birthweight_lm)
tidy(birthweight_lm)

head(birthweight_lm$fitted.values)

#Linear Model
healthdata_subset |> 
  mutate(pred_vals = predict(birthweight_lm)) |> 
  ggplot(aes(x = pred_vals, y = low_birthweight_raw_value))+
  geom_point(alpha = 0.5, size =3)+
  geom_abline(slope = 1, intercept = 0, 
              linetype = "dashed",
              color = "red",
              linewidth = 2)


#Residuals
birthweight_lm |>
  ggplot(aes(x = .fitted, y = .resid)) + 
  geom_point(alpha = 0.5, size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", 
             color = "red", linewidth = 2) +
  geom_smooth(se = FALSE)


#Regression coefficients
birthweight_lm |> 
  tidy(conf.int = TRUE) |> 
  mutate(term = str_remove(term, "_raw_value")) |> 
  mutate(term = str_replace_all(term, "_", " ")) |> 
  mutate(term = str_to_sentence(term)) |> 
  mutate(term = fct_reorder(term, estimate)) |> 
  ggplot(aes(x = estimate, y = term,  
             fill = estimate > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
  scale_fill_manual(values = c("darkred", "darkblue"))+
  labs(
    x = "Coefficient Estimate"
  )+
  theme(
    text = element_text(family = "Helvetica", size = 10, face = "bold", color = "#2a475e"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    legend.background =  element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    legend.text = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15, FALSE),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  )

#VIP plot
birthweight_lm |> 
  vip()
?vip

birthweight_lm |> 










# Model evaluation --------------------------------------------------------

set.seed(150)
k <- 10
healthdata_subset <- healthdata_subset |>
  mutate(test_fold = sample(rep(1:k, length.out = n())))

get_test_pred <- function(k) {
  test_data <- healthdata_subset |> filter(test_fold == k)                     
  train_data <- healthdata_subset |> filter(test_fold != k)
  test_x <- as.matrix(select(test_data, adult_smoking_raw_value, adult_obesity_raw_value, 
                             food_insecurity_raw_value, excessive_drinking_raw_value, child_mortality_raw_value,
                             physical_inactivity_raw_value,insufficient_sleep_raw_value, 
                             sexually_transmitted_infections_raw_value, uninsured_raw_value))                       
  train_x <- as.matrix(select(train_data, adult_smoking_raw_value, adult_obesity_raw_value, 
                              food_insecurity_raw_value, excessive_drinking_raw_value, child_mortality_raw_value,
                              physical_inactivity_raw_value,insufficient_sleep_raw_value,
                              sexually_transmitted_infections_raw_value, uninsured_raw_value))
  
  lm_fit <- lm(low_birthweight_raw_value ~ adult_smoking_raw_value + adult_obesity_raw_value +
               food_insecurity_raw_value + excessive_drinking_raw_value + child_mortality_raw_value +
               physical_inactivity_raw_value + insufficient_sleep_raw_value + sexually_transmitted_infections_raw_value +
               uninsured_raw_value, data = train_data)                           
  ridge_fit <- cv.glmnet(train_x, train_data$low_birthweight_raw_value, alpha = 0)
  lasso_fit <- cv.glmnet(train_x, train_data$low_birthweight_raw_value, alpha = 1)
  
  tibble(lm_pred = predict(lm_fit, newdata = test_data),              # return test results
         ridge_pred = as.numeric(predict(ridge_fit, newx = test_x)),
         lasso_pred = as.numeric(predict(lasso_fit, newx = test_x)),
         test_actual = test_data$low_birthweight_raw_value,
         test_fold = k)
}

test_pred_all <- map(1:k, get_test_pred) |> 
  bind_rows()


test_pred_all |>
  pivot_longer(lm_pred:lasso_pred, 
               names_to = "type", 
               values_to = "test_pred") |>
  group_by(type, test_fold) |>
  summarize(
    rmse = sqrt(mean((test_actual - test_pred)^2))
  ) |> 
  ggplot(aes(x = type, y = rmse)) + 
  geom_point(size = 4) +
  stat_summary(fun = mean, geom = "point", 
               color = "red", size = 4) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               color = "red", width = 0.2)








#explain how the lambda was picked
#Lambda was picked with cross validation
#lambda min λ is the minimum mean cross-validated error
#largest value of lambda such that error is within 1 standard error of the cross-validated errors for lambda.min.



# Linear Regression -------------------------------------------------------
birthweight_lm <- lm(low_birthweight_raw_value ~ adult_smoking_raw_value + adult_obesity_raw_value + 
                       food_insecurity_raw_value + excessive_drinking_raw_value + physical_inactivity_raw_value +
                       insufficient_sleep_raw_value + sexually_transmitted_infections_raw_value + 
                       uninsured_raw_value, data = healthdata_subset)
summary(birthweight_lm)
tidy(birthweight_lm)
train_preds <- predict(birthweight_lm)
head(train_preds)
head(birthweight_lm$fitted.values)

healthdata_subset |> 
  mutate(pred_vals = predict(birthweight_lm)) |> 
  ggplot(aes(x = pred_vals, y = low_birthweight_raw_value))+
  geom_point(alpha = 0.5, size =3)+
  geom_abline(slope = 1, intercept = 0, 
              linetype = "dashed",
              color = "red",
              linewidth = 2)






healthdata_subset <- healthdata_subset |> 
  mutate(binary_child_mortality = as.factor(ifelse(child_mortality_raw_value >=62, "yes", "no")))


healthdata_subset <- healthdata_subset |> 
  mutate(binary_sleep = as.factor(ifelse(insufficient_sleep_raw_value >=0.37, "yes", "no")))

healthdata_subset <- healthdata_subset |> 
  mutate(binary_drinking = as.factor(ifelse(excessive_drinking_raw_value >=0.19, "yes", "no")))





