library(tidyverse)
read_csv("Devraj/analytic_data2024.csv")

library(ggpubr)
library(janitor)
library(tidyverse)
library(scales)
install.packages("ggExtra")
library(ggExtra)

theme_set(theme_light())
healthdata <- read_csv("Data/analytic_data2024.csv") |> 
  slice(-1) |> 
  clean_names()
 
View(healthdata)

tibble(healthdata)

glimpse(healthdata)
 
#which adult-health related factors impact low birth weight 

#Goal...How do different adult health-related practices predict low birth weight?->use lasso regression?

#Low birthweight is defined as less than 2500 grams
 
str(healthdata)

healthdata_subset <- healthdata |> 
  select(state_abbreviation, name,county_clustered_yes_1_no_0, release_year, adult_smoking_raw_value, adult_obesity_raw_value, 
         food_insecurity_raw_value, excessive_drinking_raw_value, child_mortality_raw_value,
         physical_inactivity_raw_value,insufficient_sleep_raw_value, low_birthweight_raw_value, 
        sexually_transmitted_infections_raw_value, uninsured_raw_value) |> 
  mutate(across(ends_with('raw_value'), as.numeric)) |> 
  filter(county_clustered_yes_1_no_0 %in% c(0,1)) |> 
  na.omit()

#percent_female_raw_value, #low_birthweight_asian_pacific_islander, low_birthweight_black, 
#low_birthweight_asian_pacific_islander, low_birthweight_white,low_birthweight_hispanic, 



View(healthdata_subset)
str(healthdata_subset)

mean(healthdata_subset$child_mortality_raw_value)


birthweight_plot <- function(var_name, var_label){
  plot <- healthdata_subset |>
    ggplot(aes(x= {{var_name}}, y=low_birthweight_raw_value))+
    geom_point(alpha =0.4, color = "brown")+
   #geom_point(size = 3, alpha = 0.3, color = "brown") + 
    geom_smooth(method = lm, color = "#2a475e") +
    stat_cor(method="pearson")+
    #geom_density2d() +
    #coord_fixed() +
    #theme(legend.position = "bottom")+
    #geom_rug(alpha = 0.5, color = "#2a475e")+
    scale_x_continuous(labels=percent_format())+
    scale_y_continuous(labels=percent_format())+
  labs(x = paste("Proportion of ", var_label),
       y = "Proportion of low birthweight",
       title = paste0(var_label, " vs. Low Birthweight"))+
  theme(text = element_text(family = "Helvetica", size = 8, face = "bold", color = "#2a475e"),
        plot.title = element_text(
          family = "Helvetica", 
          size = 10,
          face = "bold",
          color = "#2a475e",
          hjust = 0.5
        ))+
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
  return(plot)
}


smoke_plot = birthweight_plot(adult_smoking_raw_value, "Adult Smoking")
drinking_plot = birthweight_plot(excessive_drinking_raw_value, "Adult Drinking")
obesity_plot = birthweight_plot(adult_obesity_raw_value, "Adult Obesity")
inactivity_plot = birthweight_plot(physical_inactivity_raw_value, "Physical Inactivity")
sleep_plot = birthweight_plot(insufficient_sleep_raw_value, "Insufficient Sleep")
food_plot = birthweight_plot(food_insecurity_raw_value, "Food insecurity")
sti_plot = birthweight_plot(sexually_transmitted_infections_raw_value, "Sexually Transmitted Infections")
uninsured_plot = birthweight_plot(uninsured_raw_value, "Uninsured Adults")

healthdata_subset |> 
  cor()


plot_grid(sleep_plot, smoke_plot, sti_plot, obesity_plot)

library(cowplot)



mean(healthdata_subset$low_birthweight_raw_value)














# Experimenting with customization ------------------------------------------------------------

install.packages("ggstatsplot")
library(ggstatsplot)

install.packages("rstantools")
library(rstantools)

obesity <- healthdata_subset |>
  ggplot(aes(x=adult_obesity_raw_value, y=low_birthweight_raw_value))+
  ggcorrplot(corr, method = "circle")+
  #geom_smooth(method = lm, color = "darkblue") +
  labs(x= "Proportion of adult obesity",
       y = "Proportion of low birthweight",
       title = "Relationship of adult obesity on low birthweight")+
  theme(text = element_text(family = "Helvetica", size = 12, face = "bold", color = "#2a475e"),
        plot.title = element_text(
          family = "Helvetica", 
          size = 20,
          face = "bold",
          color = "#2a475e"
        ))+
  theme(
    axis.ticks = element_blank(),
    axis.line = element_line(colour = "grey50"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4")
  )
























# Nonparametric regression ------------------------------------------------


healthdata_subset <- healthdata_subset |> 
  mutate(binary_child_mortality = as.factor(ifelse(child_mortality_raw_value >=62, "high", "low")))



healthdata_subset |> 
  ggplot(aes(x = insufficient_sleep_raw_value, 
             y = excessive_drinking_raw_value, 
             color = binary_child_mortality))+
  geom_point(alpha = 0.7)

set.seed(123)
train <- healthdata_subset |> 
  slice_sample(prop = 0.5)
test <- healthdata_subset |> 
  anti_join(train)

library(mgcv)
mortality_gam <- gam(binary_child_mortality ~ s(insufficient_sleep_raw_value) + s(excessive_drinking_raw_value),
              family = binomial,
              method = "REML", 
              data = train)

library(broom)
tidy(mortality_gam)
tidy(mortality_gam, parametric = TRUE) 


library(gratia)
draw(mortality_gam)
draw(mortality_gam, fun = plogis)
draw(mortality_gam, fun = plogis, constant = coef(mortality_gam)[1])

appraise(mortality_gam)


mortality_gam |> 
  augment(type.predict = "response") |> 
  mutate(newdata = train, pred_class = round(.fitted)) |> 
  summarize(correct = mean(binary_child_mortality == pred_class))

mortality_gam |> 
  augment(newdata = test, type.predict = "response") |> 
  mutate(pred_class = round(.fitted)) |> 
  summarize(correct = mean(binary_child_mortality == pred_class))



mortality_gam_mult <- gam(binary_child_mortality ~ s(insufficient_sleep_raw_value, excessive_drinking_raw_value), family = binomial, data = train)

mortality_gam_mult |> 
  smooth_estimates() |> 
  ggplot(aes(insufficient_sleep_raw_value, excessive_drinking_raw_value, z = .estimate)) +
  geom_contour_filled()

mortality_gam_mult |> 
  smooth_estimates() |> 
  mutate(prob = plogis(.estimate)) |> 
  ggplot(aes(insufficient_sleep_raw_value, excessive_drinking_raw_value, z = prob)) +
  geom_contour_filled()



mortality_gam_mult |> 
  augment(newdata = train, type.predict = "response") |> 
  mutate(pred_class = round(.fitted)) |> 
  summarize(correct = mean(binary_child_mortality == pred_class))








# Lollipop plot -----------------------------------------------------------


healthdata_subset <- healthdata_subset |> 
  mutate(binary_child_mortality = as.factor(ifelse(child_mortality_raw_value >=62, "yes", "no")))


healthdata_subset <- healthdata_subset |> 
  mutate(binary_sleep = as.factor(ifelse(insufficient_sleep_raw_value >=0.37, "yes", "no")))

healthdata_subset <- healthdata_subset |> 
  mutate(binary_drinking = as.factor(ifelse(excessive_drinking_raw_value >=0.19, "yes", "no")))

summary(healthdata_subset$excessive_drinking_raw_value)

View(healthdata_subset)

#Sleep
healthdata_subset |> 
 #mutate(state_abbreviation=factor(state_abbreviation), 
        # state_abbreviation = fct_reorder(state_abbreviation, low_birthweight_raw_value)) |> 
  ggplot(aes(group=state_abbreviation)) +
  geom_segment(aes(x=state_abbreviation, xend=state_abbreviation, y=0, yend=low_birthweight_raw_value)) +
  geom_point(aes(x=state_abbreviation,y=low_birthweight_raw_value, fill = binary_sleep),size=4.5, 
             alpha=0.60, shape=21)+
  #scale_y_continuous(breaks=seq(0,600,50))+
  scale_y_continuous(labels=percent_format())+
  coord_flip()+
  theme(
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 18, hjust = 0.5, vjust = 0.5, face = "bold", 
                              margin = margin(b = 0.2, unit = "cm")),
    axis.ticks = element_blank(),
    #axis.line = element_line(colour = "grey50"),
    #panel.grid = element_line(color = "#b4aea9"),
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    axis.text.x = element_text(face="bold", color = "black", size = 15),
    axis.text.y = element_text(face="bold", color="black", size = 11),
    axis.title.y = element_text(face = "bold", color = "brown", size = 25),
    axis.title.x = element_text(face = "bold", color = "brown",vjust = -0.85, size = 25),
    legend.background =  element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 18)
  ) +
  scale_fill_manual(values = c("yes"="brown1","no"="dimgrey"))+
  labs(
    x = "State",
    y = "Low Birthweight",
    fill = "Insufficient sleep in \u{2265} 75% of county population ",
  )


#Drinking
healthdata_subset |> 
  ggplot(aes(group=state_abbreviation)) +
  geom_segment(aes(x=state_abbreviation, xend=state_abbreviation, y=0, yend=low_birthweight_raw_value)) +
  geom_point(aes(x=state_abbreviation,y=low_birthweight_raw_value, fill = binary_drinking),size=3, 
             alpha=0.5, shape=21)+
  #scale_y_continuous(breaks=seq(0,600,50))+
  scale_y_continuous(labels=percent_format())+
  coord_flip()+
  theme(
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 13, hjust = 0.5, vjust = 0.5, face = "bold", 
                              margin = margin(b = 0.2, unit = "cm")),
    axis.ticks = element_blank(),
    #axis.line = element_line(colour = "grey50"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    axis.text.x = element_text(face="bold", color = "black"),
    axis.text.y = element_text(face="bold", color="black"),
    axis.title.y = element_text(face = "bold", color = "darkred"),
    axis.title.x = element_text(face = "bold", color = "darkred",vjust = -0.85),
    legend.background =  element_rect(fill = "#fbf9f4", color = "#fbf9f4")
  ) +
  scale_fill_manual(values = c("yes"="red","no"="darkgrey"))+
  labs(
    x = "State",
    y = "Low Birthweight",
    fill = "Excessive drinking in \u{2265} 75% of county population ",
  )

mean(healthdata_subset$child_mortality_raw_value)


library(GGally)
library(ggcorrplot)
healthdata_subset |> 
  select(low_birthweight_raw_value, insufficient_sleep_raw_value, excessive_drinking_raw_value,adult_smoking_raw_value, adult_obesity_raw_value,
            food_insecurity_raw_value, physical_inactivity_raw_value, sexually_transmitted_infections_raw_value, 
            uninsured_raw_value) |> 
  cor() |> 
  ggpairs()









