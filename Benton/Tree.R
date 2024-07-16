library(ggplot2)
library(tidyverse)
library(janitor)

Data<- read_csv("Benton/analytic_data2024.csv")


na.omit(Data)
Updated_Data <- Data |> 
  clean_names() |> 
  slice(-1) |> 
  mutate(adult_smoking_raw_value=as.numeric(adult_smoking_raw_value),
         child_mortality_raw_value= as.numeric(child_mortality_raw_value), infant_mortality_raw_value= as.numeric(infant_mortality_raw_value)) |> 
  filter(!is.na(adult_smoking_raw_value), !is.na(child_mortality_raw_value)) |> 
  filter(county_clustered_yes_1_no_0 %in% c(0,1))

Updated_Data |> 
  ggplot(aes(x=adult_smoking_raw_value, y= child_mortality_raw_value))+
  geom_point()


cor(Updated_Data$adult_smoking_raw_value, Updated_Data$child_mortality_raw_value)


#Adult Obesity 

Updated_Data2 <- Updated_Data|> 
  clean_names() |> 
  slice(-1) |> 
  mutate(adult_obesity_raw_value=as.numeric(adult_obesity_raw_value),
         child_mortality_raw_value= as.numeric(child_mortality_raw_value)) |> 
  filter(!is.na(adult_obesity_raw_value), !is.na(child_mortality_raw_value))

Updated_Data2 |> 
  ggplot(aes(x=adult_obesity_raw_value, y= child_mortality_raw_value))+
  geom_point()


cor(Updated_Data2$adult_obesity_raw_value, Updated_Data2$child_mortality_raw_value)



#Excessive Drinking 

Updated_Data3 <- Updated_Data2 |> 
  clean_names() |> 
  slice(-1) |> 
  mutate(excessive_drinking_raw_value=as.numeric(excessive_drinking_raw_value),
         child_mortality_raw_value= as.numeric(child_mortality_raw_value), 
         food_insecurity_raw_value= as.numeric(food_insecurity_raw_value), 
         physical_inactivity_raw_value= as.numeric(food_insecurity_raw_value), 
         insufficient_sleep_raw_value= as.numeric(insufficient_sleep_raw_value), 
         STIs= as.numeric(sexually_transmitted_infections_raw_value))|> 
  filter(!is.na(excessive_drinking_raw_value), !is.na(child_mortality_raw_value))


analysis <- lm(child_mortality_raw_value~adult_smoking_raw_value+adult_obesity_raw_value+ excessive_drinking_raw_value, data=Updated_Data3)
summary(analysis)

#| eval: false
library(ggfortify) # install.packages("ggfortify")
analysis|> 
  autoplot() +
  theme_light()

#Going based off of R^2, it appears smoking the highest indicator of childhood mortaility. 

library(scales)
library(cowplot)
library(patchwork)

obesity<- Updated_Data3 |> 
  ggplot(aes(x= adult_obesity_raw_value, y= child_mortality_raw_value))+
  geom_point(color="brown", alpha=.5)+
  geom_smooth(method= lm, color="darkblue")+
  scale_x_continuous(labels=percent_format())+
  scale_y_continuous()+
  labs(x= "Adult Obesity", y= "Child Mortaility", title= "Adult Obesity")+
  theme(text= element_text(family="Helvetica", size=20, face="bold", color="#2A475E"),
        plot.title = element_text(
          family= "Helvetica",
          size=18, 
          face= "bold", 
          color= "#2A475E", 
          hjust=.5
        ))+
  theme(
    axis.ticks= element_blank(), 
    axis.line= element_line(colour="grey50"), 
    panel.grid= element_line(color="#B4AEA9"),
    panel.grid.minor = element_blank(), 
    panel.grid.major.x = element_blank(), 
    panel.grid.major.y = element_blank(), 
    panel.background = element_rect(fill="#FBF9F4", color= "#FBF9F4"), 
    plot.background = element_rect(fill="#FBF9F4", color= "#FBF9F4")
 )

smoking<- Updated_Data3 |> 
  ggplot(aes(x= adult_smoking_raw_value, y= child_mortality_raw_value))+
  geom_point(color="brown", alpha=.5)+
  geom_smooth(method= lm, color="darkblue")+
  scale_x_continuous(labels=percent_format())+
  scale_y_continuous()+
  labs(x= "Adult Smoking", y= "Child Mortality",  title= "Adult Smoking")+
  theme(text= element_text(family="Helvetica", size=20, face="bold", color="#2A475E"),
        plot.title = element_text(
          family= "Helvetica",
          size=18, 
          face= "bold", 
          color= "#2A475E", 
          hjust=.5
        ))+
  theme(
    axis.ticks= element_blank(), 
    axis.line= element_line(colour="grey50"), 
    panel.grid= element_line(color="#B4AEA9"),
    panel.grid.minor = element_blank(), 
    panel.grid.major.x = element_blank(), 
    panel.grid.major.y = element_blank(), 
    panel.background = element_rect(fill="#FBF9F4", color= "#FBF9F4"), 
    plot.background = element_rect(fill="#FBF9F4", color= "#FBF9F4")
  )

drinking<- Updated_Data3 |> 
  ggplot(aes(x= excessive_drinking_raw_value, y= child_mortality_raw_value))+
  geom_point(color="brown", alpha=.5)+
  geom_smooth(method= lm, color="darkblue")+
  scale_x_continuous(labels=percent_format())+
  scale_y_continuous()+
  labs(x= "Excessive Drinking", y= "Child Mortality",  title= "Excessive Drinking")+
  theme(text= element_text(family="Helvetica", size=20, face="bold", color="#2A475E"),
        plot.title = element_text(
          family= "Helvetica",
          size=18, 
          face= "bold", 
          color= "#2A475E", 
          hjust=.5
        ))+
  theme(
    axis.ticks= element_blank(), 
    axis.line= element_line(colour="grey50"), 
    panel.grid= element_line(color="#B4AEA9"),
    panel.grid.minor = element_blank(), 
    panel.grid.major.x = element_blank(), 
    panel.grid.major.y = element_blank(), 
    panel.background = element_rect(fill="#FBF9F4", color= "#FBF9F4"), 
    plot.background = element_rect(fill="#FBF9F4", color= "#FBF9F4")
  )

plot_grid(drinking+obesity)



infant<- Updated_Data3 |> 
  ggplot(aes(x= excessive_drinking_raw_value, y= infant_mortality_raw_value))+
  geom_point(color="brown", alpha=.5)+
  geom_smooth(method= lm, color="darkblue")+
  scale_x_continuous(labels=percent_format())+
  scale_y_continuous()+
  labs(x= "Excessive Drinking", y= "Infant Mortaliity",  title= "Excessive Drinking")+
  theme(text= element_text(family="Helvetica", size=17, face="bold", color="#2A475E"),
        plot.title = element_text(
          family= "Helvetica",
          size=17, 
          face= "bold", 
          color= "#2A475E", 
          hjust=.5
        ))+
  theme(
    axis.ticks= element_blank(), 
    axis.line= element_line(colour="grey50"), 
    panel.grid= element_line(color="#B4AEA9"),
    panel.grid.minor = element_blank(), 
    panel.grid.major.x = element_blank(), 
    panel.grid.major.y = element_blank(), 
    panel.background = element_rect(fill="#FBF9F4", color= "#FBF9F4"), 
    plot.background = element_rect(fill="#FBF9F4", color= "#FBF9F4")
  )


plot_grid(infant+drinking)

model <-lm(data= Updated_Data3, infant_mortality_raw_value~excessive_drinking_raw_value)
summary(model)


# Tree Diagram 

Updated_Data4 <- Updated_Data3 |> 
  select(child_mortality_raw_value, 
         excessive_drinking_raw_value, adult_smoking_raw_value, 
         adult_obesity_raw_value, STIs, insufficient_sleep_raw_value,
         physical_inactivity_raw_value, food_insecurity_raw_value, 
         uninsured_raw_value) |> 
  mutate(child_mortality_raw_value =child_mortality_raw_value,
         excessive_drinking_raw_value = excessive_drinking_raw_value, 
         adult_smoking_raw_value= adult_smoking_raw_value,
         adult_obesity_raw_value= adult_obesity_raw_value, 
         insufficient_sleep_raw_value= insufficient_sleep_raw_value, 
        physical_inactivity_raw_value= physical_inactivity_raw_value, 
        food_insecurity_raw_value= food_insecurity_raw_value, 
        uninsured_raw_value= as.numeric(uninsured_raw_value )) |> 
  na.omit()

# colnames(Updated_Data4) <- c("Child Mortality", "Excessive Drinking", "Smoking", "Obesity", "STIs", "Insufficent Sleep", "Physical Inactivity", "Food Insecurity", "Uninsured")
  
set.seed(1)
train <- Updated_Data4 |> 
  slice_sample(prop = 0.5)
test <- Updated_Data4 |> 
  anti_join(train)

library(caret)
hr_tree <- train(child_mortality_raw_value ~ ., method = "rpart", tuneLength = 20,
                 trControl = trainControl(method = "cv", number = 10),
                 data = as.data.frame(train))
# str(hr_tree)
ggplot(hr_tree)

split.fun <- function(x, labs, digits, varlen, faclen){
  # replace variable names in the labels
  labs   <- sub("child_mortality_raw_value",   "Child Mortality", labs)
  labs   <- sub("excessive_drinking_raw_value",   "Excessive Drinking", labs)
  labs   <- sub("adult_smoking_raw_value",   "Smoking", labs)
  labs   <- sub("adult_obesity_raw_value",   "Obesity", labs)
  labs   <- sub("STIs",   "STIs", labs)
  labs   <- sub("insufficient_sleep_raw_value",   "Insufficent Sleep", labs)
  labs   <- sub("physical_inactivity_raw_value", "Physical Inactivity", labs)
  labs   <- sub(" food_insecurity_raw_value", "Food Insecurity", labs)
  labs   <- sub("uninsured_raw_value", "Uninsured", labs)
  
  # labs <- sub("survived", "survived", labs)
  labs # return the modified labels
}

library(rpart.plot) 
hr_tree |> 
  pluck("finalModel") |> 
  rpart.plot(tweak= 1.5, split.fun = split.fun, box.palette = c("white"))

# Evaluation
train |> 
  mutate(pred = predict(hr_tree, newdata = train)) |> 
  summarize(correct = mean(child_mortality_raw_value == pred))

test |> 
  mutate(pred = predict(hr_tree, newdata = test)) |> 
  summarize(correct = mean(child_mortality_raw_value == pred))



 
 library(vip)
 vip_plot = hr_tree |> 
   vip(aesthetics = list(fill = "brown")) + scale_x_discrete(labels = c(adult_obesity_raw_value='Adult Obesity',
                                       adult_smoking_raw_value = 'Adult Smoking',
                                       excessive_drinking_raw_value= 'Excessive Drinking', 
                            physical_inactivity_raw_value= 'Physical Inactivity', 
                            insufficient_sleep_raw_value= 'Insufficent Sleep', 
                           food_insecurity_raw_value= 'Food Insecurity', 
                           uninsured_raw_value= 'Uninsured'))+
   # scale_x_continuous(labels=percent_format())+
   scale_y_continuous()+
   theme(text= element_text(family="Helvetica", size=20, face="bold", color="#2A475E"),
         plot.title = element_text(
           family= "Helvetica",
           size=18, 
           face= "bold", 
           color= "#2A475E", 
           hjust=.5
         ))+
   theme(
     axis.ticks= element_blank(), 
     axis.line= element_line(colour="grey50"), 
     panel.grid= element_line(color="#B4AEA9"),
     panel.grid.minor = element_blank(), 
     panel.grid.major.x = element_blank(), 
     panel.grid.major.y = element_blank(), 
     panel.background = element_rect(fill="#FBF9F4", color= "#FBF9F4"), 
     plot.background = element_rect(fill="#FBF9F4", color= "#FBF9F4")
   )
 
 drinking<- Updated_Data3 |> 
   ggplot(aes(x= excessive_drinking_raw_value, y= child_mortality_raw_value))+
   geom_point(color="brown", alpha=.5)+
   geom_smooth(method= lm, color="darkblue")+
   scale_x_continuous(labels=percent_format())+
   scale_y_continuous()+
   labs(x= "Excessive Drinking", y= "Child Mortality",  title= "Excessive Drinking")+
   theme(text= element_text(family="Helvetica", size=20, face="bold", color="#2A475E"),
         plot.title = element_text(
           family= "Helvetica",
           size=18, 
           face= "bold", 
           color= "#2A475E", 
           hjust=.5
         ))+
   theme(
     axis.ticks= element_blank(), 
     axis.line= element_line(colour="grey50"), 
     panel.grid= element_line(color="#B4AEA9"),
     panel.grid.minor = element_blank(), 
     panel.grid.major.x = element_blank(), 
     panel.grid.major.y = element_blank(), 
     panel.background = element_rect(fill="#FBF9F4", color= "#FBF9F4"), 
     plot.background = element_rect(fill="#FBF9F4", color= "#FBF9F4")
   )
 
 
 vip_plot

 
