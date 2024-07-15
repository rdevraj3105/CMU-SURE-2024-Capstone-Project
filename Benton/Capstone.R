library(ggplot2)
library(tidyverse)
library(janitor)

Data<- read_csv("Benton/analytic_data2024.csv")


#Adult Smoking 
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
         child_mortality_raw_value= as.numeric(child_mortality_raw_value)) |> 
  filter(!is.na(excessive_drinking_raw_value), !is.na(child_mortality_raw_value))

Updated_Data3 |> 
  ggplot(aes(x= excessive_drinking_raw_value, y= child_mortality_raw_value))+
  geom_point()


cor(Updated_Data3$excessive_drinking_raw_value, Updated_Data3$child_mortality_raw_value)




#Regression Analysis 

smoking_analysis<- lm(child_mortality_raw_value~adult_smoking_raw_value, data=Updated_Data)
summary (smoking_analysis)


obesity_analysis<- lm(child_mortality_raw_value~ adult_obesity_raw_value, data = Updated_Data2)
summary(obesity_analysis)


drinking_analysis <-  lm(child_mortality_raw_value~ excessive_drinking_raw_value, data = Updated_Data3)
summary(drinking_analysis)

#Looks like adult smoking is the best predictor of childhood mortaility out of the three 



#Diagnostic Tests 

#| eval: false
library(ggfortify) # install.packages("ggfortify")
smoking_analysis|> 
  autoplot() +
  theme_light()

#Cone Pattern pattern for graph one 
#Line is normal for graph two
#There appears to be equal constant variance 
#There appears to be an outlier

#| eval: false
library(ggfortify) # install.packages("ggfortify")
obesity_analysis|> 
  autoplot() +
  theme_light()

#No obvious Pattern 
#Line is good 
#A small cone shape 
#No outliers 

#| eval: false
library(ggfortify) # install.packages("ggfortify")
drinking_analysis|> 
  autoplot() +
  theme_light()

#All graphs look good

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
  theme(text= element_text(family="Helvetica", size=15, face="bold", color="#2A475E"),
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

smoking<- Updated_Data3 |> 
  ggplot(aes(x= adult_smoking_raw_value, y= child_mortality_raw_value))+
  geom_point(color="brown", alpha=.5)+
  geom_smooth(method= lm, color="darkblue")+
  scale_x_continuous(labels=percent_format())+
  scale_y_continuous()+
  labs(x= "Adult Smoking", y= "Child Mortaility",  title= "Adult Smoking")+
  theme(text= element_text(family="Helvetica", size=15, face="bold", color="#2A475E"),
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

drinking<- Updated_Data3 |> 
  ggplot(aes(x= excessive_drinking_raw_value, y= child_mortality_raw_value))+
  geom_point(color="brown", alpha=.5)+
  geom_smooth(method= lm, color="darkblue")+
  scale_x_continuous(labels=percent_format())+
  scale_y_continuous()+
  labs(x= "Excessive Drinking", y= "Child Mortaility",  title= "Excessive Drinking")+
  theme(text= element_text(family="Helvetica", size=15, face="bold", color="#2A475E"),
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

plot_grid(drinking+smoking+obesity)



infant<- Updated_Data3 |> 
  ggplot(aes(x= excessive_drinking_raw_value, y= infant_mortality_raw_value))+
  geom_point(color="brown", alpha=.5)+
  geom_smooth(method= lm, color="darkblue")+
  scale_x_continuous(labels=percent_format())+
  scale_y_continuous()+
  labs(x= "Excessive Drinking", y= "Infant Mortaility",  title= "Excessive Drinking")+
  theme(text= element_text(family="Helvetica", size=15, face="bold", color="#2A475E"),
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

set.seed(123)
Updated_Data4 <- Updated_Data3 |> 
  select(child_mortality_raw_value, excessive_drinking_raw_value, adult_smoking_raw_value, adult_obesity_raw_value) |> 
  na.omit()
train <- Updated_Data4 |> 
  slice_sample(prop = 0.5)
test <- Updated_Data4 |> 
  anti_join(train)

library(caret)
hr_tree <- train(child_mortality_raw_value~ ., method = "rpart", tuneLength = 20,
                 trControl = trainControl(method = "cv", number = 10),
                 data = train)
# str(hr_tree)

library(rpart.plot)
hr_tree |> 
  pluck("finalModel") |> 
  rpart.plot()

# Evaluation
train |> 
  mutate(pred = predict(hr_tree, newdata = train)) |> 
  summarize(correct = mean(child_mortality_raw_value == pred))

test |> 
  mutate(pred = predict(hr_tree, newdata = test)) |> 
  summarize(correct = mean(child_mortality_raw_value == pred))


# Mean 
 mean(Updated_Data3$child_mortality_raw_value)