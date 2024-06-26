library(tidyverse)
library(janitor)

Data<- read_csv("Benton/analytic_data2024.csv")


#Adult Smoking 

Updated_Data <- Data |> 
  clean_names() |> 
  slice(-1) |> 
  mutate(adult_smoking_raw_value=as.numeric(adult_smoking_raw_value),
         child_mortality_raw_value= as.numeric(child_mortality_raw_value)) |> 
  filter(!is.na(adult_smoking_raw_value), !is.na(child_mortality_raw_value))

Updated_Data |> 
  ggplot(aes(x=adult_smoking_raw_value, y= child_mortality_raw_value))+
  geom_point()+


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

#No obvious pattern for graph one 
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


#Going based off of R^2, it appears smoking the highest indicator of childhood mortaility. 