


library(tidyverse)
read_csv("Devraj/analytic_data2024.csv")

2
 
library(tidyverse)
3
 
healthdata <- read_csv("Data/analytic_data2024.csv") |> 
4
 
  slice(-1) |> 
5
 
  clean_names()
6
 
View(healthdata)
7
 
tibble(healthdata)
8
 
#which adult-health related factors impact low birth weight 
9
 
10
 
# healthdata |> 
11
 
#   slice(-1) |>  
Unstage line	
 
12
 
#   select(prematuredeath_num = "Premature Death numerator", prematuredeath_denom = "Premature Death denominator") |> 
13
 
#   mutate(prematuredeath_num = as.numeric(prematuredeath_num),
14
 
#          prematuredeath_denom = as.numeric(prematuredeath_denom),
15
 
#     prop_premature =  prematuredeath_num/prematuredeath_denom)
16
 
17
 
18
 
19
 
#Goal...How do different adult health-related practices predict low birth weight?->use lasso regression?
20
 
21
 
str(healthdata)
22
 
23
 
healthdata_subset <- healthdata |> 
24
 
  select(state_abbreviation, name, release_year, adult_smoking_raw_value, adult_obesity_raw_value, 
25
 
         food_insecurity_raw_value, excessive_drinking_raw_value,
26
 
         physical_inactivity_raw_value,insufficient_sleep_raw_value, low_birthweight_raw_value, 
27
 
         percent_female_raw_value, low_birthweight_asian_pacific_islander, low_birthweight_black, 
28
 
         low_birthweight_asian_pacific_islander, low_birthweight_white,low_birthweight_hispanic, 
29
 
         sexually_transmitted_infections_raw_value, uninsured_raw_value
30
 
         ) |> 
31
 
  mutate(across(ends_with('raw_value'), as.numeric)) |> 
32
 
  na.omit()
33
 
34
 
35
 
#healthdata_subset <- healthdata_subset[complete.cases(healthdata_subset),]
36
 
37
 
38
 
#Obesity and low birthweight
39
 
obesity <- healthdata_subset |> 
40
 
  ggplot(aes(x=adult_obesity_raw_value, y=low_birthweight_raw_value))+
41
 
  geom_point(alpha =0.3)+
42
 
  geom_smooth(method = lm)
43
 
44
 
#Smoking and low birthweight
45
 
smoking <- healthdata_subset |> 
46
 
  ggplot(aes(x= adult_smoking_raw_value, y=low_birthweight_raw_value))+
47
 
  geom_point(alpha =0.3)+
48
 
  geom_smooth(method = lm)
49
 
  #geom_violin(fill = "green")
50
 
51
 
#does not consider age...so I can't use this right?
52
 
food <- healthdata_subset |> 
53
 
  ggplot(aes(x= food_insecurity_raw_value, y=low_birthweight_raw_value))+
54
 
  geom_point(alpha =0.3)+
55
 
  geom_smooth(method = lm)
56
 
57
 
#Excessive drinking and low birthweight
58
 
#break down by gender
59
 
#alcohol frequency in county by gender 
60
 
61
 
drinking <- healthdata_subset |> 
62
 
  ggplot(aes(x=excessive_drinking_raw_value, y=low_birthweight_raw_value))+
63
 
  geom_point(alpha =0.3)  +
64
 
  geom_smooth(method = lm)
65
 
  #geom_violin(fill = "red")+
66
 
  #geom_boxplot()
67
 
68
 
#physical inactivity and low birthweight
69
 
inactivity <- healthdata_subset |> 
70
 
  ggplot(aes(x= physical_inactivity_raw_value, y=low_birthweight_raw_value))+
71
 
  geom_point(alpha =0.3)+
72
 
  geom_smooth(method = lm)
73
 
  #geom_violin(fill = "blue")
74
 
75
 
#insufficient sleep and low birthweight
76
 
sleep <- healthdata_subset |>
77
 
  ggplot(aes(x= insufficient_sleep_raw_value, y=low_birthweight_raw_value))+
78
 
  geom_point(alpha = 0.3) +
79
 
  geom_smooth(method = lm)
80
 
  #geom_violin(fill = "purple")
81
 
82
 
sti <- healthdata_subset |> 
83
 
  ggplot(aes(x=sexually_transmitted_infections_raw_value, y=low_birthweight_raw_value))+
84
 
  geom_point(alpha = 0.3) +
85
 
  geom_smooth(method = lm)
86
 
87
 
uninsured <- healthdata_subset |> 
88
 
  ggplot(aes(x=uninsured_raw_value, y=low_birthweight_raw_value))+
89
 
  geom_point(alpha = 0.3) +
90
 
  geom_smooth(method = lm)
91
 
92
 
library(cowplot)
93
 
plot_grid(smoking,drinking, inactivity, sleep, obesity, food, sti, uninsured)
94
 
95
 
96
 
97
 
98
 
# Lasso Regression --------------------------------------------------------
99
 
#Predicting low birthweight
100
 
library(glmnet)
101
 
model_predictors <- healthdata_subset |>
102
 
  select(adult_smoking_raw_value, adult_obesity_raw_value, 
103
 
         food_insecurity_raw_value, excessive_drinking_raw_value,
104
 
         physical_inactivity_raw_value,insufficient_sleep_raw_value, 
105
 
         sexually_transmitted_infections_raw_value, uninsured_raw_value) |> 
106
 
  as.matrix()
107
 
  
108
 
109
 
110
 
model_response_birthweight <- healthdata_subset |> 
111
 
  pull(low_birthweight_raw_value)
112
 
113
 
birthweight_lm <- lm(low_birthweight_raw_value ~ adult_smoking_raw_value + adult_obesity_raw_value + 
114
 
                       food_insecurity_raw_value + excessive_drinking_raw_value + physical_inactivity_raw_value +
115
 
                       insufficient_sleep_raw_value + sexually_transmitted_infections_raw_value + 
116
 
                       uninsured_raw_value, data = healthdata_subset)
117
 
118
 
#difference between predict and lm?
119
 
120
 
library(broom)
121
 
birthweight_lm |> 
122
 
  tidy() |> 
123
 
  mutate(term = fct_reorder(term, estimate)) |> 
124
 
  ggplot(aes(x = estimate, y = term, 
125
 
             fill = estimate > 0)) +
126
 
  geom_col(color = "white", show.legend = FALSE) +
127
 
  scale_fill_manual(values = c("darkred", "darkblue"))
128
 
129
 
birthweight_ridge <- glmnet(model_predictors, model_response_birthweight, alpha = 0)
130
 
plot(birthweight_ridge, xvar = "lambda")
131
 
132
 
133
 
birthweight_lasso_cv <- cv.glmnet(model_predictors, model_response_birthweight, alpha = 1)
134
 
135
 
tidy_lasso_coef <- tidy(birthweight_lasso_cv$glmnet.fit)
136
 
tidy_lasso_coef |> 
137
 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
138
 
  scale_x_log10() +
139
 
  geom_line(alpha = 0.75) +
140
 
  geom_vline(xintercept = birthweight_lasso_cv$lambda.min) +
141
 
  geom_vline(xintercept = birthweight_lasso_cv$lambda.1se, 
142
 
             linetype = "dashed", color = "red")
143
 
144
 
tidy_lasso_cv <- tidy(birthweight_lasso_cv)
145
 
tidy_lasso_cv |>
146
 
  ggplot(aes(x = lambda, y = nzero)) +
147
 
  geom_line() +
148
 
  geom_vline(xintercept = birthweight_lasso_cv$lambda.min) +
149
 
  geom_vline(xintercept = birthweight_lasso_cv$lambda.1se, 
150
 
             linetype = "dashed", color = "red") +
151
 
  scale_x_log10()
152
 
153
 
154
 
lasso_final <- glmnet(
155
 
  model_predictors, model_response_birthweight,
156
 
  alpha = 1,
157
 
  lambda = birthweight_lasso_cv$lambda.1se
158
 
)
159
 
library(vip)
160
 
lasso_final |> 
161
 
  vi() |> 
162
 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
163
 
  ggplot(aes(x = Importance, y = Variable, 
164
 
             fill = Importance > 0)) +
165
 
  geom_col(color = "white", show.legend = FALSE) +
166
 
  scale_fill_manual(values = c("darkred", "darkblue")) +
167
 
  labs(x = "estimate", y = NULL)
168
 
169
 
170


 


library(tidyverse)

healthdata <- read_csv("Data/analytic_data2024.csv") |> 

slice(-1)

clean_names()
6

View(healthdata)
7

tibble(healthdata)
8

#which adult-health related factors impact low birth weight 
9

10

# healthdata |> 
11

#   slice(-1) |> 
12

#   select(prematuredeath_num = "Premature Death numerator", prematuredeath_denom = "Premature Death denominator") |> 
13

#   mutate(prematuredeath_num = as.numeric(prematuredeath_num),
14

#          prematuredeath_denom = as.numeric(prematuredeath_denom),
15

#     prop_premature =  prematuredeath_num/prematuredeath_denom)
16

17

18

19

#Goal...How do different adult health-related practices predict low birth weight?->use lasso regression?
20

21

str(healthdata)
22

23

healthdata_subset <- healthdata |> 
  24

select(state_abbreviation, name, release_year, adult_smoking_raw_value, adult_obesity_raw_value, 
       25
       
       food_insecurity_raw_value, excessive_drinking_raw_value,
       26
       
       physical_inactivity_raw_value,insufficient_sleep_raw_value, low_birthweight_raw_value, 
       27
       
       percent_female_raw_value, low_birthweight_asian_pacific_islander, low_birthweight_black, 
       28
       
       low_birthweight_asian_pacific_islander, low_birthweight_white,low_birthweight_hispanic, 
       29
       
       sexually_transmitted_infections_raw_value, uninsured_raw_value
       30
       
) |> 
  31

mutate(across(ends_with('raw_value'), as.numeric)) |> 
  32

na.omit()
33

34

35

#healthdata_subset <- healthdata_subset[complete.cases(healthdata_subset),]
36

37

38

#Obesity and low birthweight
39

obesity <- healthdata_subset |> 
  40

ggplot(aes(x=adult_obesity_raw_value, y=low_birthweight_raw_value))+
  41

geom_point(alpha =0.3)+
  42

geom_smooth(method = lm)
43

44

#Smoking and low birthweight
45

smoking <- healthdata_subset |> 
  46

ggplot(aes(x= adult_smoking_raw_value, y=low_birthweight_raw_value))+
  47

geom_point(alpha =0.3)+
  48

geom_smooth(method = lm)
49

#geom_violin(fill = "green")
50

51

#does not consider age...so I can't use this right?
52

food <- healthdata_subset |> 
  53

ggplot(aes(x= food_insecurity_raw_value, y=low_birthweight_raw_value))+
  54

geom_point(alpha =0.3)+
  55

geom_smooth(method = lm)
56

57

#Excessive drinking and low birthweight
58

#break down by gender
59

#alcohol frequency in county by gender 
60

61

drinking <- healthdata_subset |> 
  62

ggplot(aes(x=excessive_drinking_raw_value, y=low_birthweight_raw_value))+
  63

geom_point(alpha =0.3)  +
  64

geom_smooth(method = lm)
65

#geom_violin(fill = "red")+
66

#geom_boxplot()
67

68

#physical inactivity and low birthweight
69

inactivity <- healthdata_subset |> 
  70

ggplot(aes(x= physical_inactivity_raw_value, y=low_birthweight_raw_value))+
  71

geom_point(alpha =0.3)+
  72

geom_smooth(method = lm)
73

#geom_violin(fill = "blue")
74

75

#insufficient sleep and low birthweight
76

sleep <- healthdata_subset |>
  77

ggplot(aes(x= insufficient_sleep_raw_value, y=low_birthweight_raw_value))+
  78

geom_point(alpha = 0.3) +
  79

geom_smooth(method = lm)
80

#geom_violin(fill = "purple")
81

82

sti <- healthdata_subset |> 
  83

ggplot(aes(x=sexually_transmitted_infections_raw_value, y=low_birthweight_raw_value))+
  84

geom_point(alpha = 0.3) +
  85

geom_smooth(method = lm)
86

87

uninsured <- healthdata_subset |> 
  88

ggplot(aes(x=uninsured_raw_value, y=low_birthweight_raw_value))+
  89

geom_point(alpha = 0.3) +
  90

geom_smooth(method = lm)
91

92

library(cowplot)
93

plot_grid(smoking,drinking, inactivity, sleep, obesity, food, sti, uninsured)
94

95

96

97

98

# Lasso Regression --------------------------------------------------------
99

#Predicting low birthweight
100

library(glmnet)
101

model_predictors <- healthdata_subset |>
  102

select(adult_smoking_raw_value, adult_obesity_raw_value, 
       103
       
       food_insecurity_raw_value, excessive_drinking_raw_value,
       104
       
       physical_inactivity_raw_value,insufficient_sleep_raw_value, 
       105
       
       sexually_transmitted_infections_raw_value, uninsured_raw_value) |> 
  106

as.matrix()
107


108

109

110

model_response_birthweight <- healthdata_subset |> 
  111

pull(low_birthweight_raw_value)
112

113

birthweight_lm <- lm(low_birthweight_raw_value ~ adult_smoking_raw_value + adult_obesity_raw_value + 
                       114
                     
                     food_insecurity_raw_value + excessive_drinking_raw_value + physical_inactivity_raw_value +
                       115
                     
                     insufficient_sleep_raw_value + sexually_transmitted_infections_raw_value + 
                       116
                     
                     uninsured_raw_value, data = healthdata_subset)
117

118

#difference between predict and lm?
119

120

library(broom)
121

birthweight_lm |> 
  122

tidy() |> 
  123

mutate(term = fct_reorder(term, estimate)) |> 
  124

ggplot(aes(x = estimate, y = term, 
           125
           
           fill = estimate > 0)) +
  126

geom_col(color = "white", show.legend = FALSE) +
  127

scale_fill_manual(values = c("darkred", "darkblue"))
128

129

birthweight_ridge <- glmnet(model_predictors, model_response_birthweight, alpha = 0)
130

plot(birthweight_ridge, xvar = "lambda")
131

132

133

birthweight_lasso_cv <- cv.glmnet(model_predictors, model_response_birthweight, alpha = 1)
134

135

tidy_lasso_coef <- tidy(birthweight_lasso_cv$glmnet.fit)
136

tidy_lasso_coef |> 
  137

ggplot(aes(x = lambda, y = estimate, group = term)) +
  138

scale_x_log10() +
  139

geom_line(alpha = 0.75) +
  140

geom_vline(xintercept = birthweight_lasso_cv$lambda.min) +
  141

geom_vline(xintercept = birthweight_lasso_cv$lambda.1se, 
           142
           
           linetype = "dashed", color = "red")
143

144

tidy_lasso_cv <- tidy(birthweight_lasso_cv)
145

tidy_lasso_cv |>
  146

ggplot(aes(x = lambda, y = nzero)) +
  147

geom_line() +
  148

geom_vline(xintercept = birthweight_lasso_cv$lambda.min) +
  149

geom_vline(xintercept = birthweight_lasso_cv$lambda.1se, 
           150
           
           linetype = "dashed", color = "red") +
  151

scale_x_log10()
152

153

154

lasso_final <- glmnet(
  155
  
  model_predictors, model_response_birthweight,
  156
  
  alpha = 1,
  157
  
  lambda = birthweight_lasso_cv$lambda.1se
  158
  
)
159

library(vip)
160

lasso_final |> 
  161

vi() |> 
  162

mutate(Variable = fct_reorder(Variable, Importance)) |>
  163

ggplot(aes(x = Importance, y = Variable, 
           164
           
           fill = Importance > 0)) +
  165

geom_col(color = "white", show.legend = FALSE) +
  166

scale_fill_manual(values = c("darkred", "darkblue")) +
  167

labs(x = "estimate", y = NULL)
168

