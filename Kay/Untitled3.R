healthdata_subset2 <- healthdata_subset[,c('juvenile_arrests_raw_value', 'excessive_drinking_raw_value', 'poor_mental_health_days_raw_value')]
healthdata_subset2 <- na.omit(healthdata_subset2)
healthdata_subset2
cor(healthdata_subset2)
corrplot(cor(healthdata_subset2))
