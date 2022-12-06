library(tidyverse)

#use caret package for machine learning workflow
library(caret)

theme_set(theme_bw())
set.seed(123)
data('marketing')
training.samples <- marketing$sales %>% createDataPartition(p = 0.8, list = FALSE)
train.data <- marketing[training.samples,]
test.data <- marketing[-training.samples,]

model <- lm(sales ~., data = train.data)
summary(model)

predictions <- model %>% predict(test.data)
RMSE(predictions, test.data$sales)
R2(predictions, test.data$sales)
