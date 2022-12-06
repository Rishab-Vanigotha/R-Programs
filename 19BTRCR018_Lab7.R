library(tidyverse)
library(caret)
library(leaps)
library(MASS)
#fit the full model
full.model <- lm(Fertility ~., data = swiss)
dim(swiss)

#stepwise regression model
step.model <- stepAIC(full.model, direction = 'both', trace = TRUE)
summary(step.model)
models <- regsubsets(Fertility ~., data = swiss, nvmax = 5, method = 'seqrep')
summary(models)
#set seed for reproducability
set.seed(123)

#setup repeated k-fold cross validation
train.control <- trainControl(method = 'cv', number = 10)
step.model <- train(Fertility ~., data = swiss, method = 'leapBackward', tuneGrid = data.frame(nvmax =
                                                                                                              1:5), trControl = train.control)
step.model
step.model$results
step.model$bestTune
