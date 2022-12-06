library(tidyverse)
library(caret)
library(leaps)
library(MASS)
res.lm <- lm(Fertility ~., data = swiss)
step <- stepAIC(res.lm, direction = 'both', trace = FALSE)
step
set.seed(123)
train.control <- trainControl(method= 'cv', number = 10)
#train the model
step.model <- train(Fertility ~., data = swiss, method = 'lmStepAIC', trControl = train.control, trace =
                         FALSE)
# MODEL ACCURACY
step.model$results
#final model coefficients
step.model$finalModel
#summary of the model
summary(step.model$finalModel)
#######################################
library('car')

data1 = Prestige

reg1 <- lm(prestige ~ education + log2(income) + women, data = Prestige)

summary(reg1)

## Dummy regression with no interactions

reg2 <- lm(prestige ~ education + log2(income) +
                type, data = Prestige)

summary(reg2)

# Reordering factor variables
Prestige$type <- with(Prestige, factor(type,
                                          levels=c('bc', 'wc', 'prof')))

## Dummy regression with interactions

reg3 <- lm(prestige ~ type*(education +
                                 log2(income)), data = Prestige)
summary(reg3)

# Other ways to run the same model
reg3a <- lm(prestige ~ education + log2(income) +
                 type + log2(income):type + education:type,
               data = Prestige)
reg3b <- lm(prestige ~ education*type +
                 log2(income)*type, data = Prestige)

## Diagnostics for linear regression (residual plots)

library(car)
reg1 <- lm(prestige ~ education + income + type,
              data = Prestige)
residualPlots(reg1)

# Using ‘income’ as is.
# Variable ‘income’ shows some patterns.
# Other options:

residualPlots(reg1, ~ 1, fitted=TRUE) #Residuals vs fitted only

residualPlots(reg1, ~ education, fitted=FALSE) # Residuals vs education only

