#1. Use decision tree for dimension reduction of the given data set and find the classification
#techniques with efficiency.
#2. Use Random Forest for dimension reduction of the given data set and find the
#classification techniques with efficiency.
#3. Use LDA (Linear Discriminant Analysis) for solving any multi class classification
#problem.
sink('./output 2.txt',append =T)
Corrmatrix <- structure(c(1, 0.82, 0.54, 0.36, 0.85, 0.82, 1, 0.01, 0.74, 0.36,
                             0.54, 0.01, 1, 0.65, 0.91, 0.36, 0.74, 0.65, 1, 0.36,
                             0.85, 0.36, 0.91, 0.36, 1),
                           .Dim = c(5L, 5L))
Corrmatrix
library(caret)
findCorrelation(Corrmatrix, cutoff = .6, verbose = TRUE, names = F)

## R Code : Removing Redundant Variables

# load required libraries

library(corrplot)
library(plyr)

# load required dataset
dat <- read.csv("C:\\Users\\DELL\\Documents\\7th sem\\DRMV_R\\pml-training.csv")

# Set seed
set.seed(227)

# Remove variables having high missing percentage (50%)
dat1 <- dat[, colMeans(is.na(dat)) <= .5]
dim(dat1)

# Remove Zero and Near Zero-Variance Predictors
nzv <- nearZeroVar(dat1)
print(nzv)
dat2 <- dat1[, -nzv]
dim(dat2)

# Identifying numeric variables
numericData <- dat2[sapply(dat2, is.numeric)]

# Calculate correlation matrix
descrCor <- cor(numericData)

# Print correlation matrix and look at max correlation
print(descrCor)
summary(descrCor[upper.tri(descrCor)])

# Check Correlation Plot
corrplot(descrCor, order = "FPC", method = "color", type = "lower", tl.cex = 0.7, tl.col = rgb(0,0,0))
                                                                                                                             

# find attributes that are highly corrected
highlyCorrelated <- findCorrelation(descrCor, cutoff=0.7)

# print indexes of highly correlated attributes
print(highlyCorrelated)

# Indentifying Variable Names of Highly Correlated Variables
highlyCorCol <- colnames(numericData)[highlyCorrelated]

# Print highly correlated attributes
highlyCorCol

# Remove highly correlated variables and create a new dataset
dat3 <- dat2[, -which(colnames(dat2) %in% highlyCorCol)]
dim(dat3)

## R Code : Feature Selection with Random Forest

library(randomForest)

#Train Random Forest
rf <-randomForest(as.factor(classe)~., data=dat3, importance=TRUE, ntree=100)

#Evaluate variable importance
imp = importance(rf, type=1)
imp <- data.frame(predictors=rownames(imp),imp)

# Order the predictor levels by importance
imp.sort <- arrange(imp,desc(MeanDecreaseAccuracy))
imp.sort$predictors <- factor(imp.sort$predictors,levels=imp.sort$predictors)

# Select the top 20 predictors
imp.20 <- imp.sort[1:20,]
print(imp.20)

# Plot Important Variables
varImpPlot(rf, type=1)

# Subset data with 20 independent and 1 dependent variables
dat4 = cbind(classe = dat3$classe, dat3[,c(imp.20$predictors)])
dim(dat4)
colnames(dat4)

## Implementation of LDA

# Load Library

library(klaR)
library(psych)
library(MASS)
library(ggord)
library(devtools)

## Getting Data
data("iris")
str(iris)

pairs.panels(iris[1:4],
             gap = 0,
             bg = c("red", "green", "blue")[iris$Species],
             pch = 21)

## Data Partition

set.seed(123)
ind <- sample(2, nrow(iris),
                 
                 replace = TRUE,
                 prob = c(0.6, 0.4))
training <- iris[ind==1,]
testing <- iris[ind==2,]

## Linear discriminant analysis
linear <- lda(Species~., training)
linear

attributes(linear)

## Histogram
## Stacked histogram for discriminant function values.

p <- predict(linear, training)
ldahist(data = p$x[,1], g = training$Species)

ldahist(data = p$x[,2], g = training$Species)

## Partition plot
# It provides the classification of each and every combination in the training dataset.

partimat(Species~., data = training, method = "lda")

## Confusion matrix and accuracy – training data
p1 <- predict(linear, training)$class
tab <- table(Predicted = p1, Actual = training$Species)
tab

## Confusion matrix and accuracy – testing data
p2 <- predict(linear, testing)$class
tab1 <- table(Predicted = p2, Actual = testing$Species)
tab1
sink()
