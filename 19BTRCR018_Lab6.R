library(TH.data)
library(caret)
data("GlaucomaM", package = "TH.data")
trainData <- GlaucomaM
#View(trainData)
dim(trainData)
head(trainData)
str(trainData)
set.seed(100)
options(warn = -1)

subsets <- c(1:5, 10, 15, 18)

ctrl <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 5,
                      verbose = FALSE)

lmProfile <- rfe(x = trainData[, c(1:3, 5:13)], y=trainData$Class,
                    sizes = subsets,
                    rfeControl = ctrl)

lmProfile

input<-iris
names(input)
str(input)

model = prcomp(input[,1:4], scale=TRUE)
model$sdev
model$rotation
model$center
model$scale

par(mfrow=c(2,2))
plot(model$x[,1], col=input[,5])
plot(model$x[,2], col=input[,5])
plot(model$x[,3], col=input[,5])
plot(model$x[,4], col=input[,5])

model$sdev^2 / sum(model$sdev^2)
plot(model)

## PCA without &#39;prcomp&#39;
## Normalize the input feature.
input$sepal_len1 = (input$Sepal.Length - mean(input$Sepal.Length) )/sd(input$Sepal.Length)
input$sepal_wid1 = (input$Sepal.Width - mean(input$Sepal.Width))/sd(input$Sepal.Width)
input$petal_len1 = (input$Petal.Length - mean(input$Petal.Length))/sd(input$Petal.Length)
input$petal_wid1 = (input$Petal.Width - mean(input$Petal.Width))/sd(input$Petal.Width)

##Get the covarience matrix and eigen vector.
matrix_form = matrix(c(input$sepal_len1, input$sepal_wid1, input$petal_len1, input$petal_wid1),
                     ncol=4)
m = cov(matrix_form)
eigenV = eigen(m)
eigenV$vectors
