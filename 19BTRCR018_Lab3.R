library(MASS)
#data(package="MASS")
boston<-Boston
dim(boston)
names(boston)
#LOAD RANDOM FOREST PACKAGE
require(randomForest)
#SET SEED AND CREATE A SAMPLE TRAINING SET OF 300 OBSERVATIONS
set.seed(101)
train=sample(1:nrow(boston),300)
#LETS FIT THE RANDOM FOREST AND SEE HOW IT WORKS
rf.boston=randomForest(medv~.,data=boston,subset=train)
rf.boston
#LETS STORE THE MEAN SQUARE ERROR ON THE OBJECT (OUT OF BAG ERROR)
oob.err=double(13)
# TEST ERROR: MEAN SQUARE ERROR WHICH IS EQUALS TO MEAN((medv-pred)^2)
test.err=double(13)
for(mtry in 1:13)
{
  fit=randomForest(medv~.,data=boston,subset=train,mtry=mtry,ntree=350)
  oob.err[mtry]=fit$mse[350]
  pred=predict(fit,boston[-train,])
  test.err[mtry]=with(boston[-train,], mean((medv-pred)^2))
}
#lets plot the
matplot(1:mtry, cbind(test.err,oob.err),pch=23,col=c("red","blue"),type="b",ylab="meansquared
        Error")
legend("topright",legend=c("OOB","Test"),pch=23,col=c("red","blue"))



data("Boston", package = "MASS")
# Split the data into training and test set
set.seed(123)
training.samples <- Boston$medv %>% createDataPartition(p = 0.8, list = FALSE)
train.data  <- Boston[training.samples, ]
test.data <- Boston[-training.samples, ]
model1 <- lm(medv ~., data = train.data)
# Make predictions
predictions <- model1 %>% predict(test.data)
# Model performance
data.frame(
  RMSE = RMSE(predictions, test.data$medv),
  R2 = R2(predictions, test.data$medv)
)
car::vif(model1)
