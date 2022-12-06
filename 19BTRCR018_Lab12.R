x <- c(10,20,30,40,50)
y <- c(20,24,40,45,90)
res.ftest <- var.test(y,x)
res.ftest

# Null hypothesis is true ,
# Ratio of variance is equal to 1
#p value is low, it should be less than the significance level
# Fail to reject the null hypothesis
# We can assume the variables x and y have equal variance

# R script to test homoscedsticity

lmMod <- lm(dist ~ speed, data=cars)

par(mfrow=c(2,2))
plot(lmMod)

