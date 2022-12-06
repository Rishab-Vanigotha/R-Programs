dim(mtcars)
names(mtcars)
mtcars.pca <- prcomp(mtcars[,c(1:7,10,11)], center = TRUE, scale. = TRUE)
summary(mtcars.pca)

#####second####
#lets dispaly pca object details
str(mtcars.pca)

#Include devtools, which is used for visualization
library(devtools)
library(ggplot2)

plot(mtcars.pca,)
plot(mtcars.pca,labels=rownames(mtcars))

###third####

#additing a new car to mtcars, to add a new car lets creae a new car and add new car and mycars
#to the new dataset

#creating a new car,adding a new row to mtcar dataset
spacecar <- c(1000,60,50,500,0,0.5,2.5,0,1,0,0)

#create a new data set with mtcars and space car
mtcarsplus <- rbind(mtcars, spacecar)
mtcars.countryplus <- c(mtcarsplus.country, "Jupiter")

#finding the principal components of mtcarplus

mtcarsplus.pca <- prcomp(mtcarsplus[,c(1:7,10,11)], center = TRUE,scale. = TRUE)

plot(mtcarsplus.pca)
