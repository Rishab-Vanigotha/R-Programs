#1. For the given data, analyze the missing values and comment on imputing or
#dropping the missing values. Explain how the missing values will affect the model
#estimation and its goodness of fit.
#2. Influence of variance in large data set with many variables. Higher the variance of
#the variable will contribute the higher importance to the data set. Find the variance
#of method to reduce the dimension of the data set. Give justification of output.
#3. Dimensionality reduction using PCA (Principal Component Analysis) and Feature
#Selection through OMP (Orthogonal Matching Pursuit)

x<-c(1:4,NA,6:7,NA)
print(x)
is.na(x)

#data frame with missing values

df<-data.frame(col1=c(10:12,NA),
                  col2=c("I",NA,"am","fine"),
                  col3=c(TRUE,FALSE,TRUE,FALSE),
                  col4=c(1.2,2.3,NA,4.3),
                  stringsAsFactors=FALSE)
print(df)
is.na(df)
#identify the location of N's in the data frame
which(is.na(df))

#display the sum of NA's in vector
sum(is.na(x))

#display the sum of NA's in the data frame
sum(is.na(df))

#display the count of NA's in the each column of data frame
colSums(is.na(df))

#in two ways missing values are coded i.e., NA and 99
#recode missing values with mean
#before recode x is
x
mean(x)
mean(x,na.rm=TRUE)
x[is.na(x)]<-mean(x,na.rm=TRUE)

#after recode x is
x
round(x,2)

# data frame with missing values coded as 99
df1<-data.frame(col1=c(1:3,99),col2=c(2.8,4.7,99,4.2))
df1

#change 99 to NA
df1[df1==99]<-NA
df1

#Exclude missing values
x1<-c(1:3,NA,4:5)
x1

#display mean of x1

mean(x1)

#mean of x1 after excluding NA
mean(x1,na.rm=TRUE)

#DATA FRAME WITH MISSING VALUES
df

#list of complete rows of the data frame
complete.cases(df)

#subset with complete.cases to get complete cases
df[complete.cases(df),]

#subset with ! to get incomplete cases
df[!complete.cases(df),]

#short hand way to get the complete cases of data frame by omitting
na.omit(df)


library("factoextra")
data(decathlon2)

decathlon2.active <- decathlon2[1:23, 1:10]
head(decathlon2.active[, 1:6])

## Compute PCA
res.pca <- prcomp(decathlon2.active, scale = TRUE)

## Visualize eigenvalues (scree plot). Show the percentage of variances explained by
#each principal component.
fviz_eig(res.pca)
# Graph of individuals. Individuals with a similar profile are grouped together.
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                repel = TRUE # Avoid text overlapping
             )
             ## Graph of variables. Positive correlated variables point to the same side of the plot.
            ## Negative correlated variables point to opposite sides of the graph.
             fviz_pca_var(res.pca,
                          col.var = "contrib", # Color by contributions to the PC
                          gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                            repel = TRUE # Avoid text overlapping
                          )
                          
                          ## Biplot of individuals and variables
                          fviz_pca_biplot(res.pca, repel = TRUE,
                                          col.var = "#2E9FDF", # Variables color
                                          col.ind = "#696969" # Individuals color
                          )
