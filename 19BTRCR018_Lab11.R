library(readxl)
library(httr)

url1<-'https://github.com/Corvu-s/R-machine-learning-/blob/master/ageandheight.xls'
GET(url1, write_disk(tf <- tempfile(fileext = ".xls")))

ageandheight <- read_excel(path = tf)
ageandheight <- data.frame(ageandheight)
#Upload the data
lmHeight = lm(height~age, data = ageandheight)
#Create the linear regression
summary(lmHeight)
res.ftest <- var.test(lm(height~age), lm(), data = ageandheight)
res.ftest
#Review the results
# sum of the squares residuals
x <- c(1,2,3,4,5)
y <- c(10,20,30,40,50)
t_line=lm(y ~ x)
residuals(t_line)
residuals(t_line)^2
sum(residuals(t_line)^2)
              
