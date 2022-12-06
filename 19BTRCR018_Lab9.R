library(psych)
fit <- principal(mtcars, nfactors=5, rotate='varimax')
fit # print results

# Maximum Likelihood Factor Analysis
# entering raw data and extracting 3 factors,
# with varimax rotation

fit <- factanal(mtcars, 3, rotation='varimax')
print(fit, digits=2, cutoff=.3, sort=TRUE)

# plot factor 1 by factor 2
load <- fit$loadings[,1:2]
plot(load,type='n') # set up plot
text(load,labels=names(mtcars),cex=.7) # add variable names

# Principal Axis Factor Analysis
library(psych)
fit <- factor.pa(mtcars, nfactors=3, rotation='varimax')
fit # print results

# Determine Number of Factors to Extract

library(nFactors)

ev <- eigen(cor(mtcars)) # get eigenvalues

ap <- parallel(subject=nrow(mtcars),var=ncol(mtcars),
                  rep=100,cent=.05)

nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)

plotnScree(nS)

