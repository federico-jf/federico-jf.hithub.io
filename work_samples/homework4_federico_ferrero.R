## Federico Ferrero
## April 1, 2020
## Homework #4: Multicollinearity and missing data


# clear environment
rm(list=ls())

# set working directory
setwd("C:/Users/feder/Desktop/SDAR/D'ORAZIO/Homework 4")

# see my working directory
getwd()

# reading in the data
library(haven)
mydata <- read_dta("/Users/feder/Desktop/SDAR/D'ORAZIO/Homework 4/hw4data2.dta")

# dimensions of dataset
dim(mydata)

# linear regression model
fit<- lm(exptorev ~ lnpop + lngnp + lnimports + lnexports + democracy, data=mydata)
summary(fit)

# create a table with the fit outputs
library('stargazer')
stargazer(fit, type="text", title = "Linear model outputs: exptorev ~ lnpop + lngnp + lnimports + lnexports + democracy",
          dep.var.labels=c("Measure of Deficit"),
          out="table1.txt")

# coefficient plot
library(ggplot2)
coefplot::coefplot(fit, title="Coefficient plot: Measure of Deficit (dependent variable)",
                   ylab="Predictors")

#scatterplot matrix:correlations
library(GGally)
mydata2 <- read.delim("/Users/feder/Desktop/SDAR/D'ORAZIO/Homework 4/hw4data2_predictors.txt")
ggpairs(mydata2)

# showing the vifs and tolerance of predictors
library(car)
vifs <-vif(fit)
tolerance <- (1/vifs)

stargazer(vifs, tolerance,
          type="text", title = "VIF and Tolerance for predictors",
          out="table2.txt")

# orthogonalize
library(matlib)
Y <- mydata[,c("exptorev")]
X <- mydata2
Z <- cbind(X[,1], 0, 0, 0, 0)
Z[,2] <- X[,2] - Proj(X[,2], Z[,1])
Z[,3] <- X[,3] - Proj(X[,3], Z[,1]) - Proj(X[,3], Z[,2])
Z[,4] <- X[,4] - Proj(X[,4], Z[,1]) - Proj(X[,4], Z[,2]) - Proj(X[,4], Z[,3])
Z[,5] <- X[,5] - Proj(X[,5], Z[,1]) - Proj(X[,5], Z[,2]) - Proj(X[,5], Z[,3]) - Proj(X[,5], Z[,4])
mydata3 <-cbind (Y, Z)
fit.orthog<- lm(exptorev ~ ., data=mydata3)
summary(fit.orthog)

stargazer(list(fit, fit.orthog),
          title = "Base model (1) and Orthogonalized variables (2)",
          out="table4.txt")


                                                                                                                                                                                           fit.orthog <- lm(Y ~ Z)
                                                                                                                                                                                                                            stargazer(fit, fit.orthog)
