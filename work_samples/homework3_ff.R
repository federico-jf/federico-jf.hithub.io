## Federico Ferrero
## March 10, 2020
## Homework # 3: Dummy variables and interactions

##QUESTION 1:
# clear environment 
rm(list=ls())

# set working directory
setwd("C:/Users/feder/Desktop/SDAR/D'ORAZIO/Homework 3")

# see my working directory
getwd()

# reading in the data
mydata <- read.delim("/Users/feder/Desktop/SDAR/D'ORAZIO/RABE_All_Data/P151.txt")

# dimensions of dataset
dim(mydata)

# creation of dummy variables
mydata$Northeast <- 0
mydata$Northeast[which(mydata$Region==1)] <- 1

mydata$North_central <- 0
mydata$North_central[which(mydata$Region==2)] <- 1

mydata$South <- 0
mydata$South[which(mydata$Region==3)] <- 1

mydata$West <- 0
mydata$West[which(mydata$Region==4)] <- 1

# linear regression model
fit<- lm(Y ~ Northeast + North_central + South, data=mydata)
summary(fit)

# create a table with the fit outputs
library('stargazer')
stargazer(fit, type="text", title = "Linear model outputs: Expenditure in public education ~ Regions",
          dep.var.labels=c("Per capita expenditure in public education"), 
          out="table1.txt")

# coefficient plot
library(ggplot2)
coefplot::coefplot(fit, title="Coefficient plot: Public Education Expenditure ~ Regions",
                   ylab="Predictors")

# linear model with interaction 
fit2<- lm(Y ~ X1 + South + X1*South, data=mydata)
summary(fit2)

# marginal effects plot
library(interplot)
interplot(m = fit2, var1 = "X1", var2 = "South", hist = TRUE) +
  aes(color = "pink") + theme(legend.position="none") +   geom_line(color = "pink") + 
  geom_hline(yintercept = 0, linetype = "dashed") + xlab("Southern states") + 
  ylab("Marginal Effect of Personal Income (X1)") + labs(title="Marginal Effect of Personal Income on Public Education Expenditures by Southern states", subtitle="Y ~ X1 + South + X1*South") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        title=element_text(size=14))

##QUESTION 2:
# clear environment 
rm(list=ls())

# set working directory
setwd("C:/Users/feder/Desktop/SDAR/D'ORAZIO/Homework 3")

# see my working directory
getwd()

# reading in the data
mydata <- read.delim("/Users/feder/Desktop/SDAR/D'ORAZIO/RABE_All_Data/P160.txt")

# dimensions of dataset
dim(mydata)

# recode I to ve 0 if I ==-1
I_dummy <- mydata$I
mydata$I_dummy<-1 
mydata$I_dummy[which(mydata$I =='-1')] <- 0

# regression model
fit<- lm(V ~ I_dummy + D + W + G + G*I_dummy, data=mydata)
summary(fit)

# create a table with the fit outputs
library('stargazer')
stargazer(fit, type="text", title = "Linear model outputs: Presidential Election Data",
          dep.var.labels=c("Democratic share of the two-party presidential vote"), 
          out="table2.txt")

#sum of squared errors
SSE <- sum((fit$residuals)^2)

# marginal effects plot 
library(interplot)
interplot(m = fit, var1 = "I_dummy", var2 = "G", hist = TRUE) +
  aes(color = "pink") + theme(legend.position="none") +   geom_line(color = "pink") + 
  geom_hline(yintercept = 0, linetype = "dashed") + xlab("Grow rate of per capita GDP") + 
  ylab("Marginal Effect of Incumbency (I)") + labs(title="Marginal Effect of Democratic incumbency on Democratic share at Presidential election", subtitle="V ~ I + D + W + G + G*I") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        title=element_text(size=12))


