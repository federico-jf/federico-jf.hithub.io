## Federico Ferrero
## April 13, 2020
## Homework #5

## QUESTION 1:missing data
# clear environment 
rm(list=ls())

#libraries
library('Amelia') # for multiple imputation
library('zoo') # for linear interpolation
library('stargazer') # for tables
library('plotrix')
library('foreign')


# set working directory
setwd("C:/Users/feder/Desktop/SDAR/D'ORAZIO/Homework 5")

# see my working directory
getwd()

# reading in the data
load("burgoonsubset.RData")

# dimensions of dataset
dim(reasonabledata)

# detecting missingness and plot it
missd <- reasonabledata[0,]
for(i in 1:ncol(missd)) {
  missd[1,i] <- length(which(is.na(reasonabledata[,i])))
}
missd <- t(missd) #transpose
missmap(reasonabledata) #generate missmap

# listwise deletion (model 1)
fit_listwise_del <-lm(terrorinclead~ welfarelog + govleft + democ + poplog + govcap + conflict + tradelog + terrorinc, data=reasonabledata)
stargazer(fit_listwise_del, type="text", title = "Listwise_deletion",
          dep.var.labels=c("terrorinclead"), 
          out="table1.txt")

# variable mean imputations
reasonabledata$wl_vmi <- reasonabledata$welfarelog
reasonabledata$wl_vmi[which(is.na(reasonabledata$welfarelog))] <- mean(reasonabledata$welfarelog, na.rm=TRUE)

reasonabledata$popl_vmi <- reasonabledata$poplog
reasonabledata$popl_vmi[which(is.na(reasonabledata$poplog))] <- mean(reasonabledata$poplog, na.rm=TRUE)

reasonabledata$democ_vmi <- reasonabledata$democ
reasonabledata$democ_vmi[which(is.na(reasonabledata$democ))] <- mean(reasonabledata$democ, na.rm=TRUE)

reasonabledata$govcap_vmi <- reasonabledata$govcap
reasonabledata$govcap_vmi[which(is.na(reasonabledata$govcap))] <- mean(reasonabledata$govcap, na.rm=TRUE)

reasonabledata$govleft_vmi <- reasonabledata$govleft
reasonabledata$govleft_vmi[which(is.na(reasonabledata$govleft))] <- mean(reasonabledata$govleft, na.rm=TRUE)

reasonabledata$tradelog_vmi <- reasonabledata$tradelog
reasonabledata$tradelog_vmi[which(is.na(reasonabledata$tradelog))] <- mean(reasonabledata$tradelog, na.rm=TRUE)

# variable mean imputation (model 2)
fit_vmi <-lm(terrorinclead~ wl_vmi + govleft_vmi + democ_vmi + popl_vmi + govcap_vmi + conflict + tradelog_vmi + terrorinc, data=reasonabledata)
stargazer(fit_vmi, type="text", title = "Variable mean imputation",
          dep.var.labels=c("terrorinclead"), 
          out="table2.txt")

# country mean imputations
meanvals <- aggregate(reasonabledata$welfarelog, by=list(reasonabledata$cow), FUN="mean", na.rm=TRUE, na.action=NULL)
colnames(meanvals) <- c("cow","wl_cmi")
reasonabledata <- merge(x=reasonabledata,y=meanvals,all.x=TRUE,by="cow")
reasonabledata$wl_cmi[which(!is.na(reasonabledata$welfarelog))] <- reasonabledata$welfarelog[which(!is.na(reasonabledata$welfarelog))]

meanvals2 <- aggregate(reasonabledata$poplog, by=list(reasonabledata$cow), FUN="mean", na.rm=TRUE, na.action=NULL)
colnames(meanvals2) <- c("cow","popl_cmi")
reasonabledata <- merge(x=reasonabledata,y=meanvals2,all.x=TRUE,by="cow")
reasonabledata$popl_cmi[which(!is.na(reasonabledata$poplog))] <- reasonabledata$poplog[which(!is.na(reasonabledata$poplog))]

meanvals3 <- aggregate(reasonabledata$democ, by=list(reasonabledata$cow), FUN="mean", na.rm=TRUE, na.action=NULL)
colnames(meanvals3) <- c("cow","democ_cmi") 
reasonabledata <- merge(x=reasonabledata,y=meanvals3,all.x=TRUE,by="cow")
reasonabledata$democ_cmi[which(!is.na(reasonabledata$democ))] <- reasonabledata$democ[which(!is.na(reasonabledata$democ))]

meanvals4 <- aggregate(reasonabledata$govcap, by=list(reasonabledata$cow), FUN="mean", na.rm=TRUE, na.action=NULL)
colnames(meanvals4) <- c("cow","govcap_cmi") 
reasonabledata <- merge(x=reasonabledata,y=meanvals4,all.x=TRUE,by="cow")
reasonabledata$govcap_cmi[which(!is.na(reasonabledata$govcap))] <- reasonabledata$govcap[which(!is.na(reasonabledata$govcap))]

meanvals5 <- aggregate(reasonabledata$govleft, by=list(reasonabledata$cow), FUN="mean", na.rm=TRUE, na.action=NULL)
colnames(meanvals5) <- c("cow","govleft_cmi")
reasonabledata <- merge(x=reasonabledata,y=meanvals5,all.x=TRUE,by="cow")
reasonabledata$govleft_cmi[which(!is.na(reasonabledata$govleft))] <- reasonabledata$govleft[which(!is.na(reasonabledata$govleft))]

meanvals6 <- aggregate(reasonabledata$tradelog, by=list(reasonabledata$cow), FUN="mean", na.rm=TRUE, na.action=NULL)
colnames(meanvals6) <- c("cow","tradelog_cmi")
reasonabledata <- merge(x=reasonabledata,y=meanvals6,all.x=TRUE,by="cow")
reasonabledata$tradelog_cmi[which(!is.na(reasonabledata$tradelog))] <- reasonabledata$tradelog[which(!is.na(reasonabledata$tradelog))]

# country mean imputations (model 3)
fit_cmi <-lm(terrorinclead~ wl_cmi + govleft_cmi + democ_cmi + popl_cmi + govcap_cmi + conflict + tradelog_cmi + terrorinc, data=reasonabledata)
stargazer(fit_cmi, type="text", title = "Country mean imputation",
          dep.var.labels=c("terrorinclead"), 
          out="table3.txt")

# linear interpolation using zoo for welfarelog
reasonabledata$wl_li <- 0
states <- unique(reasonabledata$cow)
for(i in 1:length(states)) {
  temp <- reasonabledata[which(reasonabledata$cow==states[i]),c("cow","year","welfarelog")]
  if(nrow(temp)==length(which(is.na(temp[,3])))) {
    cat("State ", temp[1,1], " is completely missing.")
    next
  }
  zoodata <- zoo(temp)
  zoodata$wl_li <- na.approx(zoodata$welfarelog)
  reasonabledata[which(reasonabledata$cow==states[i]),"wl_li"] <- zoodata$wl_li
}

# linear interpolation for poplog
reasonabledata$popl_li <- 0
states <- unique(reasonabledata$cow)
for(i in 1:length(states)) {
  temp <- reasonabledata[which(reasonabledata$cow==states[i]),c("cow","year","poplog")]
  if(nrow(temp)==length(which(is.na(temp[,3])))) {
    cat("State ", temp[1,1], " is completely missing.")
    next
  }
  zoodata <- zoo(temp)
  zoodata$popl_li <- na.approx(zoodata$poplog)
  reasonabledata[which(reasonabledata$cow==states[i]),"popl_li"] <- zoodata$popl_li
}

# linear interpolation for democ
reasonabledata$democ_li <- 0
states <- unique(reasonabledata$cow)
for(i in 1:length(states)) {
  temp <- reasonabledata[which(reasonabledata$cow==states[i]),c("cow","year","democ")]
  if(nrow(temp)==length(which(is.na(temp[,3])))) {
    cat("State ", temp[1,1], " is completely missing.")
    next
  }
  zoodata <- zoo(temp)
  zoodata$democ_li <- na.approx(zoodata$democ)
  reasonabledata[which(reasonabledata$cow==states[i]),"democ_li"] <- zoodata$democ_li
}

# linear interpolation for govcap
reasonabledata$govcap_li <- 0
states <- unique(reasonabledata$cow)
for(i in 1:length(states)) {
  temp <- reasonabledata[which(reasonabledata$cow==states[i]),c("cow","year","govcap")]
  if(nrow(temp)==length(which(is.na(temp[,3])))) {
    cat("State ", temp[1,1], " is completely missing.")
    next
  }
  zoodata <- zoo(temp)
  zoodata$govcap_li <- na.approx(zoodata$govcap)
  reasonabledata[which(reasonabledata$cow==states[i]),"govcap_li"] <- zoodata$govcap_li
}

# linear interpolation govleft
reasonabledata$govleft_li <- 0
states <- unique(reasonabledata$cow)
for(i in 1:length(states)) {
  temp <- reasonabledata[which(reasonabledata$cow==states[i]),c("cow","year","govleft")]
  if(nrow(temp)==length(which(is.na(temp[,3])))) {
    cat("State ", temp[1,1], " is completely missing.")
    next
  }
  zoodata <- zoo(temp)
  zoodata$govleft_li <- na.approx(zoodata$govleft)
  reasonabledata[which(reasonabledata$cow==states[i]),"govleft_li"] <- zoodata$govleft_li
}

# linear interpolation tradelog
reasonabledata$tradelog_li <- 0
states <- unique(reasonabledata$cow)
for(i in 1:length(states)) {
  temp <- reasonabledata[which(reasonabledata$cow==states[i]),c("cow","year","tradelog")]
  if(nrow(temp)==length(which(is.na(temp[,3])))) {
    cat("State ", temp[1,1], " is completely missing.")
    next
  }
  zoodata <- zoo(temp)
  zoodata$tradelog_li <- na.approx(zoodata$tradelog)
  reasonabledata[which(reasonabledata$cow==states[i]),"tradelog_li"] <- zoodata$tradelog_li
}

# linear interpolation (model 4)
fit_li <-lm(terrorinclead~ wl_li + govleft_li + democ_li + popl_li + govcap_li + conflict + tradelog_li + terrorinc, data=reasonabledata)
stargazer(fit_li, type="text", title = "Linear interpolation",
          dep.var.labels=c("terrorinclead"), 
          out="table4.txt")

# multiple imputation using Amelia
m<-10
a.out<-amelia(reasonabledata,m=m,cs="cow",ts="year",idvars=c("wl_vmi", "wl_cmi", "wl_li",
                                                             "popl_vmi", "popl_cmi", "popl_li",
                                                             "democ_vmi", "democ_cmi", "democ_li",
                                                             "govcap_vmi", "govcap_cmi", "govcap_li",
                                                             "govleft_vmi", "govleft_cmi", "govleft_li",
                                                             "tradelog_vmi", "tradelog_cmi", "tradelog_li"), outname="p3ny", lead=c("welfarelog"))

# check and pulling out 1 of the m datasets
a1 <- a.out$imputations[1]
a1 <- as.data.frame(a1)

# COMBINE THE RESULTS FROM 10 DIFFERENT IMPUTATIONS DATASETS 
# NOT an amelia function: just calling the lm() function for each imputation of the data, and the returns from each of those calls are put into a list
fits.mi <- lapply(a.out$imputations, function(i) lm(terrorinclead~ welfarelog + govleft + democ + poplog + govcap + conflict + tradelog + terrorinc, data=i))
# NOT an amelia function: extracting coefficients from each of those fit lms (stored in the fits.mi list), and rbind-ing them together
coefs.amelia <- do.call(rbind, lapply(fits.mi, function(i) coef(summary(i))[,1]))
# NOT an amelia function: same as above for standard errors
ses.amelia <- do.call(rbind, lapply(fits.mi, function(i) coef(summary(i))[,2]))
# this is an amelia function to combine estimates
results <- mi.meld(coefs.amelia, ses.amelia)
# to see the results a little more clearly
t(do.call(rbind, results))

# table for multiple imputations
stargazer(fits.mi, type="text", title = "Multiple imputations",
          dep.var.labels="terrorinclead", 
          out="table5.txt")

stargazer(t(do.call(rbind, results)), type="text", title = "Multiple imputations",
          dep.var.labels="terrorinclead", 
          out="table5summary.txt")

# aggregation to 5 year chunks 
reasonabledata$fiveyear<- 0
reasonabledata$fiveyear[which(reasonabledata$year<1999)] <- "1995-1999"
reasonabledata$fiveyear[which(reasonabledata$year<=1994)] <- "1990-1994"
reasonabledata$fiveyear[which(reasonabledata$year<=1989)] <- "1985-1989"
reasonabledata$fiveyear[which(reasonabledata$year<=1984)] <- "1980-1984"
reasonabledata$fiveyear[which(reasonabledata$year<=1979)] <- "1975-1979"

agg1<-data.frame(aggregate(terrorinclead~cow+fiveyear, reasonabledata, mean))
agg2<-data.frame(aggregate(welfarelog~cow+fiveyear, reasonabledata, mean))
agg3<-data.frame(aggregate(poplog~cow+fiveyear, reasonabledata, mean))
agg4<-data.frame(aggregate(democ~cow+fiveyear, reasonabledata, mean))
agg5<-data.frame(aggregate(govcap~cow+fiveyear, reasonabledata, mean))
agg6<-data.frame(aggregate(conflict~cow+fiveyear, reasonabledata, mean))
agg7<-data.frame(aggregate(govleft~cow+fiveyear, reasonabledata, mean))
agg8<-data.frame(aggregate(tradelog~cow+fiveyear, reasonabledata, mean))
agg9<-data.frame(aggregate(terrorinc~cow+fiveyear, reasonabledata, mean))

dataset1<-data.frame(merge(agg1, agg2))
dataset2<-data.frame(merge(dataset1, agg3))
dataset3<-data.frame(merge(dataset2, agg4))
dataset4<-data.frame(merge(dataset3, agg5))
dataset5<-data.frame(merge(dataset4, agg6))
dataset6<-data.frame(merge(dataset5, agg7))
dataset7<-data.frame(merge(dataset6, agg8))
dataset_agg_model<-data.frame(merge(dataset7, agg9))

fit_fiveyear <-lm(terrorinclead~ welfarelog + govleft + democ + poplog + govcap + conflict + tradelog + terrorinc, data=dataset_agg_model)
nobs(fit_fiveyear)

# table for 5 year chunks
stargazer(fit_fiveyear, type="text", title = "5 year chunks",
          dep.var.labels=c("terrorinclead"), 
          out="table6.txt")

#Construct table with all the 5 linear models
stargazer(list(fit_listwise_del, fit_vmi, fit_cmi, fit_li, fit_fiveyear),
          title = "Linear models using different methods for dealing with missing data", 
          covariate.labels = c('Total Welfare', 'Left Government', 'Democracy', 'Population', 'Government Capacity', 'Conflict', 'Trade Openness', 'Incidents'),
          out="table7.txt")

# Question 2:Extra credit
reasonabledata$terrorinc_dummy <- 0
reasonabledata$terrorinc_dummy[which(reasonabledata$terrorinclead>0)] <- 1

mydata <- data.frame(reasonabledata$terrorinc_dummy, reasonabledata$welfarelog, 
                     reasonabledata$govleft, reasonabledata$democ, 
                     reasonabledata$poplog, reasonabledata$govcap, reasonabledata$conflict,
                     reasonabledata$tradelog, reasonabledata$terrorinc)

mydata <-na.omit(mydata)
dim(mydata)
# linear probability model
fit <- lm (reasonabledata.terrorinc_dummy ~ reasonabledata.welfarelog + reasonabledata.govleft 
           + reasonabledata.democ + reasonabledata.poplog + reasonabledata.govcap + 
             reasonabledata.conflict + reasonabledata.tradelog + reasonabledata.terrorinc, data=mydata)

stargazer(fit,title = "Linear probability model", 
          covariate.labels = c('Total Welfare', 'Left Government', 'Democracy', 'Population', 'Government Capacity', 'Conflict', 'Trade Openness', 'Incidents'),
          out="table8.txt")
# LPM predicted probabilities on in-sample data
predprobs <- predict(fit, se.fit=TRUE)

upperbound <- predprobs$fit + (1.96*predprobs$se.fit)
lowerbound <- predprobs$fit - (1.96*predprobs$se.fit)

plotdata <- cbind(as.data.frame(predprobs), upperbound, lowerbound)

plotCI(mydata$reasonabledata.welfarelog, plotdata$fit, ui=plotdata$upperbound, li=plotdata$lowerbound, pch=20, 
       xlab="Welfare spending (welfarelog)", ylab="Pred (terror incidents)", main="LPM with Burgoon Data")
       
abline(h=1, lwd=2,col="red")
abline(h=0, lwd=2,col="red")

# lPM predicted probabilities on out-of-sample data

newdata<-data.frame(
  reasonabledata.govleft=mean(mydata$reasonabledata.govleft), 
  reasonabledata.democ=mean(mydata$reasonabledata.democ), 
  reasonabledata.poplog=mean(mydata$reasonabledata.poplog), 
  reasonabledata.govcap=mean(mydata$reasonabledata.govcap),
  reasonabledata.conflict=mean(mydata$reasonabledata.conflict), 
  reasonabledata.tradelog=mean(mydata$reasonabledata.tradelog),
  reasonabledata.terrorinc=mean(mydata$reasonabledata.terrorinc),
  reasonabledata.welfarelog=seq(from=0, to=100, length=100))

out.predprobs <- predict(fit, se.fit=TRUE, newdata=newdata)

upperbound <- out.predprobs$fit + (1.96*out.predprobs$se.fit)
lowerbound <- out.predprobs$fit - (1.96*out.predprobs$se.fit)

plotdata <- cbind(as.data.frame(out.predprobs), upperbound, lowerbound)

plotCI(newdata$reasonabledata.welfarelog, plotdata$fit, ui=plotdata$upperbound, li=plotdata$lowerbound, pch=20, 
       xlab="Welfare spending (welfarelog)", ylab="Pred (terror incidents)", main="Out of Sample Likelihoods")

# logistic regression
fit <- glm (reasonabledata.terrorinc_dummy ~ reasonabledata.welfarelog + reasonabledata.govleft 
           + reasonabledata.democ + reasonabledata.poplog + reasonabledata.govcap + 
             reasonabledata.conflict + reasonabledata.tradelog + reasonabledata.terrorinc, family=binomial, data=mydata)


stargazer(fit, type="text", title = "Logistic",
          dep.var.labels=c("Terror incident"), 
          out="table9.txt")

# calculate the odds ratios
fit.coefficients
exp(coefficients(fit))

## predicted probabilities on in-sample data
predprobs <- predict.glm(fit, se.fit=TRUE)

upperbound <- predprobs$fit + (1.96*predprobs$se.fit)
lowerbound <- predprobs$fit - (1.96*predprobs$se.fit)

plotdata <- cbind(as.data.frame(predprobs), upperbound, lowerbound)
plotdata<-data.frame(lapply(plotdata,binomial(link="logit")$linkinv))

plotCI(mydata$reasonabledata.welfarelog, plotdata$fit, ui=plotdata$upperbound, li=plotdata$lowerbound, pch=20, 
       xlab="Welfare spending (welfarelog)", ylab="Pred (terror incidents)", main="Logit with Burgoon Data")

abline(h=1, lwd=2,col="red")
abline(h=0, lwd=2,col="red")

## Logistic regression. Predicted probabilities on out-of-sample data
newdata<-data.frame(
  reasonabledata.govleft=mean(mydata$reasonabledata.govleft), 
  reasonabledata.democ=mean(mydata$reasonabledata.democ), 
  reasonabledata.poplog=mean(mydata$reasonabledata.poplog), 
  reasonabledata.govcap=mean(mydata$reasonabledata.govcap),
  reasonabledata.conflict=mean(mydata$reasonabledata.conflict), 
  reasonabledata.tradelog=mean(mydata$reasonabledata.tradelog),
  reasonabledata.terrorinc=mean(mydata$reasonabledata.terrorinc),
  reasonabledata.welfarelog=seq(from=0, to=100, length=100))

out.predprobs <- predict(fit, se.fit=TRUE, newdata=newdata)
upperbound <- out.predprobs$fit + (1.96*out.predprobs$se.fit)
lowerbound <- out.predprobs$fit - (1.96*out.predprobs$se.fit)

plotdata <- cbind(as.data.frame(out.predprobs), upperbound, lowerbound)
plotdata<-data.frame(lapply(plotdata,binomial(link="logit")$linkinv))

plotCI(newdata$reasonabledata.welfarelog, plotdata$fit, ui=plotdata$upperbound, li=plotdata$lowerbound, pch=20, 
       xlab="Welfare spending (welfarelog)", ylab="Pred (terror incidents)", main="Out of Sample Likelihoods")



