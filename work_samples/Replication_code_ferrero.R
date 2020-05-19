## Federico Ferrero
## April 29, 2020
## Final Paper  

# clear environment 
rm(list=ls())

# set working directory
setwd("C:/Users/feder/Desktop/SDAR/D'ORAZIO/paper_regression_analysis")

# see my working directory
getwd()

# reading in the data
mydata <- read.delim("/Users/feder/Desktop/SDAR/D'ORAZIO/paper_regression_analysis/Main_data_replication.txt")

# dimensions of dataset
dim(mydata)

# REPLICATION TABLE 1: 4 linear regression models
model1 <- lm(gem_women_share ~ fair_dic, data = mydata)
model2 <- lm(rot_fem_share_z ~ fair_dic, data = mydata[mydata$rot_nearest_neighbor == 0, ])
model3 <- lm(rot_adel_int1_z ~ fair_dic, data = mydata[mydata$rot_nearest_neighbor == 0, ])
model4 <- lm(rot_uradel_int_z ~ fair_dic, data = mydata[mydata$rot_nearest_neighbor == 0, ])

summary(model1)
summary(model2)
summary(model3)
summary(model4)

# create a table with the models outputs
library('stargazer')
stargazer(list(model1, model2, model3, model4),
          title = "Equitable Inheritance and Inequality", 
          covariate.labels = 'Equitable Inheritance', 
          out="table1.txt")

# REPLICATION TABLE 2: 4 linear regression models (controls included)
model1_2 <- lm(gem_women_share ~ fair_dic + childlabor_mean_1898 +  
                 support_expenses_total_capita + gem_council + gem_pop_density +  
                 pop_tot + factor(law_cat), data=mydata)

model2_2 <- lm(rot_fem_share_z ~ fair_dic+ childlabor_mean_1898 +  
                 support_expenses_total_capita + gem_pop_density + pop_tot +
                 factor(law_cat), data=mydata[mydata$rot_nearest_neighbor == 0, ])

model3_2 <- lm(rot_adel_int1_z ~ fair_dic+ childlabor_mean_1898 +  
                 support_expenses_total_capita + gem_pop_density + pop_tot +
                 factor(law_cat), data=mydata[mydata$rot_nearest_neighbor == 0, ])

model4_2 <- lm(rot_uradel_int_z ~ fair_dic+ childlabor_mean_1898 +  
                 support_expenses_total_capita + gem_pop_density + pop_tot +
                 factor(law_cat), data=mydata[mydata$rot_nearest_neighbor == 0, ])

summary(model1_2)
summary(model2_2)
summary(model3_2)
summary(model4_2)

# create a table with the models outputs
stargazer(list(model1_2, model2_2, model3_2, model4_2),
          title = "Equitable Inheritance and Inequality (Controls Included)", 
          covariate.labels = c('Equitable inheritance', 'Child labor', 'Welfare expenditure',
                               'Council size', 'Population density', 'Total population',
                               'Code civil', 'Common law',
                               'Danish law', 'Prussian land law'),
          out="table2.txt")

# EXTENSION

# coefficient plot for table 2
library(ggplot2)
coefplot::coefplot(model1_2, title="Coefficient plot",
                   ylab="Predictors")
coefplot::coefplot(model2_2, title="Coefficient plot",
                   ylab="Predictors")
coefplot::coefplot(model3_2, title="Coefficient plot",
                   ylab="Predictors")
coefplot::coefplot(model4_2, title="Coefficient plot",
                   ylab="Predictors")


# Formulate 2 models to see impact on income and unemployment
model8 <- lm(suminc31_gini ~ fair_dic, data=mydata)
summary(model8)

model9 <- lm(ue_longterm ~ fair_dic, data=mydata)
summary(model9)

# table
stargazer(list(model8, model9),
          title = "Impact of equitable inheritance on unemployment and GINI of income (2014)", 
          keep = 'fair_dic',
          covariate.labels = 'Equitable inheritance', 
          out="table3.txt")

# Test interaction
fit22<- lm (gem_women_share ~ fair_dic + support_expenses_total_capita 
            + fair_dic*support_expenses_total_capita , data = mydata)
summary(fit22)
fit33<- lm (gem_women_share ~ fair_dic + welfare_recip_capita 
            + fair_dic*welfare_recip_capita , data = mydata)
summary(fit33)

# table
stargazer(list(fit22, fit33),
          title = " titulo", 
          covariate.labels = c('Equitable inheritance', 'Welfare spending on poor 1890 (per capita)', 'Equitable Inheritance * Welfare spending on poor 1890 (per capita)',
                               'Welfare recipients 2014 (per capita)', 'Equitable Inheritance * Welfare recipients 2014 (per capita)'),
          out="table4.txt")

# marginal effects plot
library(interplot)
interplot(m = fit33, var1 = "fair_dic", var2 = "welfare_recip_capita", hist = TRUE) +
  aes(color = "pink") + theme(legend.position="none") +   geom_line(color = "pink") + 
  geom_hline(yintercept = 0, linetype = "dashed") + xlab("Welfare (2014)") + 
  ylab("Marginal Effect of Equitable Inheritance") + labs(title="Marginal Effect of Equitable Inheritance on Women's political participation by Welfare expenditure (2014)") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        title=element_text(size=14))

# marginal effects plot
interplot(m = fit22, var1 = "fair_dic", var2 = "support_expenses_total_capita", hist = TRUE) +
  aes(color = "pink") + theme(legend.position="none") +   geom_line(color = "pink") + 
  geom_hline(yintercept = 0, linetype = "dashed") + xlab("Welfare (1890)") + 
  ylab("Marginal Effect of Equitable Inheritance") + labs(title="Marginal Effect of Equitable Inheritance on on Women's political participation by Welfare expenditure (1890)") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        title=element_text(size=14))

# scatterplot matrix: correlations
library(ggplot2)
library(GGally)
mydata2 <- read.delim("/Users/feder/Desktop/SDAR/D'ORAZIO/paper_regression_analysis/Main_data_replication_predictors.txt")

ggpairs(mydata2)

# showing the vifs and tolerance of predictors
library(car)
vifs <-vif(model1_2)
tolerance <- (1/vifs)
tolerance
stargazer(vifs, tolerance, 
          type="text", title = "VIF and Tolerance for predictors model1_2", 
          out="table5.txt")

vifs <-vif(model2_2)
tolerance <- (1/vifs)

stargazer(vifs, tolerance, 
          type="text", title = "VIF and Tolerance for predictors model2_2", 
          out="table6.txt")

# detecting missingness and plot it
missd <- mydata[0,]
for(i in 1:ncol(missd)) {
  missd[1,i] <- length(which(is.na(mydata[,i])))
}
missd <- t(missd) #transpose
library('Amelia') 
missmap(mydata) #generate missmap

# variable mean imputations

mydata$rot_adel_int1_z_vmi <- mydata$rot_adel_int1_z
mydata$rot_adel_int1_z_vmi[which(is.na(mydata$rot_adel_int1_z))] <- mean(mydata$rot_adel_int1_z, na.rm=TRUE)

mydata$rot_uradel_int_z_vmi <- mydata$rot_uradel_int_z
mydata$rot_uradel_int_z_vmi[which(is.na(mydata$rot_uradel_int_z))] <- mean(mydata$rot_uradel_int_z, na.rm=TRUE)

mydata$gem_women_share_vmi <- mydata$gem_women_share
mydata$gem_women_share_vmi[which(is.na(mydata$gem_women_share))] <- mean(mydata$gem_women_share, na.rm=TRUE)

mydata$gem_council_vmi <- mydata$gem_council
mydata$gem_council_vmi[which(is.na(mydata$gem_council))] <- mean(mydata$gem_council, na.rm=TRUE)

mydata$gem_pop_density_vmi <- mydata$gem_pop_density
mydata$gem_pop_density_vmi[which(is.na(mydata$gem_pop_density))] <- mean(mydata$gem_pop_density, na.rm=TRUE)

mydata$pop_tot_vmi <- mydata$pop_tot
mydata$pop_tot_vmi[which(is.na(mydata$pop_tot))] <- mean(mydata$pop_tot, na.rm=TRUE)

mydata$childlabor_mean_1898_vmi <- mydata$childlabor_mean_1898
mydata$childlabor_mean_1898_vmi[which(is.na(mydata$childlabor_mean_1898))] <- mean(mydata$childlabor_mean_1898, na.rm=TRUE)

mydata$welfare_recip_capita_vmi <- mydata$welfare_recip_capita
mydata$welfare_recip_capita_vmi[which(is.na(mydata$welfare_recip_capita))] <- mean(mydata$welfare_recip_capita, na.rm=TRUE)

mydata$support_expenses_total_capita_vmi <- mydata$support_expenses_total_capita
mydata$support_expenses_total_capita_vmi[which(is.na(mydata$support_expenses_total_capita))] <- mean(mydata$support_expenses_total_capita, na.rm=TRUE)

#run models with mean imputation
model1_2_imp <- lm(gem_women_share_vmi ~ fair_dic + childlabor_mean_1898_vmi +  
                 support_expenses_total_capita_vmi + gem_council_vmi + gem_pop_density_vmi +  
                 pop_tot_vmi + factor(law_cat), data=mydata)

model2_2_imp <- lm(rot_fem_share_z ~ fair_dic+ childlabor_mean_1898_vmi +  
                 support_expenses_total_capita_vmi + gem_pop_density_vmi + pop_tot_vmi +
                 factor(law_cat), data=mydata[mydata$rot_nearest_neighbor == 0, ])

model3_2_imp <- lm(rot_adel_int1_z_vmi ~ fair_dic+ childlabor_mean_1898_vmi +  
                 support_expenses_total_capita_vmi + gem_pop_density_vmi + pop_tot_vmi +
                 factor(law_cat), data=mydata[mydata$rot_nearest_neighbor == 0, ])

model4_2_imp <- lm(rot_uradel_int_z_vmi ~ fair_dic+ childlabor_mean_1898_vmi +  
                 support_expenses_total_capita_vmi + gem_pop_density_vmi + pop_tot_vmi +
                 factor(law_cat), data=mydata[mydata$rot_nearest_neighbor == 0, ])

model4_2_imp
# create a table with the models outputs having done mean imputation
stargazer(list(model1_2_imp, model2_2_imp, model3_2_imp, model4_2_imp),
          title = "Equitable Inheritance and Inequality (Controls Included)", 
          covariate.labels = c('Equitable inheritance', 'Child labor', 'Welfare expenditure',
                               'Council size', 'Population density', 'Total population',
                               'Code civil', 'Common law',
                               'Danish law', 'Prussian land law'),
          out="table7.txt")

