## burn-moist.R

# 1. Reads in data from burn trials
# 2. Analyses ignition probability (figure code in plots)
# 3. Analyses flammability and moisture content

library(dplyr)
# read data
burnt <- read.csv("../data/moisture/burn_moisture_trials_new.csv")
#burnt$spcode <- factor(burnt$spcode)
source("read-flam.R") # for species table
burnt <- left_join(burnt, species)

burnt$t2ignit[burnt$t2ignit==0] <- 180 #solves the error in ignit plot (had a zero)

# Adding VPD to the data

library(plantecophys)

burnt$vpd <- RHtoVPD(burnt$rh, burnt$T_C)


############################################################
## Relationships between time since wetting and flammability
############################################################

modspread <- lm(spread ~ hour + genus + genus:hour + vpd, data=burnt) 
summary(modspread)
anova(modspread)

modspread2 <- lm(spread ~ actualMC_dry*hour*genus + vpd, data=filter(burnt, genus!="Calocedrus" & genus!="Sequoiadendron")) 
summary(modspread2)
anova(modspread2)

modt2ignit <- lm(t2ignit ~ hour*genus + vpd, data=filter(burnt, genus!="Calocedrus" & genus!="Sequoiadendron")) 
summary(modt2ignit)
anova(modt2ignit)
#modspread <- lm(spread ~ actualMC_dry*genus + vpd, data=burnt) 
#summary(modspread)

#modt2ignit <- lm(t2ignit ~ actualMC_dry*genus + vpd, data=burnt) 
#summary(modt2ignit)


library(agricolae)

tuk <- HSD.test(modspread, "genus", group=TRUE)
tuk

tuk <- HSD.test(modt2ignit, "genus", group=TRUE)
tuk

##################################
## Binomial analysis of ignition
##################################

fit <- glm(ignit~MC_dry + spcode + rh + T_C, data=burnt, family=binomial())
summary(fit)

### Obtaining the inflection point
p <- 0.5
x <- (log(p/(1-p)) - coef(fit)[1]) / coef(fit)[2]
x
