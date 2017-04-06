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

############################################################
## Relationships between moisture content and flammability
############################################################

modspread <- lm(spread ~ actualMC_dry + T_C + rh + genus, data=burnt) 
summary(modspread)

modt2ignit <- lm(t2ignit ~ actualMC_dry + T_C + rh+ genus, data=burnt) 
summary(modt2ignit)

modsustain <- lm(sustain ~ actualMC_dry + T_C + rh+ genus, data=burnt) 
summary(modsustain)

modcombust <- lm(combust ~ actualMC_dry + T_C + rh+ genus, data=burnt)
summary(modcombust)

modconsum <- lm(consum ~ actualMC_dry + T_C + rh+ genus, data=burnt) 
summary(modconsum)

library(agricolae)

tuk <- HSD.test(modspread, "genus", group=T)
tuk

tuk <- HSD.test(modt2ignit, "genus", group=T)
tuk

tuk <- HSD.test(modsustain, "genus", group=T)
tuk

tuk <- HSD.test(modcombust, "genus", group=T)
tuk

tuk <- HSD.test(modconsum, "genus", group=T)
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
