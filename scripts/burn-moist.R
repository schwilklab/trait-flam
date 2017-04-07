## burn-moist.R

# 1. Reads in data from burn trials
# 2. Analyses ignition probability (figure code in plots)
# 3. Analyses flammability and moisture content

# read data
burnt <- read.csv("../data/moisture/burn_moisture_trials_new.csv")
#burnt$spcode <- factor(burnt$spcode)

############################################################
## Relationships between moisture content and flammability
############################################################

modspread <- lm(spread ~ MC_dry + T_C + rh, data=burnt) # add spcode to get species responses
summary(modspread)

modt2ignit <- lm(t2ignit ~ MC_dry + T_C + rh, data=burnt) # add spcode to get species responses
summary(modt2ignit)

modsustain <- lm(sustain ~ MC_dry + T_C + rh, data=burnt) # add spcode to get species responses
summary(modsustain)

modcombust <- lm(combust ~ MC_dry + T_C + rh, data=burnt) # add spcode to get species responses
summary(modcombust)

modconsum <- lm(consum ~ MC_dry + T_C + rh, data=burnt) # add spcode to get species responses
summary(modconsum)

library(agricolae)

tuk <- HSD.test(modspread, "spcode", group=T)
tuk

tuk <- HSD.test(modt2ignit, "spcode", group=T)
tuk

tuk <- HSD.test(modsustain, "spcode", group=T)
tuk

tuk <- HSD.test(modcombust, "spcode", group=T)
tuk

tuk <- HSD.test(modconsum, "spcode", group=T)
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
