# read-moistmix.R

# 1. Reads in the data files for the moisture content and the flamambility of the mixtures,
#    both observed and predicted
# 2. Creates summary statistics
# 3. Investigate species differences in dry down intercepts and rates
# 4. Investigate non-additivity

library(lme4)
library(plyr)
library (tidyr)
library(dplyr) #must come after plyr
library(stringr)

#############################################################
# MOISTURE
#############################################################
source("dry-down.R") # for dry.mod

# Read in mixtures moisture data
mmc <- read.csv("../data/moisture/dry_down_long_mix.csv")

mmc$mixcode <- mmc$spcode

mmc <- mmc %>% mutate(mixcode = str_replace(mixcode, "Ab", "Abco"),
                      mixcode = str_replace(mixcode, "Ca", "Cade"),
                      mixcode = str_replace(mixcode, "Pi", "Pije"),
                      mixcode = str_replace(mixcode, "Qu", "Quke"),
                      sp1 = str_sub(mixcode, 1,4),
                      sp2 = str_sub(mixcode, 5,8),
                      sp3 = str_sub(mixcode, 9,12))


mmc <- mmc %>% mutate(MC_dry_pred1 = exp(predict(dry.mod, allow.new.levels=TRUE,
                                                newdata=data.frame(spcode=sp1,
                                                tray = str_c(sp1,"_",rep,"NEW"),
                                                hour = hour))),
                      MC_dry_pred2 = exp(predict(dry.mod, allow.new.levels=TRUE,
                                                newdata=data.frame(spcode=sp2,
                                                tray = str_c(sp2,"_",rep,"NEW"),
                                                hour = hour))),
                      MC_dry_pred3 = exp(predict(dry.mod, allow.new.levels=TRUE,
                                                newdata=data.frame(spcode=sp3,
                                                tray = str_c(sp3,"_",rep,"NEW"),
                                                hour = hour))),
                      MC_dry_pred = (MC_dry_pred1 + MC_dry_pred2 + MC_dry_pred3)/3
                      )

# end of DWS new code

mmc$res_MC_dry <- mmc$MC_dry - mmc$MC_dry_pred

mmc.sum <- mmc %>% group_by(spcode, hour) %>%
		summarise(MC_dry.mean = mean(MC_dry), 
		          MC_dry.sd = sd(MC_dry),
		          res_MC_dry.mean = mean(res_MC_dry), 
		          res_MC_dry.sd = sd(res_MC_dry),
		          bd.mean = mean(bd), 
		          bd.sd = sd(bd)
		          )

mmcpred <-mmc[, c("spcode", "hour", "MC_dry_pred")] 

mmcpred.sum <- mmcpred %>% group_by(spcode, hour) %>% sample_n(1)

mmc.sum <- mmc.sum %>% left_join(mmcpred.sum, by=c("spcode", "hour"))

###############################################################################
## Investigate species differences in dry down intercepts and rates
###############################################################################

mmc$logMC_dry <-  log(mmc$MC_dry)
mmc <- mmc %>% mutate(tray = str_c(spcode, "_", rep))

# Fit a nested model using lmer
mdry.mod <- lmer(log(MC_dry) ~ hour*spcode + (1 + hour | tray), data=mmc)
summary(mdry.mod)
anova(mdry.mod)

# comparing the above model with one with bulk density added. Second model is better
mdrybd.mod <- lmer(log(MC_dry) ~ hour*spcode + bd + (1 + hour | tray), data=mmc)
summary(mdrybd.mod)
anova(mdry.mod, mdrybd.mod)

# test for pairwise differences and number of distinct groups
library(lsmeans)
cld(lstrends(mdry.mod,~ spcode, var = "hour"))
cld(lsmeans(mdry.mod, ~ spcode))

# subset by species to get the coefficients (y0 and B) for each curve.
coefuncm <- function(d){
  mod <- lmer(log(MC_dry)~ hour + (1 + hour | tray ), data=d)
  res <- summary(mod)$coefficients
  return(data.frame(logmaxMC = res[1,1], logmaxMC.se = res[1,2],  di= res[2,1], di.se = res[2,2]))
}

mmcdis <- mmc %>% group_by(spcode) %>% do(coefuncm(.)) %>% mutate(maxMC = exp(logmaxMC), maxMC.se=exp(logmaxMC.se))


###############################################################################
#FLAMMABILITY
##############
# Read in mixtures flammability data
mflam <- read.csv("../data/moisture/burn_moisture_trials_mix.csv")

library(plantecophys)

mflam$vpd <- RHtoVPD(mflam$rh, mflam$T_C)

mflam$mixcode <- mflam$spcode

mflam <- mflam %>% mutate(mixcode = str_replace(mixcode, "Ab", "Abco"),
                      mixcode = str_replace(mixcode, "Ca", "Cade"),
                      mixcode = str_replace(mixcode, "Pi", "Pije"),
                      mixcode = str_replace(mixcode, "Qu", "Quke"),
                      sp1 = str_sub(mixcode, 1,4),
                      sp2 = str_sub(mixcode, 5,8),
                      sp3 = str_sub(mixcode, 9,12))

source("burn-moist.R")
modspreadsp <- lm(spread ~ actualMC_dry*spcode, data=burnt)
summary(modspreadsp)
modignitsp <- lm(t2ignit ~ actualMC_dry*spcode, data=burnt)

mflam <- mflam %>% mutate(spread_pred1 = predict(modspreadsp, allow.new.levels=TRUE,
                                                 newdata=data.frame(spcode=sp1,
                                                                    actualMC_dry = actualMC_dry)),
                      spread_pred2 = predict(modspreadsp, allow.new.levels=TRUE,
                                                 newdata=data.frame(spcode=sp2,
                                                                    actualMC_dry = actualMC_dry)),
                      spread_pred3 = predict(modspreadsp, allow.new.levels=TRUE,
                                                 newdata=data.frame(spcode=sp3,
                                                                    actualMC_dry = actualMC_dry)),
                      spread_pred = (spread_pred1 + spread_pred2 + spread_pred3)/3)

mflam <- mflam %>% mutate(ignit_pred1 = predict(modignitsp, allow.new.levels=TRUE,
                                                 newdata=data.frame(spcode=sp1,
                                                                    actualMC_dry = actualMC_dry)),
                          ignit_pred2 = predict(modignitsp, allow.new.levels=TRUE,
                                                 newdata=data.frame(spcode=sp2,
                                                                    actualMC_dry = actualMC_dry)),
                          ignit_pred3 = predict(modignitsp, allow.new.levels=TRUE,
                                                 newdata=data.frame(spcode=sp3,
                                                                    actualMC_dry = actualMC_dry)),
                          ignit_pred = (ignit_pred1 + ignit_pred2 + ignit_pred3)/3)

mflam$res_spread <- mflam$spread - mflam$spread_pred
mflam$res_ignit <- mflam$t2ignit - mflam$ignit_pred

mflam.sum <- mflam %>% group_by(spcode, hour) %>%
  summarise(t2ignit.mean = mean(t2ignit),
            t2ignit.sd = sd(t2ignit),
            spread.mean = mean(spread),
            spread.sd = sd(spread),
            actualMC_dry.mean = mean(actualMC_dry), 
            actualMC_dry.sd = sd(actualMC_dry) 
              )

mflampred <-mflam[, c("spcode", "hour", "spread_pred", "ignit_pred")] 

mflampred.sum <- mflampred %>% group_by(spcode, hour) %>% sample_n(1)

mflam.sum <- mflam.sum %>% left_join(mflampred.sum, by=c("spcode", "hour"))

###############################################################################
## Investigate non-additivity
###############################################################################

# Residual analysis on moisture
res.mod <- lmer(res_MC_dry ~ (1|spcode), data=mmc)
summary(res.mod)

res2.mod <- lmer(res_MC_dry ~ hour + (1|spcode), data=mmc)
summary(res2.mod)

anova(res.mod, res2.mod)

# Residual analysis on flammability (spread rate and time to ignition)
resspread.mod <- lmer(res_spread ~ (1|spcode), data=mflam)
summary(resspread.mod)

resspread2.mod <- lmer(res_spread ~ hour + (1|spcode), data=mflam)
summary(resspread2.mod)

anova(resspread.mod, resspread2.mod)

resignit.mod <- lmer(res_ignit ~ (1|spcode), data=mflam)
summary(resignit.mod)

resignit2.mod <- lmer(res_ignit ~ hour + (1|spcode), data=mflam)
summary(resignit2.mod)

anova(resignit.mod, resignit2.mod)
