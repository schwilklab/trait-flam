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

# Read in mixtures flammability data
mflam <- read.csv("../data/moisture/burn_moisture_trials_mix.csv")


# read in moisture dry down data from single species trials
# Read in mixtures moisture data
mmc <- read.csv("../data/moisture/dry_down_long_mix.csv")
#pred_mmc <- read.csv("../data/moisture/pred_MCdry_mix.csv")


mmc$mixcode <- mmc$spcode

mmc <- mmc %>% mutate(mixcode = str_replace(mixcode, "Ab", "Abco"),
                      mixcode = str_replace(mixcode, "Ca", "Cade"),
                      mixcode = str_replace(mixcode, "Pi", "Pije"),
                      mixcode = str_replace(mixcode, "Qu", "Quke"),
                      sp1 = str_sub(mixcode, 1,4),
                      sp2 = str_sub(mixcode, 5,8),
                      sp3 = str_sub(mixcode, 9,12))


temppredict <- predict(dry.mod, allow.new.levels=TRUE,
                       newdata=data.frame(spcode=mmc$sp1,
                                          tray = str_c(mmc$sp1, "_", mmc$rep, "new"),
                                          hour = mmc$hour))

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

mmc.sum <- mmc %>% group_by(spcode, hour) %>%
		summarise(MC_dry.mean=mean(MC_dry), MC_dry.sd = sd(MC_dry))

obs_pred_mc <- merge(pred_mmc, mmc, by=c("hour", "spcode", "type"))

obs_pred_mc$res_MCdry <- obs_pred_mc$MC_dry - obs_pred_mc$predMC_dry

obs_pred_mc.sum <- obs_pred_mc %>% group_by(hour, spcode) %>%
                                  summarise(predMC_dry.mean = mean(predMC_dry),
                                            predMC_dry.sd = sd(predMC_dry),
                                            MC_dry.mean = mean(MC_dry),
                                            MC_dry.sd = sd(MC_dry),
                                            res_MCdry.mean = mean(res_MCdry),
                                            res_MCdry.sd = sd(res_MCdry))


pred_mflam <- read.csv("../data/moisture/pred_flam_mix2.csv", na.strings = c("","NA"),
                       stringsAsFactors=FALSE)

obs_pred_flam <- merge(pred_mflam, mflam, by=c("hour", "spcode"))

obs_pred_flam.sum <- obs_pred_flam %>% 
  group_by(hour, spcode) %>% 
  summarize(pred_spread.mean = mean(pred_spread),
            pred_spread.sd = sd(pred_spread),
            pred_ignit.mean = mean(pred_ignit, na.rm=TRUE),
            pred_ignit.sd = sd(pred_ignit, na.rm=TRUE),
            spread.mean = mean(spread),
            spread.sd = sd(spread),
            ignit.mean = mean(t2ignit),
            ignit.sd = sd(t2ignit))

## Creating the residuals for non-additivity

obs_pred_flam.res <- obs_pred_flam[, c(1,2)]
obs_pred_flam.res$spread.res <- obs_pred_flam$spread - obs_pred_flam$pred_spread
obs_pred_flam.res$ignit.res <- obs_pred_flam$t2ignit - obs_pred_flam$pred_ignit
obs_pred_flam.res$sustain.res <- obs_pred_flam$sustain - obs_pred_flam$pred_sustain
obs_pred_flam.res$combust.res <- obs_pred_flam$combust - obs_pred_flam$pred_combust
obs_pred_flam.res$consum.res <- obs_pred_flam$consum - obs_pred_flam$pred_consum

obs_pred_flam.res_sum <- obs_pred_flam.res %>% group_by(hour, spcode) %>%
                                                summarise_each(funs(mean(., na.rm = TRUE), sd(., na.rm=TRUE)))

###############################################################################
## Investigate species differences in dry down intercepts and rates
###############################################################################

mmc$logMC_dry <-  log(mmc$MC_dry)

# Fit a nested model using lmer
mdry.mod <- lm(MC_dry ~ hour*spcode, data=mmc)
summary(mdry.mod)

# test for pairwise differences and number of distinct groups
library(lsmeans)
cld(lstrends(mdry.mod,~ spcode, var = "hour"))
cld(lsmeans(mdry.mod, ~ spcode))

# subset by species to get the coefficients (y0 and B) for each curve.
coefuncm <- function(mmc){
  mod <- lm(MC_dry~hour, data=mmc)
  res <- coef(mod)
  return(data.frame(maxMC = res[1], di= res[2]))
}

mmcdis <- mmc %>% group_by(spcode) %>% do(coefuncm(.)) 


###############################################################################
## Investigate non-additivity
###############################################################################

# Residual analysis on moisture
res.mod <- lmer(res_MCdry ~ (1|spcode), data=obs_pred_mc)
summary(res.mod)

res2.mod <- lmer(res_MCdry ~ hour + (1|spcode), data=obs_pred_mc)
summary(res2.mod)

anova(res.mod, res2.mod)

# Residual analysis on flammability (spread rate and time to ignition)
resspread.mod <- lmer(spread.res ~ (1|spcode), data=obs_pred_flam.res)
summary(resspread.mod)

resspread2.mod <- lmer(spread.res ~ hour + (1|spcode), data=obs_pred_flam.res)
summary(resspread2.mod)

anova(resspread.mod, resspread2.mod)

resignit.mod <- lmer(ignit.res ~ (1|spcode), data=obs_pred_flam.res)
summary(resignit.mod)

resignit2.mod <- lmer(ignit.res ~ hour + (1|spcode), data=obs_pred_flam.res)
summary(resignit2.mod)

anova(resignit.mod, resignit2.mod)

#################################################################################
# Testing how different from 0 the residual zscores are
#(done for all speceis and all parameters separately) to 
# evaluate species contrbution to a mixture

x <- scale(obs_pred_flam[, c(3:7, 11:15)])
y <- as.data.frame(x)
zdata <- cbind(obs_pred_flam[, c(1,2)], y)

zdata$spread.diff <- zdata$spread - zdata$pred_spread
zdata$ignit.diff <- zdata$t2ignit - zdata$pred_ignit

Ab.spreaddiff <- zdata$spread.diff[grepl("Ab", zdata$spcode)]

rvector=rep(NA, 10000)
for(i in seq(1:10000)){
  x <- mean(sample(zdata$spread.diff, 150))
  rvector[i] <- x
}
p <- ecdf(rvector)(mean(Ab.spreaddiff))

# Once all p values are calculated, paste them onto the first line and run the second line
#Spread
p_values <- c(0.9042, 0.3471, 0.7343, 0.1123)
#Ignition
p_values <- c(1, 0, 0.9993, 0.3365)

pa <- p.adjust(p_values, method="BH") # to control for false discovery rate
pa
# spread: 0.9042 0.6942 0.9042 0.4492
#ignition:  1.000 0.000 1.000 0.673
