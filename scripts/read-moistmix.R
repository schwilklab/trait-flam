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

# Read in mixtures moisture data

mmc <- read.csv("../data/moisture/dry_down_long_mix.csv")
pred_mmc <- read.csv("../data/moisture/pred_MCdry_mix.csv")

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

# Read in mixtures flammability data

mflam <- read.csv("../data/moisture/burn_moisture_trials_mix.csv")
pred_mflam <- read.csv("../data/moisture/pred_flam_mix2.csv", na.strings = c("","NA"),
                       stringsAsFactors=FALSE)

mflam.sum <- mflam %>% group_by(spcode, hour) %>%
    				summarise(spread.mean = mean(spread),
              				spread.sd = sd(spread),
              				ignit.mean = mean(t2ignit),
              				ignit.sd = sd(t2ignit),
              				combus.mean = mean(combust),
              				combus.sd = sd(combust),
             		 	  	consum.mean = mean(consum),
              				consum.sd = sd(consum),
           		   	  	sustain.mean = mean(sustain),
           		   	  	sustain.sd = sd(sustain)
           		       		   )

obs_pred_flam <- merge(pred_mflam, mflam, by=c("hour", "spcode"))

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

# Residual analysis on flammability
resspread.mod <- lmer(spread.res ~ (1|spcode) + (1|hour), data=obs_pred_flam.res)
summary(resspread.mod)

x <- scale(obs_pred_flam[, c(3:7, 11:15)])
y <- as.data.frame(x)
zdata <- cbind(obs_pred_flam[, c(1,2)], y)

zdata$spread.diff <- zdata$spread - zdata$pred_spread
zdata$sustain.diff <- zdata$sustain - zdata$pred_sustain
zdata$ignit.diff <- zdata$t2ignit - zdata$pred_ignit
zdata$combust.diff <- zdata$combust - zdata$pred_combust
zdata$consum.diff <- zdata$consum - zdata$pred_consum

# Testing how different from 0 the residual zscores are
#(done for all speceis and all parameters separately)

Ab.spreaddiff <- zdata$spread.diff[grepl("Ab", zdata$spcode)]

rvector=rep(NA, 10000)
for(i in seq(1:10000)){
  x <- mean(sample(zdata$spread.diff, 150))
  rvector[i] <- x
}
p <- ecdf(rvector)(mean(Ab.spreaddiff))

# Once all p values are calculated, paste them onto the first line and run the second line
p_values <- c(0.0126, 0, 0.0012, 0.7222, 0, 0.7226, 0, 0.046)
pa <- p.adjust(p_values, method="BH") # to control for false discovery rate
pa
