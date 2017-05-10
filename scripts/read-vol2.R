# Volatiles

## read-vol2.R

## Volatiles data.

## 1. Read in files, turn from wide to long
## 2. Create summary statistics data frame
## 3. Prepare data frames for stats (join with flammability and decomp)

library(tidyr)
library(dplyr)

species <- read.csv("../data/species.csv", stringsAsFactors=FALSE)

## 1. Read in files, turn from wide to long

vol_wide <- read.csv("../data/volatiles/vol_area_wide.csv", na.strings = c("","NA"))

vol <- vol_wide %>% gather(key=spcode, value=area, -name, -group_more, -group_less, -terpene, -flash_point) %>%
  separate(spcode, c("spcode", "replicate"), "\\.") %>%
  select(spcode, replicate, name, group_more, group_less, terpene, flash_point, area) %>%
  filter(complete.cases(.))

vol$terpene <- as.factor(vol$terpene)

## 2. Create summary statistics data frames

vol.tv <- vol %>% group_by(spcode, replicate) %>% 
  summarise(sum_area=sum(area), sum_low_fp = sum(area[flash_point < 40]))

vol.tv.mean <- vol.tv %>% group_by(spcode) %>% 
  summarise(mean_area=mean(sum_area), sd_area=sd(sum_area),
            mean_low_fp = mean(sum_low_fp), sd_low_fp = sd(sum_low_fp)) %>% 
  left_join(species, by="spcode") #for graphing


## Analysis on the flamambility
## Total volatile content
source("read-flam.R")
library(nlme)
library(car)

vol_pred_mix <- read.csv("../data/volatiles/vol_pred_mix.csv", na.strings = c("","NA"))
# this is the average volatile content calculated for each mixture I had on the masters
vol_pred_flam <- flam.avg %>% left_join(vol_pred_mix, by=c("spcode", "type"))

mod.spread1.lme <- lme(spread.mean ~ bulk.mean, data=vol_pred_flam, random = ~ 1 | spcode, method="ML")

mod.spread2.lme <- lme(spread.mean ~ bulk.mean + mean_area, data=vol_pred_flam, random = ~ 1 | spcode, method="ML")

Anova(mod.spread2.lme, white.adjust=TRUE)

anova(mod.spread1.lme, mod.spread2.lme)

mod.ignit1.lme <- lme(ignit.mean ~ bulk.mean, data=vol_pred_flam, random = ~ 1 | spcode, method="ML")

mod.ignit2.lme <- lme(ignit.mean ~ bulk.mean + mean_area, data=vol_pred_flam, random = ~ 1 | spcode, method="ML")

Anova(mod.ignit2.lme, white.adjust=TRUE)

anova(mod.ignit1.lme, mod.ignit2.lme)

## Monoterpenes only (fp<40)

mod.spread3.lme <- lme(spread.mean ~ bulk.mean, data=vol_pred_flam, random = ~ 1 | spcode, method="ML")

mod.spread4.lme <- lme(spread.mean ~ bulk.mean + mean_low_fp, data=vol_pred_flam, random = ~ 1 | spcode, method="ML")

Anova(mod.spread4.lme, white.adjust=TRUE)

anova(mod.spread3.lme, mod.spread4.lme)

mod.ignit3.lme <- lme(ignit.mean ~ bulk.mean, data=vol_pred_flam, random = ~ 1 | spcode, method="ML")

mod.ignit4.lme <- lme(ignit.mean ~ bulk.mean + mean_low_fp, data=vol_pred_flam, random = ~ 1 | spcode, method="ML")

Anova(mod.ignit4.lme, white.adjust=TRUE)

anova(mod.ignit3.lme, mod.ignit4.lme)

# Plotting the non-additivity and volatiles

source("theme-opts.R")

# 1 Creating a simple model to extract the residuals from spread ~ bulk,
# then graphing those residuals against volatile content

mod1 <- lm(spread.mean ~ bulk.mean, data= vol_pred_flam)
vol_pred_flam$res_mod1 <- residuals(mod1)

summary(lme(res_mod1 ~ mean_area, data=vol_pred_flam, random = ~ 1 | spcode, method="ML"))

mod2 <- lm(ignit.mean ~ bulk.mean, data= vol_pred_flam)
vol_pred_flam$res_mod2 <- residuals(mod2)

summary(lme(res_mod2 ~ mean_area, data=vol_pred_flam, random = ~ 1 | spcode, method="ML"))

ggplot(vol_pred_flam, aes(mean_area, res_mod1,color=type)) +
  geom_point(size=2.5) +
  xlab("Volatile content")+
  ylab("Residuals from model of Spread rate ~ Bulk density") +
  ritatheme +
  geom_smooth(method="lm", se=F, size=1.0) +
  scale_linetype_manual(values = c(1,3,2)) +
  scale_color_manual(values = c( "gray50", "black")) 

ggsave("../results/plots/spread_bulk_res_vol.png", width=9, height=6, dpi=ppi)

ggplot(vol_pred_flam, aes(mean_area, res_mod2,color=type)) +
  geom_point(size=2.5) +
  xlab("Volatile content")+
  ylab("Residuals from model of Time to ignition ~ Bulk density") +
  ritatheme +
  geom_smooth(method="lm", se=F, size=1.0) +
  scale_linetype_manual(values = c(1,3,2)) +
  scale_color_manual(values = c( "gray50", "black")) 

ggsave("../results/plots/ignit_bulk_res_vol.png", width=9, height=6, dpi=ppi)

# 2 Using the residuals from observed - predicted from the mixtures

flam_res <- read.csv("../data/volatiles/flam_obs-pred.csv", na.strings = c("","NA"))
# this is the values for observed - predicted on flamambility measurements from the
# mixtures in the masters
flam_res_vol <- flam_res %>% left_join(vol_pred_mix, by="spcode")

flam_res_vol$bulk.mean <- vol_pred_flam$bulk.mean[match(flam_res_vol$spcode, vol_pred_flam$spcode)]

mod.spreadres <- lme(spread ~ bulk.mean + mean_area, data=flam_res_vol, random = ~ 1 | spcode, na.action = na.exclude, method="ML")
Anova(mod.spreadres, white.adjust=TRUE)
mod.ignitres <- lme(ignit ~ bulk.mean + mean_area, data=flam_res_vol, random = ~ 1 | spcode, na.action = na.exclude, method="ML")
Anova(mod.ignitres, white.adjust=TRUE)

ggplot(flam_res_vol, aes(mean_area, spread)) +
  geom_point(size=2.5) +
  xlab("Volatile content")+
  ylab("Non-additivity score for spread rate") +
  ritatheme + geom_smooth(method="lm", se=F, size=1.0)

ggsave("../results/plots/non-add_res_vol.png", width=9, height=6, dpi=ppi)

ggplot(flam_res_vol, aes(mean_area, ignit)) +
  geom_point(size=2.5) +
  xlab("Volatile content")+
  ylab("Non-additivity score for time to igntion") +
  ritatheme + geom_smooth(method="lm", se=F, size=1.0)

