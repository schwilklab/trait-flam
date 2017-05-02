# Volatiles

## vol-stats.R

source("read-vol.R")

library(agricolae)

## Are there differences in terpene content among the species? ... yes

library(lme4)

modsp <- lmer(log(sum_area +1) ~ spcode + (1|replicate), data=vol.tt1)

summary(modsp)

library(car)
Anova(modsp, white.adjust=TRUE)
#library(lsmeans)
#lsmeans(modsp, pairwise~spcode)

## Does terpene content affect time to ignition and spread rate?

vol.flamtt1 <- filter(vol.flamtt, terpene==1)

modspread <- lmer(spread_mean ~ log(sum_area +1) + (1|replicate), data=vol.flamtt1) # not working
summary(modspread)
Anova(modspread, white.adjust=TRUE)

modignit <- lmer(ignit_mean ~ log(sum_area +1) + (1|replicate), data=vol.flamtt1) 
summary(modignit)
Anova(modignit, white.adjust=TRUE)

## Is terpene content influenced by leaf size? yes

modsizelt <- lmer(log(sum_area + 1) ~ lt + (1|replicate), data=vol.decomp)
summary(modsizelt)
Anova(modsizelt, white.adjust=TRUE)
      
## Is decomposition rate affected by terpene content?

moddecomp <- lmer(ldrate_mean ~ log(sum_area + 1)  + (1|replicate), data=vol.decomprates)
summary(moddecomp)
Anova(moddecomp, white.adjust=TRUE)

