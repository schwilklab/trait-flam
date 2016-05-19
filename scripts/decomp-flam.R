## decomp-flam.R

## 1. read in decomp and flammability data
## 2. Creates a file, flamdecomp, that merges the flammability data with the 
##    decomposition data
## 3. Explores the relationship between spread rate and leaf length
## 4. Predicts spread rate from leaf length using the relationship uncovered in 3.

source("read-decomp.R")

library(ggplot2)
library(dplyr)
library(lme4)
library(AICcmodavg)

# DWS: you should fit on original flam data in a nested model. By fitting on
# means you are overestimating certainty. But may be fine for now

lmfit <- lm(spread.mean ~ larea_mean*t_mean, data=flamdecomp.sum.y0)
summary(lmfit)

# just "thinness"
lmfit2 <- lm(spread.mean~ I(l_mean / t_mean), data=flamdecomp.sum.y0)
summary(lmfit2)

# compare
aictab(list(lmfit, lmfit2))

# so simple model wins.
# does this mean bulk density is function of length over thickness?
ggplot(flamdecomp.sum.y0, aes(l_mean/t_mean, bulk.mean)) + geom_point() +
    geom_smooth(method="lm", se=FALSE)
ggplot(flamdecomp.sum.y0, aes(l_mean/t_mean, spread.mean)) + geom_point()
ggplot(flamdecomp.sum.y0, aes(bulk.mean, spread.mean)) + geom_point()
# yes.

# Creating new dataframe with the predicted spread rate after decomposition (year 1)

pred.y1 <- decomp.sum %>%  filter(year==1) %>% ungroup() %>%
    select(spcode, l_mean, w_mean, t_mean, larea_mean)

pred.y1$pred_spread <- predict(lmfit2, newdata=pred.y1)
#pred.y1$ci.spread.y1 <- predict(lmfit2, newdata=pred.y1, interval="confidence")
pred.y1$lt_mean <- pred.y1$l_mean/pred.y1$t_mean


pred.y1sum <- pred.y1 %>% select(spcode, pred_spread, l_mean, w_mean, larea_mean, t_mean, lt_mean) %>%
    group_by(spcode) %>% summarise(pred_spread_mean = mean(pred_spread),
                                   pred_spread_sd = sd(pred_spread),
                                   larea_mean = mean(larea_mean),
                                   larea_mean_sd = sd(larea_mean),
                                   t_mean = mean(t_mean),
                                   t_mean_sd = sd(t_mean),
                                   l_mean = mean(l_mean),
                                   l_mean_sd = sd(l_mean),
                                   w_mean = mean(w_mean),
                                   w_mean_sd = sd(w_mean),
                                   lt_mean= mean(lt_mean),
                                   lt_sd  = sd(lt_mean)
    )

pred.y1sum <- pred.y1sum %>% left_join(species)

#flamdecomp.sum$pred.spread <- predict(lmfit2) # pred fitted values

## flamdecomp.sum <- left_join(flamdecomp.sum, select(pred.y1, spcode, pred.spread_mean, pred.spread_sd), by="spcode")




