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

lmfit <- lm(spread.mean ~ larea_mean*t_mean, data=flamdecomp.sum)
summary(lmfit)

# just "thinness"
lmfit2 <- lm(spread.mean~ I(l_mean / t_mean), data=flamdecomp.sum)
summary(lmfit2)

# compare
aictab(list(lmfit, lmfit2))

# so simple model wins.
# does this mean bulk density is function of length over thickness?
ggplot(flamdecomp.sum, aes(l_mean/t_mean, bulk.mean)) + geom_point() +
    geom_smooth(method="lm", se=FALSE)
ggplot(flamdecomp.sum, aes(l_mean/t_mean, spread.mean)) + geom_point()
ggplot(flamdecomp.sum, aes(bulk.mean, spread.mean)) + geom_point()
# yes.

# Creating new dataframe with the predicted spread rate after decomposition (year 1)

pred.y1 <- decomp.sum %>%  filter(year==1) %>% ungroup() %>%
    select(spcode, l_mean, w_mean, t_mean, larea_mean)

pred.y1$pred.spread <- predict(lmfit2, newdata=pred.y1)
#pred.y1$ci.spread.y1 <- predict(lmfit2, newdata=pred.y1, interval="confidence")


pred.y1 <- pred.y1 %>% select(spcode, pred.spread, l_mean, w_mean, larea_mean, t_mean) %>%
    group_by(spcode) %>% summarise_each(funs(mean, sd))

flamdecomp.sum$pred.spread <- predict(lmfit2) # pred fitted values

## flamdecomp.sum <- left_join(flamdecomp.sum, select(pred.y1, spcode, pred.spread_mean, pred.spread_sd), by="spcode")

## ggplot(flamdecomp.sum, aes(l_mean/t_mean, spread.mean)) +
##     geom_point(size=3, color="black")  +
##     geom_point(data=pred.y1, size=3,color="gray50") +
##     geom_point(aes(l_mean/t_mean, pred.spread), size=3, color="blue")

# todo: add CI to predictions

