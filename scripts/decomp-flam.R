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

flamdecomp.sum.y0 <- filter(flamdecomp.sum, year == 0)
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

# Creating new dataframe with the predicted spread rate (y0, y1, y2))
pred.allyears <- decomp.sum  %>% ungroup() %>%
    select(spcode, year, l_mean, w_mean, t_mean, larea_mean)

pred.allyears$pred.spread <- predict(lmfit2, newdata=pred.allyears)

pred.allyears <- pred.allyears %>%
    select(spcode, year, pred.spread, l_mean, t_mean) %>%
    group_by(spcode, year) %>%
    summarise(lt_mean = mean(l_mean/t_mean),
              pred.spread_mean = mean(pred.spread))

#flamdecomp.sum$pred.spread <- predict(lmfit2) # pred fitted values

pred.allyears <- left_join(pred.allyears, select(flamdecomp.sum, spcode, year, spread.mean))
pred.allyears$spread.mean[pred.allyears$year > 0] <- pred.allyears$pred.spread_mean[pred.allyears$year > 0]
