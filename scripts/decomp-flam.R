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

flamdecompfilter <- flamdecomp[ , c(1, 2, 5, 6, 21:41)]
flamdecompfilterY0 <- filter(flamdecompfilter, year == 0)
flamdecompfilterQ <- filter(flamdecompfilterY0, spcode %in% 
                              c("Abco", "Abma", "Cade", "Pije", "Pila", "Pipo", "Segi"))

lmfit <- lm(spread.mean ~ larea_mean*t_mean, data=flamdecompfilterQ)
summary(lmfit)

lmfit2 <- lm(spread.mean~ I(l_mean / t_mean), data=flamdecompfilterQ)
summary(lmfit2)

lmfit3 <- lm(spread.mean~ l_mean, data=flamdecompfilterQ)
summary(lmfit3)

aictab(list(lmfit, lmfit2, lmfit3))

# Creating new dataframe with the predicted spread rate after decomposition (year 1)

pred.y1 <- decomp.sum %>%  filter(year==1) %>% ungroup() %>%
    select(spcode, l_mean, w_mean, t_mean, larea_mean)

pred.y1$pred_spread <- predict(lmfit2, newdata=pred.y1)
#pred.y1$ci.spread.y1 <- predict(lmfit2, newdata=pred.y1, interval="confidence")
pred.y1$lt_mean <- pred.y1$l_mean/pred.y1$t_mean


pred.y1sum <- pred.y1 %>% select(spcode, pred_spread, l_mean, w_mean, larea_mean, t_mean, lt_mean) %>%
  group_by(spcode) %>% summarise(l_mean = mean(l_mean),
                                 l_mean_sd = sd(l_mean),
                                 spread_mean = mean(pred_spread),
                                 spread_sd = sd(pred_spread),
                                 larea_mean = mean(larea_mean),
                                 larea_mean_sd = sd(larea_mean),
                                 t_mean = mean(t_mean),
                                 t_mean_sd = sd(t_mean),
                                 w_mean = mean(w_mean),
                                 w_mean_sd = sd(w_mean),
                                 lt_mean= mean(lt_mean),
                                 lt_sd  = sd(lt_mean)
  )

# Selecting only the columns of interest and preparing the dataframes to merge 
# later to produce the joint plot
pred.y1sum <- pred.y1sum[, c(1, 2, 4, 5)]
pred.y1sum$year <- 1
pred.y1sum <- pred.y1sum %>% left_join(species)


flamdecompfilterY0.sum <- flamdecompfilterY0 %>% 
                            select(spcode, l_mean, spread.mean, spread.se) %>%
                            group_by(spcode) %>% 
                            summarise(l_mean = mean(l_mean),
                                      spread_mean = mean(spread.mean),
                                      spread_sd = mean(spread.se))

flamdecompfilterY0.sum$year <- 0
flamdecompfilterY0.sum <- flamdecompfilterY0.sum %>% left_join(species)

# Merging the 2 dataframes for the joint plot
flamdecomppredjoin <- rbind(flamdecompfilterY0.sum, pred.y1sum)
