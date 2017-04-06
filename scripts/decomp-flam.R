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

# models predicting spread rate
lmfit <- lm(spread.mean ~ larea_mean*t_mean, data=flamdecompfilterY0)
summary(lmfit)
# best model

lmfit2 <- lm(spread.mean~ I(l_mean / t_mean), data=flamdecompfilterY0)
summary(lmfit2)
# Almost as good as first.

lmfit3 <- lm(spread.mean~ l_mean, data=flamdecompfilterY0)
summary(lmfit3)

aictab(list(lmfit, lmfit2, lmfit3))

# New dataframe with the predicted spread rate after decomposition (year 1)
pred.y1 <- decomp.sum %>%  filter(year==1) %>% ungroup() %>%
    select(spcode, l_mean, w_mean, t_mean, larea_mean, lt_mean)

pred.y1sum <- pred.y1 %>% select(spcode, l_mean, t_mean, lt_mean) %>%
  group_by(spcode) %>% summarise(l_mean = mean(l_mean),
                                 l_mean_sd = sd(l_mean),
                                 t_mean = mean(t_mean),
                                 t_mean_sd = sd(t_mean),
                                 lt_mean= mean(lt_mean),
                                 lt_sd  = sd(lt_mean)
  )

# Obtaining the prediction and confidence intervals at 95% from the prediction
# and merging the resulting data into a single dataframe

#pred.ci <- predict(lmfit2, newdata=pred.y1, interval="confidence", level=.95, se.fit=TRUE)
pred.pi <- predict(lmfit2, newdata=pred.y1sum, interval="prediction",
                   level=.95, se.fit=TRUE)$fit %>% as.data.frame() %>%
            select(spread_mean = fit, spread_lc = lwr, spread_uc = upr)


pred.y1sum <- cbind(pred.y1sum, pred.pi)

## pred.cipispsum <- pred.cipisp %>% select(spcode, pred_spread, lt_mean, ci_lwr, ci_upr, ci_se.fit, pi_lwr, pi_upr, pi_se.fit) %>%
##   group_by(spcode) %>% summarise(spread_mean = mean(pred_spread),
##                                  spread_sd = sd(pred_spread),
##                                  lt_mean= mean(lt_mean),
##                                  lt_sd  = sd(lt_mean),
##                                  cilwr_mean= mean(ci_lwr),
##                                  ciupr_mean= mean(ci_upr),
##                                  cise_mean= mean(ci_se.fit),
##                                  pilwr_mean= mean(pi_lwr),
##                                  piupr_mean= mean(pi_upr),
##                                  pise_mean= mean(pi_se.fit)
##   )



# Selecting only the columns of interest and preparing the dataframes to merge 
# later to produce the joint plot
pred.y1sum <- pred.y1sum[, c(1, 2, 4, 5, 8)] # use col name in select()
pred.y1sum$year <- 1
pred.y1sum <- pred.y1sum %>% left_join(species)


flamdecompfilterY0.sum <- flamdecompfilterY0 %>% 
                            select(spcode, l_mean, spread.mean, spread.se, lt_mean) %>%
                            group_by(spcode) %>% 
                            summarise(l_mean = mean(l_mean),
                                      spread_mean = mean(spread.mean),
                                      #spread_sd = mean(spread.se), # constant within sp anyway
                                      lt_mean = mean(lt_mean),
                                      spread_lc = spread_mean-2*mean(spread.se), # not correct
                                      spread_uc = spread_mean+2*mean(spread.se))


flamdecompfilterY0.sum$year <- 0
flamdecompfilterY0.sum <- flamdecompfilterY0.sum %>% left_join(species)

# Merging the 2 dataframes for the joint plot
flamdecomppredjoin <- rbind(flamdecompfilterY0.sum, pred.y1sum)
