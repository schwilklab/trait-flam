## decomp-flam.R

## 1. read in decomp and flammability data
## 2. Creates a file, flamdecomp, that merges the flammability data with the 
##    decomposition data
## 3. Explores the relationship between spread rate and leaf length
## 4. Predicts spread rate from leaf length using the relationship uncovered in 3.

source("read-decomp.R")

library(ggplot2)

library(lme4)
library(AICcmodavg)

#flamdecompfilter <- flamdecomp[ , c(1, 2, 5, 6, 21:41)]

## any real reason for selecting columns there?
# New dataframe with the predicted spread rate after decomposition (year 1)
decomp.by.species <- decomp.sum %>%  ungroup() %>%
  select(spcode, year, l_mean, w_mean, t_mean, larea_mean, lt_mean) %>%
  group_by(spcode, year) %>% summarise(l_mean_sd = sd(l_mean, na.rm=TRUE),
                                 l_mean = mean(l_mean),
                                 t_mean_sd = sd(t_mean, na.rm=TRUE),
                                 t_mean = mean(t_mean),
                                 w_sd = sd(w_mean),
                                 w_mean = mean(w_mean),
                                 larea_sd = sd(larea_mean),
                                 larea_mean = mean(larea_mean),
                                 lt_sd  = sd(lt_mean, na.rm=TRUE),
                                 lt_mean= mean(lt_mean)
                                 )


## Year 0 decomp by flam summary for figure 2
flam.by.species <- flam.sp.avg %>%
  mutate(spread_lc = spread.mean-2*spread.sd, spread_uc = spread.mean+2*spread.sd) %>%
  select(spcode, spread_mean=spread.mean, spread_lc, spread_uc)

flamdecomp <- decomp.by.species %>% filter(year == 0) %>% left_join(flam.by.species)
## decomp.Y0.conifers <- filter(decomp.Y0, spcode %in% 
##                               c("Abco", "Abma", "Cade", "Pije", "Pila", "Pipo", "Segi"))

## Year 1 predicted values

### Make year 1 summary (predicted spread rates)

# models predicting spread rate
lmfit <- lm(spread_mean ~ larea_mean*t_mean, data=flamdecomp)
summary(lmfit)
# best model

lmfit2 <- lm(spread_mean~ I(l_mean / t_mean), data=flamdecomp)
summary(lmfit2)
# Almost as good as first.

lmfit3 <- lm(spread_mean~ l_mean, data=flamdecomp)
summary(lmfit3)

aictab(list(lmfit, lmfit2, lmfit3))

# So model 1 definitely best. No reason you can't use that model! You always
# still plot by length/thickness


# Note: order matters above because we are re-using col names!

# Obtaining the prediction and confidence intervals at 95% from the prediction
# and merging the resulting data into a single dataframe

pred.y1 <- decomp.by.species %>% filter(year == 1) 


#pred.ci <- predict(lmfit2, newdata=pred.y1, interval="confidence", level=.95, se.fit=TRUE)
pred.pi <- predict(lmfit2, newdata=pred.y1, interval="confidence", #"prediction",
                   level=.95, se.fit=TRUE)$fit %>% as.data.frame() %>%
            select(spread_mean = fit, spread_lc = lwr, spread_uc = upr)

pred.y1 <- bind_cols(pred.y1, pred.pi)

### Add to flamdecomp
flamdecomp <- bind_rows(flamdecomp, pred.y1) %>% left_join(species)
