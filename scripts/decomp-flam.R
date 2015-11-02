## decomp-flam.R

## 1. read in decomp and flammability data
## 2. Creates a file, flamdecomp, that merges the flammability data with the 
##    decomposition data
## 3. Explores the relationship between spread rate and leaf length
## 4. Predicts spread rate from leaf length using the relatioship uncovered in 3.

source("read-decomp.R")
source("read-flam.R")

library(ggplot2)
library(dplyr)

# Using mean as the better predictor:

decomp.avg <- ddply(decomp.sum, .(spcode, year), summarise,
                    l.mean = mean(l.mean),
                    l.se = std.err(l.mean)
                    ) 

flamdecomp <- merge(subset(flam.avg, type="monoculture"), subset(decomp.avg, year == "0"),
                    by="spcode", sort=F) 

# Finding the relationship between the species

# DWS: you should fit on original flam data in a nested model. By fitting on
# means you are overestimating certainty.
lmfit <- lm(spread.mean~l.mean, data=flamdecomp)
summary(lmfit)

lmfit2 <- lm(spread.mean~l.mean + I(l.mean^2), data=flamdecomp)
summary(lmfit2)

### Quke seems to be an outlier, without it the relationship seems quadratic
flamy0q <- subset(flamdecomp, spcode!="Quke")

ggplot(flamdecomp, aes(log10(l.mean), spread.mean)) +
  geom_point(size=3) 

# Finding the relationship between the species without Quke
lmfit3 <- lm(spread.mean ~ l.mean, data=flamy0q)
summary(lmfit3)

lmfit4 <- lm(spread.mean~l.mean + I(l.mean^2), data=flamy0q)
summary(lmfit4)
### Without Quke the r2 is much better

# transform x axis
lmfit5 <- lm(spread.mean~log10(l.mean), data=flamy0q)
summary(lmfit5)

# Creating new dataframe with the predicted spread rate after decomposition (year 1)

pred.y1 <- decomp.sum2 %>%  filter(year==1 & spcode!="Quke" ) %>%
    select(spcode, l.mean)

pred.y1$spread.mean <- predict(lmfit5, newdata=pred.y1)

pred.y1 <- pred.y1 %>% group_by(spcode) %>%
    summarise(l.mean=mean(l.mean, na.rm=TRUE),
              spread.mean=mean(spread.mean, na.rm=TRUE))

ggplot(flamy0q, aes(l.mean, spread.mean, color=spcode)) +
    geom_point(size=3, shape=1) +
    geom_point(data=pred.y1, size=3,shape=2)

# DWS: but I would not believe that at all. quadratic is inferred well beyond
# data range (negative spread rates?)!, no reason to think spread rate goes
# down at high l.mean. I would not use quadratic. Basically, you have very
# little data on which to base any prediction.

# new.datay1$spread.pred <- prd # does not work, says replacement is smaller than data
## DWS: because you did not use the correct arguments for predict.lm.
