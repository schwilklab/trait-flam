## decomp-flam.R

## 1. read in decomp and flammability data
## 2. Creates a file, flamdecomp, that merges the flammability data with the 
##    decomposition data
## 3. Explores the relationship between spread rate and leaf length
## 4. Predicts spread rate from leaf length using the relatioship uncovered in 3.

source("read-decomp.R")
source("read-flam.R")

library(ggplot2)

# Using mean as the better predictor:

decomp.avg <- ddply(decomp.sum, .(spcode, year), summarise,
                    l.mean = mean(l.mean),
                    l.se = std.err(l.mean)
                    ) 

flamdecomp <- merge(subset(flam.avg, type="monoculture"), subset(decomp.avg, year == "0"),
                    by="spcode", sort=F) 

# Finding the relationship between the species 
lmfit <- lm(spread.mean~l.mean, data=flamdecomp)
summary(lmfit)

lmfit2 <- lm(spread.mean~l.mean + I(l.mean^2), data=flamdecomp)
summary(lmfit2)

### Quke seems to be an outlier, without it the relationship seems quadratic
flamy0q <- subset(flamdecomp, spcode!="Quke")

ggplot(flamy0q, aes(l.mean, spread.mean)) +
  geom_point(size=3) 

# Finding the relationship between the species without Quke
lmfit3 <- lm(spread.mean~l.mean, data=flamy0q)
summary(lmfit3)

lmfit4 <- lm(spread.mean~l.mean + I(l.mean^2), data=flamy0q)
summary(lmfit4)

### Without Quke the r2 is much better

# Creating new dataframe with the predicted spread rate after decomposition (year 1)

new.datay1 <- subset(decomp.sum2, year=="1", select=c(spcode, l.mean, l.sd))
new.dfy1 <- na.omit(new.datay1)
new.dfy1q <- subset(new.dfy1, spcode!="Quke")

prd <- predict(lmfit4, data=new.dfy1q$l.mean)

new.datay1$spread.pred <- prd # does not work, says replacement is smaller than data