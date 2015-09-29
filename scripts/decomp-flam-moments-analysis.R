## decomp-flam-moments-analysis.R

## 1. read in decomp and flammability data
## 2. Investigate relationship between decomposition data and flammability parameters 
## 3. Conclusion is that mean is the better moment to use

source("./read-decomp.R")
source("read-flam.R")

library(ggplot2)
library(MASS)
library(AICcmodavg)

## Testing which measure works best at describing spread rate
# using all values of spread
flamy0 <- merge(flam.singles, subset(decomp.sum, year == "0"), by="spcode", sort=F)

# working only with averages:
flamavgy0 <- merge(subset(flam.avg, type="monoculture"), subset(decomp.sum, year == "0"),
                   by="spcode", sort=F) 

# plotting all measures separately shows that l.25, l.var, l.iqr, and l.k are
# not good fit since they cause a dispersion of the points.
plot(spread.mean~l.var, data=flamavgy0) 

spread.all.lm <- lm(spread.mean~l.mean + l.var + l.med + l.95 + l.25 +l.iqr + l.k,
                    data=flamavgy0)
spread.some.lm <- lm(spread.mean~l.mean + l.med + l.95, data=flamavgy0)

step <- stepAIC(spread.some.lm, direction="both")

step$anova # display results

# Alternative AIC
spread.mod1 <- lm(spread.mean ~ l.mean + l.med + l.95, data=flamavgy0)
spread.mod2 <- lm(spread.mean ~ l.mean + l.med, data=flamavgy0)
spread.mod3 <- lm(spread.mean ~ l.mean, data=flamavgy0)

anova(spread.mod1, spread.mod2, spread.mod3)

aictab(list(spread.mod1, spread.mod2, spread.mod3))