## decomp-flam.R

## Investigate relationship between decomposition data and flammability
## parameters from the mixture study.

## read in decomp data
source("./read-decomp.R")

library(ggplot2)
library(MASS)
library(AICcmodavg)

# std error ## DWS: why this and not sample se?
std.err <- function(x) {
  sd(x)/sqrt(length(x))
}

## Read flammability data
flam <- read.csv("../data/burn-trials/flamdt.csv")
flam$mixture <- !(flam$spcode=="Abco" | flam$spcode=="Abma" | flam$spcode=="Pipo" | 
                        flam$spcode=="Quke" | flam$spcode=="Cade" | flam$spcode=="Pije" | 
                        flam$spcode=="Pila" | flam$spcode=="Segi")
flam.singles <- subset(flam, !mixture)
flam.singles$spcode <- factor(flam.singles$spcode)
flam.mixtures <- subset(flam, mixture)
flam$type <- factor(flam$mixture)
levels(flam$type) <- c("monoculture","mixture")

flam.avg <- ddply(flam, .(spcode,type), summarise,
                  bulk.mean = mean(bulk),
                  bulk.se = std.err(bulk),
                  spread.mean = mean(spread),
                  spread.se = std.err(spread),
                  ignit.mean = mean(ignit),
                  ignit.se = std.err(ignit),
                  combus.mean = mean(combus),
                  combus.se = std.err(combus),
                  consum.mean = mean(consum),
                  consum.se = std.err(consum),
                  sustain.mean = mean(sustain),
                  sustain.se = std.err(sustain),
                  b75.heat100.mean = mean(b75.heat100),
                  b75.heat100.se = std.err(b75.heat100),
                  b75.numsecs100.mean = mean(b75.numsecs100),
                  b75.numsecs100.se = std.err(b75.numsecs100),
                  leaf.area.mean = mean(leaf.area),
                  leaf.area.se = std.err(leaf.area)
                  )

## Testing which measure works best at describing spread rate
# using all values of spread
flamy0 <- merge(flam.singles, subset(decomp.sum, year == "0"), by="spcode", sort=F)
# working only with averages:
flamy0 <- merge(subset(flam.avg, type="monoculture"), subset(decomp.sum, year == "0"),
                by="spcode", sort=F) 

# plotting all measures separately shows that l.25, l.var, l.iqr, and l.k are
# not good fit since they cause a dispersion of the points.
plot(spread.mean~l.var, data=flamy0) 

spread.all.lm <- lm(spread.mean~l.mean + l.var + l.med + l.95 + l.25 +l.iqr + l.k,
                    data=flamy0)
spread.some.lm <- lm(spread.mean~l.mean + l.med + l.95, data=flamy0)


step <- stepAIC(spread.some.lm, direction="both")

step$anova # display results

## Stepwise Model Path 
## Analysis of Deviance Table

## Initial Model:
## spread.mean ~ l.mean + l.med + l.95

## Final Model:
## spread.mean ~ l.95


##       Step Df   Deviance Resid. Df Resid. Dev       AIC
## 1                               44  0.5471657 -206.7618
## 2  - l.med  1 0.00431305        45  0.5514787 -208.3849
## 3 - l.mean  1 0.01262522        46  0.5641039 -209.2985



# Alternative AIC
spread.mod1 <- lm(spread.mean ~ l.mean + l.med + l.95, data=flamy0)
spread.mod2 <- lm(spread.mean ~ l.mean + l.med, data=flamy0)
spread.mod3 <- lm(spread.mean ~ l.mean, data=flamy0)

anova(spread.mod1, spread.mod2, spread.mod3)

## Analysis of Variance Table

## Model 1: spread.mean ~ l.mean + l.med + l.95
## Model 2: spread.mean ~ l.mean + l.med
## Model 3: spread.mean ~ l.mean
##   Res.Df     RSS Df  Sum of Sq      F Pr(>F)
## 1     44 0.54717                            
## 2     45 0.57094 -1 -0.0237757 1.9119 0.1737
## 3     46 0.57124 -1 -0.0002971 0.0239 0.8779
## >

aictab(list(spread.mod1, spread.mod2, spread.mod3))


## Model selection based on AICc :

##      K   AICc Delta_AICc AICcWt Cum.Wt    LL
## Mod3 3 -69.93       0.00   0.64   0.64 38.24
## Mod2 4 -67.57       2.36   0.20   0.84 38.25
## Mod1 5 -67.12       2.82   0.16   1.00 39.27

## Warning message:
## In aictab.AIClm(list(spread.mod1, spread.mod2, spread.mod3)) : 
## Model names have been supplied automatically in the table
