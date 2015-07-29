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
flam$mixture <- !(flam$spcode=="Abco" | flam$spcode=="Abma" | flam$spcode==
                    "Pipo" | flam$spcode=="Quke" | flam$spcode=="Cade" | 
                    flam$spcode=="Pije" | flam$spcode=="Pila" | flam$spcode=="Segi")
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

# Using mean as the better predictor:

decomp.avg <- ddply(decomp.sum, .(spcode, year), summarise,
                    l.m = mean(l.mean),
                    l.se = std.err(l.mean)
                    ) 


flamdecomp <- merge(subset(flam.avg, type="monoculture"), subset(decomp.avg, year == "0"),
                    by="spcode", sort=F) 
