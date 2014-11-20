library(ggplot2)
library(plyr)

Y0 <- read.csv("Decomp_Y0.csv")
Y1 <- read.csv("Decomp_Y1.csv")

Y0$spcode <- factor(Y0$spcode)
Y1$spcode <- factor(Y1$spcode)
Y0$year <- factor(Y0$year)
Y1$year <- factor(Y1$year)
Y0$tag <- factor(Y0$tag)
Y1$tag <- factor(Y1$tag)

# Using ddply to summarize by different measures, and then using model selection to determine which measure is the best to use.
# mean(l)
y0.avg <- ddply(Y0, .(spcode), summarize, l.mean=mean(l))
y1.avg <- ddply(Y1, .(spcode), summarize, l1.mean=mean(l))
# var(l)
y0.var <- ddply(Y0, .(spcode), summarize, l.var=var(l))
y1.ver <- ddply(Y1, .(spcode), summarize, l1.var=var(l))
# median(l)
y0.med <- ddply(Y0, .(spcode), summarize, l.med=median(l))
y1.med <- ddply(Y1, .(spcode), summarize, l1.med=median(l))
# quantile(l, .95, type=7) 95th percentile *
y0.95 <- ddply(Y0, .(spcode), summarize, l.95=quantile(l, .95, type=7))
y1.95 <- ddply(Y1, .(spcode), summarize, l1.95=quantile(l, .95, type=7))
# quantile(l, .25, type =7) first quartile (25%) *
y0.25 <- ddply(Y0, .(spcode), summarize, l.25=quantile(l, .25, type=7))
y1.25 <- ddply(Y1, .(spcode), summarize, l1.25=quantile(l, .25, type=7))
# * R has 9 types of quantile calculation. Type 7 is the one used in summary()
# IQR(l) Interquartile range (3Q-1Q)
y0.iqr <- ddply(Y0, .(spcode), summarize, l.iqr=IQR(l))
y1.iqr <- ddply(Y1, .(spcode), summarize, l1.iqr=IQR(l))
# kurtosis
#This formulation of kurtosis adjusts it so that if a distribution is normal, it will have k=0 (rtaher than 3), if platykurtic k<0, and if leptokurtic k>0.

kurtosis<-function(x) {
	m4<-sum((x-mean(x))^4)/length(x)
	s4<-var(x)^2
	m4/s4-3}
	
y0.k <- ddply(Y0, .(spcode), summarize, l.k=kurtosis(l))
y1.k <- ddply(Y1, .(spcode), summarize, l1.k=kurtosis(l))

# all the functions together (make sure to run the code for the function kurtosis):

newY1 <- na.omit(Y1) # Since I have one bag without data

y0.sum <- ddply(Y0, .(spcode), summarize, l.mean=mean(l), l.var=var(l), l.med=median(l), l.95=quantile(l, .95, type=7), l.25=quantile(l, .25, type=7), l.iqr=IQR(l), l.k=kurtosis(l))
y1.sum <- ddply(newY1, .(spcode), summarize, l1.mean=mean(l), l1.var=var(l), l1.med=median(l), l1.95=quantile(l, .95, type=7, na.rm=TRUE), l1.25=quantile(l, .25, type=7, na.rm=TRUE), l1.iqr=IQR(l, na.rm=TRUE), l1.k=kurtosis(l))

y1.avg <- ddply(newY1, .(spcode, tag, year, asp, alt), summarize, l1.mean=mean(l)) # if I want to summarize over more than just spcode

## Flammability data

flam <- read.csv("flamdt.csv")
flam$spcode <- factor(flam$spcode)

flam$mixture <- !(flam$spcode=="Abco" | flam$spcode=="Abma" | flam$spcode=="Pipo" | 
                        flam$spcode=="Quke" | flam$spcode=="Cade" | flam$spcode=="Pije" | 
                        flam$spcode=="Pila" | flam$spcode=="Segi")
flam.singles <- subset(flam, !mixture)
flam.singles$spcode <- factor(flam.singles$spcode)
flam.mixtures <- subset(flam, mixture)
flam$type <- factor(flam$mixture)
levels(flam$type) <- c("monoculture","mixture")

std.err <- function(x) {
  sd(x)/sqrt(length(x))
}

flam.avg <- ddply(flam, .(spcode,type), summarise, bulk.mean = mean(bulk), bulk.se = std.err(bulk), spread.mean = mean(spread), spread.se = std.err(spread),  ignit.mean = mean(ignit), ignit.se = std.err(ignit),  combus.mean = mean(combus), combus.se = std.err(combus),  consum.mean = mean(consum), consum.se = std.err(consum),  sustain.mean = mean(sustain), sustain.se = std.err(sustain),  b75.heat100.mean = mean(b75.heat100), b75.heat100.se = std.err(b75.heat100),  b75.numsecs100.mean = mean(b75.numsecs100), b75.numsecs100.se = std.err(b75.numsecs100),  leaf.area.mean = mean(leaf.area), leaf.area.se = std.err(leaf.area))

## Testing which measure works best at describing spread rate

flamy0 <- merge(flam.singles, y0.sum, by="spcode", sort=F) # using all values of spread
flamy0 <- merge(subset(flam.avg, type="monoculture"), y0.sum, by="spcode", sort=F) # working only with averages

plot(spread.mean~l.var, data=flamy0) # plotting all measures separately shows that l.25, l.var, l.iqr, and l.k are not good fit since they cause a dispersion of the points.


spread.all.lm <- lm(spread.mean~l.mean + l.var + l.med + l.95 + l.25 +l.iqr + l.k, data=flamy0)

spread.some.lm <- lm(spread.mean~l.mean + l.med + l.95, data=flamy0)

library(MASS)

step <- stepAIC(spread.some.lm, direction="both")

step$anova # display results

Stepwise Model Path 
Analysis of Deviance Table

Initial Model:
spread.mean ~ l.mean + l.med + l.95

Final Model:
spread.mean ~ 1


      Step Df     Deviance Resid. Df Resid. Dev       AIC
1                                  4 0.09258656 -27.67242
2  - l.med  1 0.0007706294         5 0.09335719 -29.60611
3   - l.95  1 0.0016214068         6 0.09497860 -31.46836
4 - l.mean  1 0.0037318474         7 0.09871044 -33.16005


# Alternative AIC

spread.mod1<- lm(spread.mean~l.mean + l.med + l.95, data=flamy0)
spread.mod2<- lm(spread.mean~l.mean + l.med, data=flamy0)
spread.mod3<- lm(spread.mean~l.mean, data=flamy0)

anova(spread.mod1, spread.mod2,spread.mod3)

Analysis of Variance Table

Model 1: spread.mean ~ l.mean + l.med + l.95
Model 2: spread.mean ~ l.mean + l.med
Model 3: spread.mean ~ l.mean
  Res.Df      RSS Df  Sum of Sq      F Pr(>F)
1      4 0.092587                            
2      5 0.094278 -1 -0.0016913 0.0731 0.8003
3      6 0.094979 -1 -0.0007007 0.0303 0.8703

library(AICcmodavg)

source("AICc.R")

AICtable(spread.mod1)
                                      Model Log Lik. K  AIC$_c$ $\\Delta$AIC W$_i$
1 spread.mean $\\sim $ l.mean + l.med + l.95 6.484703 5 27.03059            0     1

AICtable(spread.mod2)
                                Model Log Lik. K AIC$_c$ $\\Delta$AIC W$_i$
1 spread.mean $\\sim $ l.mean + l.med 6.412292 4 8.50875            0     1

AICtable(spread.mod3)
                        Model Log Lik. K    AIC$_c$ $\\Delta$AIC W$_i$
1 spread.mean $\\sim $ l.mean 6.382673 3 -0.7653456            0     1



