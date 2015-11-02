## read-decomp.R

## 1. Read in files, combine, create factors
## 2. Create summary statistics data frame
## 3. Exports three data frames, "decomp", "decomp.sum" and "decomp.sum2"

source("read-flam.R")

library(plyr)

# for kurtosis() function:
source("dist-funcs.R")


## The files are separate due to their size 
Y0 <- read.csv("../data/decomp/Decomp_Y0.csv", na.strings = c("","NA"))
Y1 <- read.csv("../data/decomp/Decomp_Y1.csv", na.strings = c("","NA"))
Y2 <- read.csv("../data/decomp/Decomp_Y2.csv", na.strings = c("","NA"))

# rbind em up
decomp <- rbind.fill(Y0,Y1,Y2)

# clean up
rm(list = c("Y0","Y1", "Y2"))

decomp$year <- factor(decomp$year)
decomp$tag <- factor(decomp$tag)

decomp$larea <- decomp$l * decomp$w
decomp$lvol <- decomp$larea * decomp$t

# Using ddply to summarize by different measures, and then using model
# selection to determine which measure is the best to use. It is mean(l)

decomp.sum <- ddply(decomp, .(tag, spcode, year, alt, asp), summarize,
                 l.mean=mean(l, na.rm=TRUE),
                 l.var=var(l, na.rm=TRUE),
                 l.med=median(l, na.rm=TRUE),
                 l.95=quantile(l, .95, na.rm=TRUE), # default qauntile method 7
                 l.25=quantile(l, .25, na.rm=TRUE),
                 l.iqr=IQR(l, na.rm=TRUE), # IQR(l) Interquartile range (3Q-1Q)
                 l.k=kurtosis(l, na.rm=TRUE),
                 n = sum(!is.na(l)) # get sample size too
                 )

wt <- read.csv("../data/decomp/Decomp_weight.csv", na.strings = c("","NA"))

wt$drate <- -log(wt$wf/wt$wi)/wt$year

decomp.sum2 <- ddply(decomp, .(tag, spcode, year, alt, asp), summarize,
                     l.mean=mean(l, na.rm=TRUE),
                     l.sd=sd(l, na.rm=TRUE),
                     larea.mean=mean(larea, na.rm=TRUE),
                     larea.sd=sd(larea, na.rm=TRUE),
                     lvol.mean=mean(lvol, na.rm=TRUE),
                     lvol.sd=sd(lvol, na.rm=TRUE),
                     n = sum(!is.na(l))
                     )

decompwt <- merge(subset(decomp.sum2, year!="0"), wt, by="tag", sort=F)

flamdecomp <- merge(subset(flam.avg, type="monoculture"), subset(decomp.sum2, year == "0"),
                    by="spcode", sort=F)
flamdecomp <- merge(flamdecomp, species)
