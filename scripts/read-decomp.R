## read-decomp.R

## 1. Read in files, combine, create factors
## 2. Create summary statistics data frame
## 3. Exports two data frames, "decomp" and "decomp.sum"

library(plyr)

# for kurtosis() function:
source("dist-funcs.R")

## DWS: why are these in separate files?
Y0 <- read.csv("../data/decomp/Decomp_Y0.csv")
Y1 <- read.csv("../data/decomp/Decomp_Y1.csv")
Y2 <- read.csv("../data/decomp/Decomp_Y2.csv")
# rbind em up
decomp <- rbind.fill(Y0,Y1,Y2)
# clean up
rm(list = c("Y0","Y1", "Y2"))

decomp$year <- factor(decomp$year)
decomp$tag <- factor(decomp$tag)

# Using ddply to summarize by different measures, and then using model
# selection to determine which measure is the best to use. mean(l)
## DWS: shouldn't this be summarizing over replicates, not spcodes? How do you
## deal with variance?
decomp.sum <- ddply(decomp, .(spcode, year, alt, asp), summarize,
                 l.mean=mean(l, na.rm=TRUE),
                 l.var=var(l, na.rm=TRUE),
                 l.med=median(l, na.rm=TRUE),
                 l.95=quantile(l, .95, na.rm=TRUE), # default qauntile method 7
                 l.25=quantile(l, .25, na.rm=TRUE),
                 l.iqr=IQR(l, na.rm=TRUE), # IQR(l) Interquartile range (3Q-1Q)
                 l.k=kurtosis(l, na.rm=TRUE),
                 n = sum(!is.na(l)) # get sample size too
                 )



## Some data checks
## subset(decomp, spcode=="")
##       tag spcode alt asp year     l     w     t
## 9794   31                   2 22.77  4.14  2.58
## 10419  31                   2 50.42 28.45 11.11

## DWS: missing spcode in 2 lines
