## read-decomp.R

## 1. Read in files, combine, create factors
## 2. Create summary statistics data frame
## 3. Exports three data frames, "decomp", "decomp.sum" and "decomp.sum2"

source("read-flam.R")
library(dplyr)

# for kurtosis() function:
source("dist-funcs.R")


## The files are separate due to their size 
Y0 <- read.csv("../data/decomp/Decomp_Y0.csv", na.strings = c("","NA"), stringsAsFactors=FALSE)
Y1 <- read.csv("../data/decomp/Decomp_Y1.csv", na.strings = c("","NA"), stringsAsFactors=FALSE)
Y2 <- read.csv("../data/decomp/Decomp_Y2.csv", na.strings = c("","NA"), stringsAsFactors=FALSE)

# rbind em up
decomp <- bind_rows(Y0,Y1,Y2)
# clean up
rm(list = c("Y0","Y1", "Y2"))

#decomp$year <- factor(decomp$year)
#decomp$tag <- factor(decomp$tag)  # dangerous -- will result in merge errors
decomp <- filter(decomp, ! (tag==126 & year > 0))
decomp$larea <- decomp$l * decomp$w
decomp$lvol <- decomp$larea * decomp$t

# Using ddply to summarize by different measures, and then using model
# selection to determine which measure is the best to use. It is mean(l)

## DWS: I don't see evidence that mean l is best predictor. of what? where is
## proof?

q90 <- function(x) { quantile(x, .90, na.rm=TRUE)}
q10 <- function(x) { quantile(x, .10, na.rm=TRUE)} 
N <- function(x) {sum(!is.na(x))}

wt <- read.csv("../data/decomp/Decomp_weight.csv", na.strings = c("","NA"), stringsAsFactors=FALSE)
wt <- filter(wt, tag!=126)
wt$drate <- -log(wt$wf/wt$wi)/wt$year

# summarize by litter bag
decomp.sum <- decomp %>% group_by(tag, spcode, year, alt, asp) %>%
    summarize_each(funs(mean(., na.rm=TRUE),
                        sd(., na.rm=TRUE),
                        q10,
                        q90,
                        IQR(., na.rm=TRUE), # IQR(l) Interquartile range (3Q-1Q)
                        kurtosis(., na.rm=TRUE)
                        )
                   ) %>%
    left_join(wt) %>%
    left_join(species)

# just year zero with flam trial results by species
## flamdecomp <- decomp.sum %>% filter(year==0) %>% select(l_mean, t_mean, display.name) %>%
##     group_by(display.name) %>% summarize(lt_mean = mean(l_mean/t_mean, na.rm=TRUE),
##                                          lt_sd=sd(l_mean/t_mean, na.rm=TRUE))
flamdecomp <- merge(flam.sp.avg, decomp.sum, all.x=TRUE)

flamdecomp.sum <- flamdecomp %>% filter(year==0) %>%
    select(display.name, bulk.mean, bulk.se, spread.mean, spread.se,
           l_mean, w_mean, t_mean, spread.mean, larea_mean) %>%
    group_by(display.name) %>% summarise(bulk.mean = mean(bulk.mean),
                                         bulk.se = mean(bulk.se),
                                         spread.mean = mean(spread.mean),
                                         spread.se = mean(spread.se),
                                         larea_mean = mean(larea_mean),
                                         t_mean = mean(t_mean),
                                         l_mean = mean(l_mean),
                                         w_mean = mean(w_mean),
                                         lt_mean= mean(l_mean/t_mean),
                                         lt_sd  = sd(l_mean/t_mean)
                                         )

