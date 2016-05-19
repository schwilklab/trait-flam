## read-decomp.R

## 1. Read in files, combine, create factors
## 2. Create summary statistics data frame
## 3. Exports six data frames, "decomp", "decomp.sum", "decomp.sum2", "decomp.allrates",
##                                    "flamdecomp", and "flamdecomp.sum.y0"

source("read-flam.R")
library(dplyr)

# for kurtosis() function:
source("dist-funcs.R")


## The files are separate due to their size 
Y0 <- read.csv("../data/decomp/Decomp_Y0.csv", na.strings = c("","NA"),
               stringsAsFactors=FALSE)
Y1 <- read.csv("../data/decomp/Decomp_Y1.csv", na.strings = c("","NA"),
               stringsAsFactors=FALSE)
Y2 <- read.csv("../data/decomp/Decomp_Y2.csv", na.strings = c("","NA"),
               stringsAsFactors=FALSE)

# rbind em up
decomp <- bind_rows(Y0,Y1,Y2)
# clean up
rm(list = c("Y0","Y1", "Y2"))

#decomp$year <- factor(decomp$year)
#decomp$tag <- factor(decomp$tag)  # dangerous -- will result in merge errors
decomp <- filter(decomp, ! (tag==126 & year > 0))
decomp$larea <- decomp$l * decomp$w
decomp$lvol <- decomp$larea * decomp$t
decomp$lt <- decomp$l/decomp$t

# Using ddply to summarize by different measures

q90 <- function(x) { quantile(x, .90, na.rm=TRUE)}
q10 <- function(x) { quantile(x, .10, na.rm=TRUE)} 
N <- function(x) {sum(!is.na(x))}

wt <- read.csv("../data/decomp/Decomp_weight.csv", na.strings = c("","NA"),
               stringsAsFactors=FALSE)
wt <- filter(wt, tag!=126)
wt$wdrate <- -log(wt$wf/wt$wi)/wt$year
CN <- read.csv("../data/decomp/CNratio.csv", na.strings = c("","NA"), 
               stringsAsFactors=FALSE)
CN$CNratio <- CN$C/CN$N/CN$mass

LAI <- read.csv("../data/decomp/SEKI_LAI.csv", na.strings = c("","NA"), 
                stringsAsFactors=FALSE)

# summarize by litter bag
decomp.sum <- decomp %>% group_by(tag, spcode, year, alt, asp) %>%
    summarize_each(funs(mean(., na.rm=TRUE),
                        sd(., na.rm=TRUE),
                        q10,
                        q90,
                        IQR(., na.rm=TRUE), # IQR(l) Interquartile range (3Q-1Q)
                        kurtosis(., na.rm=TRUE)
                        )
                   ) 

## create a dataframe with decomposition rate by particle size change (ldrate), 
## decomposition rate by mass loss (wdrate), and CN ratio (CNratio)

decomp.temp <- subset(decomp.sum, year=="0")
decomp.temp <- decomp.temp[, c("tag", "l_mean")]
colnames(decomp.temp)[2] <- "li_mean"
decomp.temp1 <- subset(decomp.sum, year!="0")
decomp.temp1 <- decomp.temp1[, c("tag", "year", "l_mean")]
colnames(decomp.temp1)[3] <- "lf_mean"
decomp.rate <- decomp.temp %>% left_join(decomp.temp1)
decomp.rate$ldrate <- -log(decomp.rate$lf_mean/decomp.rate$li_mean)/decomp.rate$year
decomp.allrates <- decomp.rate %>% left_join(wt) %>% left_join(CN)


# just year zero with flam trial results by species
## flamdecomp <- decomp.sum %>% filter(year==0) %>% select(l_mean, t_mean, display.name) 
##                    %>% group_by(display.name) 
##                    %>% summarize(lt_mean = mean(l_mean/t_mean, na.rm=TRUE),
##                                          lt_sd=sd(l_mean/t_mean, na.rm=TRUE))

flamdecomp <- merge(flam.sp.avg, decomp.sum, all.x=TRUE)

flamdecomp.sum.y0 <- flamdecomp %>% filter(year==0) %>%
    select(display.name, bulk.mean, bulk.se, spread.mean, spread.se,
           l_mean, w_mean, t_mean, larea_mean) %>%
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