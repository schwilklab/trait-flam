# Volatiles

## read-vol.R

## Volatiles data.

## 1. Read in files, turn from wide to long
## 2. Create summary statistics data frame

library(tidyr)
library(dplyr)

vol_wide <- read.csv("../data/volatiles/vol_wide.csv", na.strings = c("","NA"))

vol <- vol_wide %>% gather(key=spcode, value=area, -name, -group, -flash_point) %>%
    separate(spcode, c("spcode", "replicate"), "\\.") %>%
    select(spcode, replicate, name, group, flash_point, area) %>%
    filter(complete.cases(.))

vol.sum <- vol %>% group_by(spcode, replicate, group) %>%
    summarise(sum_area=sum(area), fp_wmean=weighted.mean(flash_point, area),
              sum_low_fp = sum(area[flash_point < 100]))

library(ggplot2)
ggplot(vol.sum, aes(group, sum_low_fp)) + geom_point() + facet_grid(. ~ spcode)
ggplot(filter(vol, flash_point<100), aes(spcode, flash_point, color=group)) +
    geom_jitter(aes(size=area))

