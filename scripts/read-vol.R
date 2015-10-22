# Volatiles

## read-vol.R

## 1. Read in files, turn from wide to long
## 2. Create summary statistics data frame

library(reshape2)
library(plyr)

vol_wide <- read.csv("../data/volatiles/vol_wide.csv", na.strings = c("","NA"))

vol <- melt(vol_wide, id.vars=c("name","group","flash_point"), 
            variable.name = "spcode", 
            value.name = "area")

#sapply(vol, class)

# Need to separate spcode into spcode and replicate

library(tidyr)
voldf <- vol %>%
          separate(spcode, c("spcode", "replicate"), "\\.")

voldf <- voldf[, c(4, 5, 1, 2, 3, 6)]

voldf< - voldf[complete.cases(voldf),]
