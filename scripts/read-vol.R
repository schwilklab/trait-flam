# Volatiles

## read-vol.R

## 1. Read in files, turn from wide to long
## 2. Create summary statistics data frame

library(reshape2)
library("plyr")

vol_wide <- read.csv("../data/volatiles/vol_wide.csv", na.strings = c("","NA"))

vol <- melt(vol_wide, id.vars=c("name","group","flash_point"), 
            variable.name = "spcode", 
            value.name = "area")

#sapply(vol, class)

write.csv(vol, file = "../data/volatiles/vol_long.csv")

# Need to separate spcode into spcode and replicate
splist <- strsplit(vol[,4], "\\.")
  
vol <- ldply(splist)
colnames(vol) <- c("spcode", "replicate")

#couldn't get that to work so did it in excel

vol_long <- read.csv("../data/volatiles/vol_long.csv", na.strings = c("","NA"))

#PCA 

vol_PCA <- prcomp(na.omit(vol_long[, 6]), center=TRUE, scale.=TRUE)


# loadings
head(vol_PCA$rotation)
