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

sapply(vol, class)
#after melting the area column is now considered a character, not a numeric value!

write.csv(vol, file = "../data/volatiles/vol_long1.csv")

# Need to separate spcode into spcode and replicate
splist <- strsplit(vol$spcode, "\\.", fixed = TRUE)
  
vol <- ldply(splist)
colnames(vol) <- c("spcode", "replicate")

#couldn't get that to work so did it in excel

vol_long <- read.csv("../data/volatiles/vol_long.csv", na.strings = c("","NA"))

vol_long$flash_point <- as.numeric(vol_long$flash_point)
vol_long$x.area <- as.numeric(levels(vol_long$x.area))[vol_long$x.area] # this won't work!
#I think it might be due to the NA's present...

#PCA - can't work until I fix the problem with the area column

vol_PCA <- prcomp(vol_long, center=TRUE, scale.=TRUE)


# loadings
head(vol_PCA$rotation)
