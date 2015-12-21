## decomp-stats.R

## 1. Do correlation analysis on mass loss, CN and leaf length change
## 2. Model decomposition to tease out the effect of alt, asp, and LAI on leaf 
##    length change

source("read-decomp.R")

rates <- read.csv("../data/decomp/decomp_rates_lw.csv", na.strings = c("","NA"), 
                  stringsAsFactors=FALSE)
rates1 <- na.omit(rates)
allrates <- rates1 %>% left_join(CN)

# Correlation analysis #
corr.data <- allrates[c(8, 11, 14, 21)]
cor.mat <- cor(corr.data)

library(corrplot)
corrplot(cor.mat, method = "circle")
corrplot(cor.mat, method = "number")

# Modelling #
decomp_rates_lai <- allrates %>% left_join(LAI, by=c("year", "alt", "asp"))

library(lme4)

decomp_1 <- lmer(ldrate ~ LAI + (1|spcode), data=decomp_rates_lai, REML=FALSE)
decomp_2 <- lmer(ldrate ~ asp + LAI + (1|spcode), data=decomp_rates_lai, 
                 REML=FALSE)
decomp_3 <- lmer(ldrate ~ alt + asp + LAI + (1|spcode), data=decomp_rates_lai, 
                 REML=FALSE)
anova(decomp_1,decomp_2, decomp_3)