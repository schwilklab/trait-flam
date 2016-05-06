## decomp-stats.R

## 1. Do correlation analysis on mass loss, CN and leaf length change
## 2. Model decomposition to tease out the effect of alt, asp, and LAI on leaf 
##    length change

source("read-decomp.R")

rates <- read.csv("../data/decomp/decomp_rates_lw.csv", na.strings = c("","NA"), 
                  stringsAsFactors=FALSE)
rates1 <- na.omit(rates)
allrates <- rates1 %>% left_join(CN)

allrates2.sum <- allrates %>%
                    select(spcode, ldrate, ltdrate, wdrate, CNratio) %>%
                    group_by(spcode) %>% summarise(ldrate.mean = mean(ldrate),
                                                   ldrate.sd = sd(ldrate),
                                                   ltdrate.mean = mean(ltdrate),
                                                   ltdrate.sd = sd(ltdrate),
                                                   wdrate.mean = mean(wdrate),
                                                   wdrate.sd = sd(wdrate),
                                                   CNratio.mean= mean(CNratio),
                                                   CNratio.sd  = sd(CNratio)
                                                    )

# Regression analysis#
allratesfit <- lm(ldrate ~ wdrate + CNratio, data=allrates)
summary(allratesfit)

# Checking the relationships graphically
library(ggplot2)
ggplot(allrates, aes(wdrate, ldrate)) + geom_point() +
  geom_smooth(method="lm", se=FALSE)
ggplot(allrates, aes(CNratio, ldrate)) + geom_point() +
  geom_smooth(method="lm", se=FALSE)
ggplot(allrates, aes(CNratio, wdrate)) + geom_point() +
  geom_smooth(method="lm", se=FALSE)

# Correlation analysis (old) #
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