## decomp-stats-reordered.R


## 1. Model leaf length change for all species and all years to characterize 
##    that change and investigate significant differences among species and 
##    in between years, with elevation and aspect. Do post-hoc test on contrasts.
## 2. Test on pines vs non-pines to confirm different behaviour
## 3. Model and do correlation analysis on mass loss rate, CN and leaf length change

## No longer included: Calculate effect size and overlap coefficient for each 
##                      of the species, between year 0 and year 1
##                     Do pairwise contrasts at the genus level

source("read-decomp.R")

# Result #1: Description of change in particle size (length) over time for 8 species
#           p-value with test statistic, pairwise post-hoc test on interaction term
decomp1 <- decomp
decomp1$year <- factor(decomp1$year)
library(lme4)
lmod <- lmer(l~spcode*year + asp + alt + (1|tag), data=decomp1)
summary(lmod)
library(car)
Anova(lmod, white.adjust=TRUE)
library(lsmeans)
lsmeans(lmod, pairwise~spcode:year)
#lsm.options(pbkrtest.limit = 18624) This will set the computational limit, 
# as without it, I will get a warning that the se and tests may be biased - the
# limit is 3000


# Result #2: Test on pines vs non-pines to confirm different behaviour
#           contrast matrix for pines vs non-pines in lsmeans post-hoc test

decomp.genus$spgen <- ifelse(decomp.genus$genus=="Pinus", "Pine", "noPine")

genusfit <- lmer(l~spgen+ (1|spcode) + (1|tag), data=decomp.genus)
summary(genusfit)

#adjusting for heteroskedasticity
library(car)
Anova(genusfit, white.adjust=TRUE)


# Result #3: Comparision of 3 different metrics to assess decomposition, showing
#           mass loss and C:N are poor predictors of change in length

allratesfit <- lm(ldrate ~ wdrate + CNratio, data=decomp.allrates)
summary(allratesfit)

# Correlation analysis #
corr.data <- decomp.allrates[c( 42, 45, 51)]
cor.mat <- cor(corr.data, use="complete.obs")

library(corrplot)
corrplot(cor.mat, method = "circle")
corrplot(cor.mat, method = "number")


# Result #4: Modelling spread rate and leaf length to predict spread for year 1
#           in decomp-flam.R


### Code no longer needed ###

## 1. Calculate effect size and overlap coefficient for each of the species,
##    between year 0 and year 1

## Effect size via Cohen's D, modified to account for unequal variances and 
##  sample sizes

cohens_d <- function(x, y) {
  lx <- length(x)- 1
  ly <- length(y)- 1
  md  <- abs(mean(x) - mean(y))        ## mean difference (numerator)
  csd <- lx * var(x) + ly * var(y)
  csd <- csd/(lx + ly)
  csd <- sqrt(csd)                     ## common sd computation
  
  cd  <- md/csd                        ## cohen's d
}

x <- subset(decomp, spcode=="Abco" & year=="0", select = l)
y <- subset(decomp, spcode=="Abco" & year=="1", select = l)

res <- cohens_d(x$l, y$l)
res

## Calculating the overlap coeficient:

OVL <- 2*pnorm(-abs(res)/2)
OVL

## 2. Modelling for genus

genusfit <- lmer(l~genus+ (1|spcode) + (1|tag), data=decomp.genus)
summary(genusfit)

#adjusting for heteroskedasticity
library(car)
Anova(genusfit, white.adjust=TRUE)

#Creating the formula to extract the effect size
r2.corr.mer <- function(m) {
  lmfit <-  lm(model.response(model.frame(m)) ~ fitted(m))
  summary(lmfit)$r.squared
}
r2.corr.mer(genusfit)

library(lsmeans)
#lsm.options(pbkrtest.limit = 18624) This will set the computational limit, 
# as without it, I will get a warning that the se and tests may be biased - the
# limit is 3000
lsmeans(genusfit, pairwise~genus)
