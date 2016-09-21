## decomp-stats.R

## 1. Calculate effect size and overlap coefficient for each of the species, between year 0 and year 1
## 2. Do correlation analysis on mass loss, CN and leaf length change
## 3. Model decomposition to tease out the effect of alt, asp, and LAI on leaf 
##    length change

source("read-decomp.R")

## 1. Calculate effect size and overlap coefficient for each of the species, between year 0 and year 1

## Effect size via Cohen's D, modified to account for unequal variances and sample sizes

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

# Checking the relationships graphically
library(ggplot2)
ggplot(decomp.allrates, aes(wdrate, ldrate)) + geom_point() +
  geom_smooth(method="lm", se=FALSE)
ggplot(decomp.allrates, aes(CNratio, ldrate)) + geom_point() +
  geom_smooth(method="lm", se=FALSE)
ggplot(decomp.allrates, aes(CNratio, wdrate)) + geom_point() +
  geom_smooth(method="lm", se=FALSE)

# Correlation analysis #
#corr.data <- decomp.allrates[c(42, 45, 51)]
#cor.mat <- cor(corr.data, use="complete.obs")

#library(corrplot)
#corrplot(cor.mat, method = "circle")
#corrplot(cor.mat, method = "number")

### Modelling ###

## Modelling for genus
library(lme4)
genusfit <- lmer(l~genus+ (1|spcode), data=decomp.genus)
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

#To provide details on pariwise comparison of the genus

library(lsmeans)
#lsm.options(pbkrtest.limit = 18624) This will set the computational limit, as without it,
# I will get a warning that the se and tests may be biased - the limit is 3000
lsmeans(genusfit, pairwise~genus)

# Modelling the decomposition rates

allratesfit <- lm(ldrate ~ wdrate + CNratio, data=decomp.allrates)
summary(allratesfit)

library(lme4)

decomp_1 <- lmer(ldrate ~ asp + (1|spcode), data=decomp.allrates, 
                 REML=FALSE)
decomp_2 <- lmer(ldrate ~ alt + asp + (1|spcode), data=decomp.allrates, 
                 REML=FALSE)
decomp_3 <- lmer(ldrate ~ (1|spcode), data=decomp.allrates, 
              REML=FALSE)
anova(decomp_1,decomp_2, decomp_3)

