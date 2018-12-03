## dry-down.R

# 1. Read in data
# 2. Investigate species differences in dry-down intercepts and rates
# 3. Create summary statistics
# 4. Investigate how leaf traits influence dry-down

library(lme4)
#library(plyr)
library(tidyr)
library(dplyr) #must come after plyr
library(stringr)

source("read-flam.R") # for species table
source("read-decomp.R") # for leaf trait data

# function to make nice table of model coefficients and standard errors:
model.coefs <- function(the.mod) {
    betas <- fixef(the.mod)
    Vcov <- vcov(the.mod, useScale = FALSE)
    se <- sqrt(diag(Vcov))
    zval <- betas / se
    pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
    r <- cbind(betas, se, zval, pval)
    return(data.frame(label=rownames(r), r ))
}

# read dry-down data
mc <- read.csv("../data/moisture/dry_down_long.csv", stringsAsFactors=FALSE)
# identify trays vs within tray by time subsamples
mc <- mc %>% separate(rep, c("tray", "subrep"), "_") %>%
  mutate(tray = str_c(spcode, "_", tray))
# average subsamples by tray species and hour
mc <- mc %>% group_by(spcode, hour, tray) %>% summarize(MC_dry=mean(MC_dry), bd = mean(bd))
mc$logMC_dry <-  log(mc$MC_dry)
mc <- left_join(mc, species)


###############################################################################
## Investigate species differences in dry down intercepts and rates
###############################################################################

# model moisture as a function of time
dry.mod <- lmer(log(MC_dry) ~ hour*spcode + (1 | tray), data=mc)
summary(dry.mod)
anova(dry.mod)


# test for pairwise differences and number of distinct groups
library(lsmeans)
cld(lstrends(dry.mod,~ spcode, var = "hour"))
# so there are three slope groups
cld(lsmeans(dry.mod, ~ spcode))
# and 4 intercept groups (but slopes differ so . . .)

###############################################################################
##  extract coefficents:

# model to fit for single species
coefunc <- function(d){
    mod <- lmer(log(MC_dry)~ hour + (1 | tray ), data=d)
    res <- summary(mod)$coefficients
    return(data.frame(logmaxMC = res[1,1], logmaxMC.se = res[1,2],  di= res[2,1], di.se = res[2,2]))
}

mcdis <- mc %>% group_by(spcode) %>% do(coefunc(.)) %>% mutate(maxMC = exp(logmaxMC), maxMC.se=exp(logmaxMC.se))

mc.sum <- mc %>% group_by(spcode) %>% 
      summarise(MC_dry.mean=mean(MC_dry),
                MC_dry.sd=sd(MC_dry),
                bd.mean=mean(bd),
                bd.sd=sd(bd)) 
  
newmc <- merge(mc.sum, mcdis, by="spcode")

###################################################################
## Getting the leaf trait data

decomp.sum3 <- decomp[, c(2, 5:11)] %>% group_by(spcode, year) %>%
                  summarize_each(funs(mean(., na.rm=TRUE),
                                      sd(., na.rm=TRUE))) %>% filter(year==0)

newmctr <- merge(decomp.sum3[, c(1, 3:14)], newmc, by="spcode", sort=F)

###########################################################
# Establishing the influence of leaf traits on dry-down
###########################################################

modmaxMCbulk <- lm(maxMC ~ bd.mean, data=newmctr)
summary(modmaxMCbulk)

modmaxMClt <- lm(maxMC ~ lt_mean + bd.mean, data=newmctr)
summary(modmaxMClt)

anova(modmaxMCbulk, modmaxMClt)

moddibulk <- lm(di ~ bd.mean, data=newmctr)
summary(moddibulk)

moddilt <- lm(di ~ lt_mean + bd.mean, data=newmctr)
summary(moddilt)

anova(moddibulk, moddilt)
