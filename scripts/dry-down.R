## dry-down.R

# 1. Read in data
# 2. Investigate species differences in dry-down intercepts and rates
# 3. Create summary statistics
# 4. Investigate how leaf traits influence dry-down

library(lme4)
library(plyr)
library (tidyr)

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

###############################################################################
## Investigate species differences in dry down intercepts and rates
###############################################################################

mc$logMC_dry <-  log(mc$MC_dry)
mc <- left_join(mc, species)

# Fit a nested model using lmer
dry.mod <- lmer(logMC_dry ~ hour*spcode + (1 | rep), data=mc)
summary(dry.mod)

# test for pairwise differences and number of distinct groups
library(lsmeans)
cld(lstrends(dry.mod,~ spcode, var = "hour"))
# so there are three slope groups
cld(lsmeans(dry.mod, ~ spcode))
# and 4 intercept groups (but slopes differ so . . .)

###############################################################################
## AND Rita wanted to extract coefficents: Here goes.

# Now run without intercept just to make extracting coefficents easier. Is
# this safe? Coef vals appear same so I think so.
dry.mod.noint <- lmer(logMC_dry ~ hour*spcode + (1 | rep) -1, data=mc)
dry.mod.coefs <- model.coefs(dry.mod)
overall.slope <- dry.mod.coefs$betas[dry.mod.coefs$label=="hour"]

dry.mod.ints <- dry.mod.coefs %>% filter(grepl("^spcode", label )) %>%
    mutate(spcode = substr(label, 7,10), param = "intercept")
dry.mod.slopes <- dry.mod.coefs %>% filter(grepl("^hour:", label)) %>%
    mutate(spcode = substr(label,12,15), param = "slope")

dry.mod.results <- rbind(dry.mod.ints, dry.mod.slopes) %>% select(-label)
dry.mod.results$param <- revalue(dry.mod.results$param, 
                                 c("intercept"="maxMC", "slope"="di"))

# subset by species to get the coefficients (y0 and B) for each curve.

mc <- mc %>% separate(rep, c("rep", "subrep"), "_")
mc2 <- mc[, c("hour", "rep", "spcode", "MC_dry", "bd")]

mc.sum <- mc2 %>% group_by(spcode, hour, rep) %>% 
  summarise_each(funs(mean(., na.rm=TRUE),sd(., na.rm=TRUE))) 

mc.sum <- left_join(mc.sum, species)

coefunc <- function(mc){
    mod <- lm(log(MC_dry)~hour, data=mc) # you can't ignore your nesting!
    res <- coef(mod)
    return(data.frame(maxMC = res[1], di= res[2]))
}

mcdis <- mc2 %>% group_by(spcode, rep) %>% do(coefunc(.)) 

newmc <- merge(mc.sum, mcdis, by="spcode")

###################################################################
## Getting the leaf trait data

decomp.sum3 <- decomp[, c(2, 5:11)] %>% group_by(spcode, year) %>%
                  summarize_each(funs(mean(., na.rm=TRUE),
                                      sd(., na.rm=TRUE))) %>% filter(year==0)

newmctr <- merge(decomp.sum3[, c(1, 3:14)], newmc, by="spcode", sort=F)

newmctr.avg <- newmctr %>% group_by(spcode) %>%
                summarise(dimean = mean(di),
                          di.sd = sd(di),
                          maxMCmean = mean(maxMC),
                          maxMC.sd = sd(maxMC),
                          l.mean = mean(l_mean),
                          l.sd = mean(l_sd),
                      		w.mean = mean(w_mean), 
                          w.sd = mean(w_sd),
                          t.mean = mean(t_mean), 
                          t.sd = mean(t_sd),
                          lt.mean = mean(lt_mean), 
                          lt.sd = mean(lt_sd),
                          larea.mean = mean(larea_mean),
                          larea.sd = mean(larea_sd),
                          lvol.mean = mean(lvol_mean),
                          lvol.sd = mean(lvol_sd),
                      		bd.mean=mean(bd_mean),
                      		bd.sd = sd(bd_mean))

#flam.sp.avg <- flam.sp.avg[, c(1, 3:4)]
#newmctrbd <- merge(newmctr.avg, flam.sp.avg, by="spcode")

###########################################################
# Establishing the influence of leaf traits on dry-down
###########################################################

modmaxMCbulk <- lm(maxMCmean ~ bd.mean, data=newmctr.avg)
summary(modmaxMCbulk)

modmaxMClt <- lm(maxMCmean ~ lt.mean + bd.mean, data=newmctr.avg)
summary(modmaxMClt)

anova(modmaxMCbulk, modmaxMClt)

moddibulk <- lm(dimean ~ bd.mean, data=newmctr.avg)
summary(moddibulk)

moddilt <- lm(dimean ~ lt.mean + bd.mean, data=newmctr.avg)
summary(moddilt)

anova(moddibulk, moddilt)
