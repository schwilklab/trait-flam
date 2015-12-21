## dry-down.R

source("theme-opts.R")
source("read-flam.R") # for species table
source("read-decomp.R") # for leaf trait data

library(lme4)
library(plyr)
library (tidyr)

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
mc <- left_join(mc, species)

xbreaks <- seq(0, 144, 12)
ybreaks <- seq(0, 700, 50)

p <- ggplot(mc, aes(hour, MC_dry, colour=display.name)) +
	    geom_point(size=1.5) +
	    scale_colour_brewer(palette="Reds", name="") +
	    xlab("Hours since dry-down") + ylab("Moisture by dry weight (%)") +
	    scale_x_continuous(breaks=xbreaks) +
	    scale_y_continuous(breaks=ybreaks) +
	    pubtheme

p.exp <- p + geom_smooth(method="glm", family=gaussian(link="log"), se=FALSE, size=1)
p.exp
ggsave("../results/plots/moisture.png", width=9, height=5, dpi=ppi)

# and cut off the y axis due to oaks holding a ton of water:
p.exp + ylim(c(0,350))
ggsave("../results/plots/moisture2.png", width=9, height=5, dpi=ppi)


###############################################################################
## Investigate species differences in dry down intercepts and rates
###############################################################################
ggplot(mc, aes(hour, log10(MC_dry), colour=display.name)) +
    geom_point() +
    #scale_colour_brewer(palette="Reds", name="") +
    geom_smooth(method="lm", se=FALSE, size=1)

mc$logMC_dry <-  log(mc$MC_dry)

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

mc2 <- mc %>% separate(rep, c("rep", "subrep"), "_")
mc2 <- mc2[, c(1, 2, 6, 8, 13, 14)]

mc2.sum <- mc2 %>% group_by(spcode, hour, rep, display.name) %>% 
  summarise_each(funs(mean(., na.rm=TRUE),sd(., na.rm=TRUE)))

coefunc <- function(mc){
    mod <- lm(log(MC_dry)~hour, data=mc) # you can't ignore your nesting!
    res <- coef(mod)
    return(data.frame(maxMC = res[1], di= res[2]))
}

mcdis <- mc2 %>% group_by(spcode, rep) %>% do(coefunc(.)) 

newmc <- merge(mc2.sum, mcdis, by="spcode")

## Getting the leaf trait data

source("./read-decomp.R")

decomp.sum3 <- decomp[, c(2, 5:11)] %>% group_by(spcode, year) %>%
                  summarize_each(funs(mean(., na.rm=TRUE),
                                      sd(., na.rm=TRUE)))

decomp.sum4 <- decomp.sum3 %>% filter(year==0)

newmctr <- merge(decomp.sum4[, c(1, 3:14)], mcdis, by="spcode", sort=F)

newmctr.avg <- newmctr %>% group_by(spcode) %>%
                summarise(dimean = mean(di),
                          maxMCmean = mean(maxMC),
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
                          lvol.sd = mean(lvol_sd))

flam.sp.avg <- flam.sp.avg[, c(1, 3:4)]
newmctrbd <- merge(newmctr.avg, flam.sp.avg, by="spcode")

## Plotting against traits ##

ggplot(newmctr.avg, aes(lt_mean, dimean)) +
  geom_point(size=3) +
  scale_x_continuous("Particle length / thickness") +
  scale_y_continuous("Desiccation index") + 
  #geom_errorbarh(aes(xmin = lt_mean-lt_sd,xmax = lt_mean+lt_sd))+
  geom_smooth(method="lm",se = F, color = "black", size=1.0) +
  pubtheme

ggsave("../myresults/di_lt.pdf", width=9, height=6, dpi=ppi)
ggsave("../myresults/di_lt.png", width=9, height=6, dpi=ppi)

ggplot(newmctr.avg, aes(lt_mean, maxMCmean)) +
  geom_point(size=3) +
  scale_x_continuous("Particle length / thickness") +
  scale_y_continuous("Maximum water retention (log)") + 
  #geom_errorbarh(aes(xmin = lt_mean-lt_sd,xmax = lt_mean+lt_sd)) +
  geom_smooth(method="lm",se = F, color = "black", size=1.0) +
  pubtheme

ggsave("../myresults/maxMC_lt.pdf", width=9, height=6, dpi=ppi)
ggsave("../myresults/maxMC_lt.png", width=9, height=6, dpi=ppi)  

ggplot(newmctrbd, aes(bulk.mean, dimean)) +
  geom_point(size=3) +
  scale_x_continuous("Litter bulk density (", gcm^-3,")") +
  scale_y_continuous("Desiccation index") + 
  #geom_errorbarh(aes(xmin = t_mean-t_sd,xmax = t_mean+t_sd))+
  geom_smooth(method="lm",se = F, color = "black", size=1.0) +
  pubtheme

ggsave("../myresults/di_bd.pdf", width=9, height=6, dpi=ppi)
ggsave("../myresults/di_bd.png", width=9, height=6, dpi=ppi)

ggplot(newmctrbd, aes(bulk.mean, maxMCmean)) +
  geom_point(size=3) +
  scale_x_continuous("Litter bulk density (", gcm^-3,")") +
  scale_y_continuous("Maximum water retention (log)") + 
  #geom_errorbarh(aes(xmin = t_mean-t_sd,xmax = t_mean+t_sd)) +
  geom_smooth(method="lm",se = F, color = "black", size=1.0) +
  pubtheme

ggsave("../myresults/maxMC_bd.pdf", width=9, height=6, dpi=ppi)
ggsave("../myresults/maxMC_bd.png", width=9, height=6, dpi=ppi)
