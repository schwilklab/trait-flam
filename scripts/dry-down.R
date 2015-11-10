## dry-down.R

source("theme-opts.R")
source("read-flam.R") # for species table
source("read-decomp.R") # for leaf trait data

library(lme4)

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




###############################################################################
## DWS: Rita's code below. Not not sure what this does as what exactly is kmeans
## space? Why include decomp results in this? Simplest it seems to me is simply
## look at figure of dry down rates. There are 3-6 groups depending on how fine
## one wants to go and depending on how one weights intercepts vs slopes.


# subset by species to get the coefficients (y0 and B) for each curve. Can the
# slope of each curve be a dry down index or dissecation index?

coefunc <- function(mc){
    mod <- lm(log(MC_dry)~hour, data=mc) # you can't ignore your nesting!
    res <- coef(mod)
    return(data.frame(maxMC = res[1], di= res[2]))
}

mcdis <- mc %>% group_by(spcode, rep) %>% do(coefunc(.))

newmc <- merge(mc[, c(1, 5, 7)], mcdis[, c(1, 2, 3, 4)], by="spcode")

source("./read-decomp.R")
newmctr <- merge(subset(decomp.sum, year=="0"), newmc, by="spcode", sort=F)

newmctr.avg <- newmctr %>% group_by(spcode) %>%
    summarise(MCmean = mean(MC_dry),
              dimean = mean(di),
              maxMCmean = mean(maxMC),
              l.mean = mean(l_mean),
              l.sd = mean(l_sd),
              larea.mean = mean(larea_mean),
              larea.sd = mean(larea_sd),
              lvol.mean = mean(lvol_mean),
              lvol.sd = mean(lvol_sd))

## K-means cluster analysis to determine number of groups
## newmc.avg <- scale(newmctr.avg[-1])
## wss <- (nrow(newmc.avg)-1)*sum(apply(newmc.avg, 2, var))
## # for (i in 2:8) wss[i] <- sum(kmeans(newmc.avg, centers=i)$withinss)
## plot(1:8, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
## fit <- kmeans(newmc.avg, 4) #of clusters from above
## aggregate(newmc.avg, by=list(fit$cluster), FUN=mean)
## newmc.avg <- data.frame(newmc.avg, fit$cluster)
## library(cluster)
## clusplot(newmc.avg, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
