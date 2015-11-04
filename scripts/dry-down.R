## dry-down.R

source("theme-opts.R")
source("read-flam.R") # for species table

# read data
mc <- read.csv("../data/moisture/dry_down_long.csv", stringsAsFactors=FALSE)
mc <- left_join(mc, species)

xbreaks <- seq(0, 144, 12)
ybreaks <- seq(0, 700, 50)

p <- ggplot(mc, aes(hour, MC_dry, colour=display.name)) +
    geom_point(size=1.5) +
    scale_colour_discrete(name="") +
    xlab("Hours since dry-down") + ylab("Moisture by dry weight (%)") +
    scale_x_continuous(breaks=xbreaks) +
    scale_y_continuous(breaks=ybreaks) +
    pubtheme

p <- p+ geom_smooth(method="glm", family=gaussian(link="log"), se=FALSE)
p
ggsave("../results/plots/moisture.png", width=9, height=5, dpi=ppi)
# and cut off the y axis due to oaks holding a ton of water:
p + ylim(c(0,350))
ggsave("../results/plots/moisture2.png", width=9, height=5, dpi=ppi)


# subset by species to get the coefficients (y0 and B) for each curve. Can the
# slope of each curve be a dry down index or dissecation index?

library(plyr)

coefunc <- function(mc){
            mod <- lm(log(MC_dry)~hour, data=mc)
            return(coef(mod))
            }

mcdis <- ddply(mc, .(spcode, rep), coefunc)
mcdis

names(mcdis)[names(mcdis)=="(Intercept)"] <- "maxMC"
names(mcdis)[names(mcdis)=="hour"] <- "di"

newmc <- merge(mc[, c(1, 5, 7)], mcdis[, c(1, 2, 3, 4)], by="spcode")

newmctr <- merge(subset(decomp.sum2, year=="0"), newmc, by="spcode", sort=F)

newmctr.avg <- ddply(newmctr, .(spcode), summarise, 
                     MCmean = mean(MC_dry), 
                     dimean = mean(di),
                     maxMCmean = mean(maxMC),
                     l.mean = mean(l.mean), 
                     l.sd = mean(l.sd),
                     larea.mean = mean(larea.mean),
                     larea.sd = mean(larea.sd),
                     lvol.mean = mean(lvol.mean),
                     lvol.sd = mean(lvol.sd))

## K-means cluster analysis to determine number of groups

newmc.avg <- scale(newmc.avg[-1])

wss <- (nrow(newmc.avg)-1)*sum(apply(newmc.avg, 2, var))

for (i in 2:8) wss[i] <- sum(kmeans(newmc.avg, centers=i)$withinss)

plot(1:8, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

fit <- kmeans(newmc.avg, 4) #of clusters from above

aggregate(newmc.avg, by=list(fit$cluster), FUN=mean)

newmc.avg <- data.frame(newmc.avg, fit$cluster)

library(cluster)

clusplot(newmc.avg, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
