## dry-down.R

# read data
mc <- read.csv("../data/moisture/dry_down_long.csv")

# Graph with points instead of lines (and add the lowess line), and species
# names, saved in high resolution for exporting 

source("theme-opts.R")

xbreaks <- seq(0, 144, 12)
ybreaks <- seq(0, 700, 50)

p <- ggplot(mc, aes(hour, MC_dry, colour=spcode)) +
            geom_point(size=1.5) +
            scale_colour_discrete(name="Species", breaks=spbreaks, labels=labels1) +
            xlab("Hours since dry-down") + ylab("Moisture by dry weight (%)") +
            scale_x_continuous(breaks=xbreaks) +
            scale_y_continuous(breaks=ybreaks) +
            theme_bw()


p + stat_smooth(data=mc, method="loess", se=FALSE, size=1)

ggsave("../results/plots/moisture.png", width=9, height=5, dpi=ppi)


# Graph with log of moisture and lm line to see if data is exponential

p + stat_smooth(data=mc, method="lm", se=FALSE, size=1) 

# Fitting an exponential curve to all species

png("moisture.png", width=9*ppi, height=5*ppi, res=ppi)

p + geom_smooth(method="glm", family=gaussian(link="log")) 
  + themeopts 
  + theme_bw()

dev.off()

ggsave("../results/plots/moisture2.png", width=9, height=5, dpi=ppi)

# subset by species to get the coefficients (y0 and B) for each curve. Can the
# slope of each curve be a dry down index or dissecation index?

coefunc <- function(mc){
            mod <- lm(log(MC_dry)~hour, data=mc)
            return(coef(mod))
            }
mcdis <- ddply(mc, .(spcode), coefunc)
mcdis

## K-means cluster analysis to determine number of groups

mcdis <- scale(mcdis[-1])

wss <- (nrow(mcdis)-1)*sum(apply(mcdis, 2, var))

for (i in 2:8) wss[i] <- sum(kmeans(mcdis, centers=i)$withinss)

plot(1:8, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

fit <- kmeans(mcdis, 4) #of clusters from above

aggregate(mcdis, by=list(fit$cluster), FUN=mean)

mcdis <- data.frame(mcdis, fit$cluster)

library(cluster)

clusplot(mcdis, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

library(fpc)

plotcluster(mcdis, fit$cluster)