## dry-down.R

# read data
mc <- read.csv("../data/moisture/dry_down_long.csv")

# Graph with points instead of lines (and add the lowess line), and species
# names, saved in high resolution for exporting to Word

source("theme-opts.R")

xbreaks <- seq(0, 144, 12)
ybreaks <- seq(0, 700, 50)

p <- ggplot(mc, aes(hour, MC_dry, colour=sp)) +
     geom_point(size=1.5) +
     scale_colour_discrete(name="Species", breaks=spbreaks, labels=labels1) +
     xlab("Hours since dry-down") + ylab("Moisture by dry weight (%)") +
     scale_x_continuous(breaks=xbreaks) +
     scale_y_continuous(breaks=ybreaks)


p + stat_smooth(data=mc, method="loess", se=FALSE, size=1)

ggsave("../results/plots/moisture.png", width=9, height=5, dpi=ppi, units="cm")


# Graph with log of moisture and lm line to see if data is exponential

p + stat_smooth(data=mc, method="lm", se=FALSE, size=1) 

# Fitting an exponential curve to all species

png("moisture.png", width=9*ppi, height=5*ppi, res=ppi)

p + geom_smooth(method="glm", family=gaussian(link="log")) + themeopts + 
  theme_bw()

dev.off()

ggsave("../results/plots/moisture2.png", width=9, height=5, dpi=ppi, units="cm")

# subset by species to get the coefficients (y0 and B) for each curve. Can the
# slope of each curve be a dry down index or dissecation index?

coefunc <- function(mc){
            mod <- lm(log(MC_dry)~hour, data=mc)
            return(coef(mod))
            }
mcdis <- ddply(mc, .(sp), coefunc)
mcdis
