## dry-down.R

library(ggplot2)

# read data
mc <- read.csv("../data/moisture/dry_down_long.csv")

# Graph with points instead of lines (and add the lowess line), and species
# names, saved in high resolution for exporting to Word
ppi <- 300

spbreaks <- c("Abco", "Abma", "Cade", "Pije", "Pila", "Pipo", "Quke", "Segi")
labels <- c("White Fir", "Red Fir", "Incense Cedar", "Jeffrey Pine",
            "Sugar Pine", "Ponderosa Pine", "Black Oak", "Sequoia")
xbreaks <- seq(0, 144, 12)
ybreaks <- seq(0, 700, 50)

p <- ggplot(mc, aes(hour, MC_dry, colour=sp)) +
     geom_point(size=1.5) +
     scale_colour_discrete(name="Species", breaks=spbreaks, labels=labels) +
     xlab("Hours since dry-down") + ylab("Moisture by dry weight (%)") +
     scale_x_continuous(breaks=xbreaks) +
     scale_y_continuous(breaks=ybreaks)


p + stat_smooth(data=mc, method="loess", se=FALSE, size=1)
ggsave("../results/plots/moisture.png", width=9, height=5, dpi=ppi, units="cm")


# Graph with log of moisture and lm line to see if data is exponential
## DWS: Why not fit an exponential, then?
p + stat_smooth(data=mc, method="lm", se=FALSE, size=1) 

# subset by species to get the coefficients (y0 and B) for each curve. Can the
# slope of each curve be a dry down index or dissecation index?
mcdry.lm <- lm(log(MC_dry)~hour, data=mc) 
summary(mcdry.lm)
coef(mcdry.lm)
