library(ggplot2)

mc <- read.csv("dry_down_long.csv")

mc$sp <- factor(mc$sp)


# Graph with points instead of lines (and add the lowess line), and species names, saved in high resolution for exporting to Word

ppi <- 300
png("moisture.png", width=9*ppi, height=5*ppi, res=ppi)

ggplot(mc, aes(hour, MC_dry, colour=cp)) +  geom_point(size=1.5) + 						scale_colour_discrete(name="Species", 
						breaks=c("Abco", "Abma", "Cade", "Pije", "Pila", "Pipo", "Quke", "Segi"), 
						labels=c("White Fir", "Red Fir", "Incense Cedar", "Jeffrey Pine", "Sugar Pine", "Ponderosa Pine", "Black Oak", "Sequoia")) + xlab("Hours since dry-down") + ylab("Moisture by dry weight (%)") + scale_x_continuous(breaks=seq(0, 144, 12)) + scale_y_continuous(breaks=seq(0, 700, 50)) + stat_smooth(data=mc, method="loess", se=FALSE, size=1) # lowess line without standard error area  # Ticks from 0-700, every 50
dev.off()

# Graph with log of moisture and lm line to see if data is exponential

ggplot(mc, aes(hour, log(MC_dry), colour=cp)) +  geom_point(size=1.5) + 						scale_colour_discrete(name="Species", 
						breaks=c("Abco", "Abma", "Cade", "Pije", "Pila", "Pipo", "Quke", "Segi"), 
						labels=c("White Fir", "Red Fir", "Incense Cedar", "Jeffrey Pine", "Sugar Pine", "Ponderosa Pine", "Black Oak", "Sequoia")) + xlab("Hours since dry-down") + ylab("Moisture by dry weight (%)") + scale_x_continuous(breaks=seq(0, 144, 12)) + scale_y_continuous(breaks=seq(0, 700, 50)) + stat_smooth(data=mc, method="lm", se=FALSE, size=1) # lowess line without standard error area  # Ticks from 0-700, every 50
						
	# it is!

mcdry.lm <- lm(log(MC_dry)~hour, data=mc) #subset by species to get the coefficients (y0 and B) for each curve. Can the slope of each curve be a dry down index or dissecation index?
summary(mcdry.lm)
coef(mcdry.lm)