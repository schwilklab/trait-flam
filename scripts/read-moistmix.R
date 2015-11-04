# read data
mmc <- read.csv("../data/moisture/burn_moisture_mixtures.csv")

source("theme-opts.R")

xbreaks <- seq(0, 144, 12)
ybreaks <- seq(0, 700, 50)

ggplot(mmc, aes(drying_time, MC_dry, colour=spcode)) +
  geom_point(size=1.5) +
  scale_colour_discrete(name="Species", breaks=mspbreaks) +
  xlab("Hours since dry-down") + ylab("Moisture by dry weight (%)") +
  scale_x_continuous(breaks=xbreaks) +
  scale_y_continuous(breaks=ybreaks) +
  theme_bw()+ 
  geom_smooth(method="glm", family=gaussian(link="log"))

ggsave("../results/plots/moisturemix.png", width=9, height=5, dpi=ppi)
