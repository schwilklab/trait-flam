# read data

mmc <- read.csv("../data/moisture/burn_moisture_mixtures.csv")
pred_mmc <- read.csv("../data/moisture/pred_dry_mix.csv")

obs_pred <- merge(pred_mmc, mmc, by=c("hour", "spcode"))

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

ggplot(obs_pred, aes(pred_MC_dry, MC_dry, colour=spcode)) +
  geom_point(aes(shape=spcode), size=3) +
  scale_shape_discrete(name="Species", breaks=mspbreaks) +
  scale_colour_discrete(name="Species", breaks=mspbreaks) +
  xlab("Predicted moisture content (%)") + ylab("Observed moisture content (%)") +
  theme_bw()+ 
  geom_abline(intercept=0, slope=1)

ggsave("../results/plots/mcmix_pred_obs.pdf", width=9, height=5, dpi=ppi)
