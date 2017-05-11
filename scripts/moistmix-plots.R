# moistmix-plots

source("theme-opts.R")
source("read-moistmix.R")

## Figure 5: Plotting dry-down

xbreaks <- seq(0, 96, 24)
ybreaks <- seq(0, 400, 25)

ggplot(mmc, aes(hour, MC_dry, colour=spcode)) +
  geom_point(size=2) +
  scale_colour_brewer(palette="Set1", name="Litter mixture") +
  xlab("Hours since dry-down") + ylab("Moisture by dry weight (%)") +
  scale_x_continuous(breaks=xbreaks) +
  scale_y_continuous(breaks=ybreaks) +
  theme_bw() + geom_smooth(method="glm",
                           method.args=list(family=gaussian(link="log")), se=FALSE, size =1.5)+
  ritatheme

ggsave("../results/plots/Dry_down_mix.pdf", width=9, height=6)
ggsave("../results/plots/Dry_down_mix.png", width=9, height=6, dpi=ppi)

# Figure 6: Plotting observed vs predicted (moisture content)

ggplot(mmc.sum, aes(MC_dry_pred, MC_dry.mean, colour=factor(hour))) +
  geom_point(aes(shape=spcode), size=4) +
  geom_pointrange(aes(ymin=MC_dry.mean-MC_dry.sd,
                      ymax=MC_dry.mean+MC_dry.sd),
                  size=0.5) +
  scale_shape_discrete(name="Litter mixture", breaks=mspbreaks) +
  scale_colour_discrete(name="Hour since saturation") +
  xlab("Predicted moisture content (%)") + ylab("Observed moisture content (%)") +
  geom_abline(intercept=0, slope=1) +
  ritatheme

ggsave("../results/plots/obs_pred_mix_MCsum.pdf", width=9, height=6)
ggsave("../results/plots/obs_pred_mix_MCsum.png", width=9, height=6, dpi=ppi)

# Figure 7 - 8: Plotting observed vs predicted (flammability)
ggplot(mflam.sum, aes(spread_pred, spread.mean, colour=factor(hour))) +
  geom_point(aes(shape=spcode), size=4) +
  geom_pointrange(aes(ymin=spread.mean-spread.sd,
                      ymax=spread.mean+spread.sd),
                  size=0.5) +
  xlim(0.0, 0.3) +
  scale_shape_discrete(name="Litter mixture", breaks=mspbreaks) +
  scale_colour_discrete(name="Hour since saturation") +
  xlab("Predicted spread rate (cm/s)") + ylab("Observed spread rate (cm/s)") +
  ritatheme+ 
  geom_abline(intercept=0, slope=1, linetype=2)

ggsave("../results/plots/obs_pred_mix_spread.pdf", width=9, height=6, dpi=ppi)
ggsave("../results/plots/obs_pred_mix_spread.png", width=9, height=6, dpi=ppi)

ggplot(mflam.sum, aes(ignit_pred, t2ignit.mean, colour=factor(hour))) +
  geom_point(aes(shape=spcode), size=4) +
  geom_pointrange(aes(ymin=t2ignit.mean-t2ignit.sd,
                      ymax=t2ignit.mean+t2ignit.sd),
                  size=0.5) +
  ylim(0, 40) +
  scale_shape_discrete(name="Litter mixture", breaks=mspbreaks) +
  scale_colour_discrete(name="Hour since saturation") +
  xlab("Predicted time to ignition (s)") + ylab("Observed time to ignition (s)") +
  ritatheme+ 
  geom_abline(intercept=0, slope=1, linetype=2) 

ggsave("../results/plots/obs_pred_mix_ignit.pdf", width=9, height=6, dpi=ppi)
ggsave("../results/plots/obs_pred_mix_ignit.png", width=9, height=6, dpi=ppi)
