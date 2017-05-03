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
  theme(axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12))

ggsave("../results/plots/Dry_down_mix.pdf", width=9, height=6)
ggsave("../results/plots/Dry_down_mix.png", width=9, height=6, dpi=ppi)

# Figure 6: Plotting observed vs predicted (moisture content)

ggplot(obs_pred_mc.sum, aes(predMC_dry.mean, MC_dry.mean, colour=factor(hour))) +
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
ggplot(obs_pred_flam.sum, aes(pred_spread.mean, spread.mean, colour=factor(hour))) +
  geom_point(aes(shape=spcode), size=4) +
  geom_pointrange(aes(ymin=spread.mean-spread.sd,
                      ymax=spread.mean+spread.sd),
                  size=0.5) +
  ylim(0.0, 2.0) +
  scale_shape_discrete(name="Litter mixture", breaks=mspbreaks) +
  scale_colour_discrete(name="Hour since saturation") +
  xlab("Predicted spread rate (cm/s)") + ylab("Observed spread rate (cm/s)") +
  ritatheme+ 
  geom_abline(intercept=0, slope=1, linetype=2)

ggsave("../results/plots/obs_pred_mix_spread.pdf", width=9, height=6, dpi=ppi)
ggsave("../results/plots/obs_pred_mix_spread.png", width=9, height=6, dpi=ppi)

ggplot(obs_pred_flam.sum, aes(pred_ignit.mean, ignit.mean, colour=factor(hour))) +
  geom_point(aes(shape=spcode), size=4) +
  geom_pointrange(aes(ymin=ignit.mean-ignit.sd,
                      ymax=ignit.mean+ignit.sd),
                  size=0.5) +
  ylim(0, 40) +
  scale_shape_discrete(name="Litter mixture", breaks=mspbreaks) +
  scale_colour_discrete(name="Hour since saturation") +
  xlab("Predicted time to ignition (s)") + ylab("Observed time to ignition (s)") +
  ritatheme+ 
  geom_abline(intercept=0, slope=1, linetype=2) 

ggsave("../results/plots/obs_pred_mix_ignit.pdf", width=9, height=6, dpi=ppi)
ggsave("../results/plots/obs_pred_mix_ignit.png", width=9, height=6, dpi=ppi)
