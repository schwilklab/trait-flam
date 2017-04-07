# moistmix-plots

source("theme-opts.R")
source("read-moistmix.r")

## Figure 9: Plotting dry-down

xbreaks <- seq(0, 96, 24)
ybreaks <- seq(0, 400, 25)

ggplot(mmc, aes(hour, MC_dry, colour=spcode)) +
  geom_point(size=1.5) +
  scale_colour_brewer(palette="Set1", name="Litter mixture") +
  xlab("Hours since dry-down") + ylab("Moisture by dry weight (%)") +
  scale_x_continuous(breaks=xbreaks) +
  scale_y_continuous(breaks=ybreaks) +
  theme_bw() + geom_smooth(method="lm", se=FALSE)

ggsave("../results/plots/Dry_down_mix.pdf", width=15, height=10)

# Figure 10: Plotting observed vs predicted (moisture content)

ggplot(obs_pred_mc, aes(predMC_dry, MC_dry, colour=spcode)) +
  geom_point(aes(shape=spcode), size=4) +
  scale_shape_discrete(name="Litter mixture", breaks=mspbreaks) +
  scale_colour_discrete(name="Litter mixture", breaks=mspbreaks) +
  xlab("Predicted moisture content (%)") + ylab("Observed moisture content (%)") +
  theme_bw()+ 
  geom_abline(intercept=0, slope=1)

ggsave("../results/plots/obs_pred_mix.pdf", width=15, height=10)

ggplot(obs_pred_mc.sum, aes(predMC_dry.mean, MC_dry.mean, colour=spcode)) +
  geom_point(aes(shape=spcode, colour=factor(hour)), size=4) +
  scale_shape_discrete(name="Litter mixture", breaks=mspbreaks) +
  scale_colour_discrete(name="Hour since saturation") +
  xlab("Predicted moisture content (%)") + ylab("Observed moisture content (%)") +
  theme_bw()+ 
  geom_abline(intercept=0, slope=1)

ggsave("../results/plots/obs_pred_mix_sum.pdf", width=15, height=10)

# Figure 11 - 13: Plotting observed vs predicted (flammability)

ggplot(obs_pred_flam, aes(pred_spread, spread, colour=spcode)) +
  geom_point(aes(shape=spcode), size=4) +
  scale_shape_discrete(name="Litter mixture", breaks=mspbreaks) +
  scale_colour_discrete(name="Litter mixture", breaks=mspbreaks) +
  xlab("Predicted spread rate (cm/s)") + ylab("Observed spread rate (cm/s)") +
  theme_bw()+ 
  geom_abline(intercept=0, slope=1, linetype=2) + geom_smooth(method="lm",se = F, color = "black", size=1.0) 

ggsave("../results/plots/obs_pred_mix_spread.pdf", width=9, height=6, dpi=ppi)
ggsave("../results/plots/obs_pred_mix_spread.png", width=9, height=6, dpi=ppi)

ggplot(obs_pred_flam, aes(pred_combust, combust, colour=spcode)) +
  geom_point(aes(shape=spcode), size=4) +
  scale_shape_discrete(name="Litter mixture", breaks=mspbreaks) +
  scale_colour_discrete(name="Litter mixture", breaks=mspbreaks) +
  xlab("Predicted maximum flame height (mm)") + ylab("Observed maximum flame height (mm)") +
  theme_bw()+ 
  geom_abline(intercept=0, slope=1, linetype=2) + geom_smooth(method="lm",se = F, color = "black", size=1.0) 

ggsave("../results/plots/obs_pred_mix_combust.pdf", width=9, height=6, dpi=ppi)
ggsave("../results/plots/obs_pred_mix_combust.png", width=9, height=6, dpi=ppi)

ggplot(obs_pred_flam, aes(pred_sustain, sustain, colour=spcode)) +
  geom_point(aes(shape=spcode), size=4) +
  scale_shape_discrete(name="Litter mixture", breaks=mspbreaks) +
  scale_colour_discrete(name="Litter mixture", breaks=mspbreaks) +
  xlab("Predicted duration of flaming combustion (s)") + ylab("Observed duration of flaming combustion (s)") +
  theme_bw()+ 
  geom_abline(intercept=0, slope=1, linetype=2) + geom_smooth(method="lm",se = F, color = "black", size=1.0) 

ggsave("../results/plots/obs_pred_mix_sustain.pdf", width=9, height=6, dpi=ppi)
ggsave("../results/plots/obs_pred_mix_sustain.png", width=9, height=6, dpi=ppi)

ggplot(obs_pred_flam, aes(pred_ignit, t2ignit, colour=spcode)) +
  geom_point(aes(shape=spcode), size=4) +
  scale_shape_discrete(name="Litter mixture", breaks=mspbreaks) +
  scale_colour_discrete(name="Litter mixture", breaks=mspbreaks) +
  xlab("Predicted time to ignition (s)") + ylab("Observed time to ignition (s)") +
  theme_bw()+ 
  geom_abline(intercept=0, slope=1, linetype=2) + geom_smooth(method="lm",se = F, color = "black", size=1.0) 

ggsave("../results/plots/obs_pred_mix_ignit.pdf", width=9, height=6, dpi=ppi)
ggsave("../results/plots/obs_pred_mix_ignit.png", width=9, height=6, dpi=ppi)
