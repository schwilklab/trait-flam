# moistsingles-plots

source("theme-opts.R")
source("dry-down.R")
source("burn-moist.R")

## Figure 1: Plotting dry-down

xbreaks <- seq(0, 144, 24)
ybreaks <- seq(0, 700, 50)

ggplot(mc, aes(hour, MC_dry, group=display.name, color=genus)) +
  geom_point(size=1.2, alpha=0.7, stroke=0) +
  scale_colour_manual(values=schwilkcolors) +
  xlab("Hours since wetting") + ylab("Moisture by dry weight (%)") +
  scale_x_continuous(breaks=xbreaks) +
  scale_y_continuous(breaks=ybreaks) +
  pubtheme.nogridlines +
  geom_smooth(method="glm",
              method.args=list(family=gaussian(link="log")), se=FALSE, size =0.6) +
  theme(legend.position=c(0.75, 0.86),
        legend.title=element_blank(),
        legend.text = element_text(family=fontfamily, size=smsize, face="italic"))
 #       legend.key.height=unit(smsize,"pt"))

ggsave("../results/moisture_ms/fig1_drydown-curves.pdf", width=col1, height=col1, units="cm")

ggplot(mc, aes(hour, MC_dry, group=display.name, color=genus)) +
  geom_point(size=1.2, alpha=0.7, stroke=0) +
  scale_colour_manual(values=schwilkcolors) +
  xlab("Hours since wetting") + ylab("Moisture by dry weight (%)") +
  scale_x_continuous(breaks=xbreaks) +
  scale_y_log10() +
  pubtheme.nogridlines +
  geom_smooth(method="lm", se=FALSE, size =0.6) +
  theme(legend.position=c(0.75, 0.86),
        legend.title=element_blank(),
        legend.text = element_text(family=fontfamily, size=smsize, face="italic"))

ggsave("../results/moisture_ms/fig1_drydown-curves_logged.pdf", width=col1, height=col1, units="cm")

###############################################################################
## Figures 3-4: Plots of flammability and moisture levels

## Flammability and time since wetting by genus
#################################################

burnt1 <- subset(burnt,ignit==1)

ggplot(burnt, aes(hour, t2ignit, color=genus)) +
  geom_jitter(width=2, size=1.5, alpha=0.7, stroke=0) +
  scale_colour_manual(values=schwilkcolors) +
  xlab("Time since wetting (hr)") + ylab("Time to ignition (s)") +
  scale_x_continuous(breaks=xbreaks) +
  scale_y_log10() +
  pubtheme.nogridlines +
  stat_smooth(data=burnt, method="lm", se=FALSE, size=1.2) +
  theme(legend.position=c(0.75, 0.86),
    legend.title=element_blank(),
    legend.text = element_text(family=fontfamily, size=smsize, face="italic"))

ggsave("../results/moisture_ms/fig2_ignit_by_time.pdf", width=col1, height=col1, units="cm")

ggplot(burnt, aes(hour, spread, group=spcode, color=genus)) +
  geom_jitter(width=2, size=1.5, alpha=0.7, stroke=0) +
  scale_colour_manual(values=schwilkcolors) +
  xlab("Time since wetting (hr)") +  ylab("Spread rate (mm/s)") +
  scale_x_continuous(breaks=xbreaks) +
  #scale_y_log10() +
  pubtheme.nogridlines +
  stat_smooth(data=burnt, method="lm", se=FALSE, size=1.2) +
  theme(legend.position=c(.3, .86),
        legend.title=element_blank())

ggsave("../results/moisture_ms/fig3_spread_by_time.pdf", width=col1, height=col1, units="cm")


###############################################################################
## Flammability and moisture content ##
## Individual plots by genus ##

ggplot(burnt, aes(actualMC_dry, t2ignit, color=genus)) +
  geom_point(size=1.5, alpha=0.7, stroke=0) +
  scale_colour_manual(values=schwilkcolors) +
  xlab("Moisture content (%)") + ylab("Seconds to ignition") + 
  scale_y_log10() +
  pubtheme.nogridlines +
  stat_smooth(data=burnt, method="lm", se=FALSE, size=0.8) +
  theme(legend.position=c(.23, .86),
        legend.spacing.y=unit(0,"cm"),
        legend.text = element_text(family=fontfamily, size=smsize-1, face="italic"),
        legend.title=element_blank())
ggsave("../results/moisture_ms/actualMC_ignit.pdf", width=col1, height=col1, unit="cm")


ggplot(burnt, aes(actualMC_dry, spread, color=genus)) +
  geom_point(size=1.5, alpha=0.7, stroke=0) +
  scale_colour_manual(values=schwilkcolors) +
  xlab("Moisture content (%)") + ylab("Spread rate (mm/s)") +
  pubtheme.nogridlines +
  stat_smooth(data=burnt, method="lm", se=FALSE, size=0.8) +
  theme(legend.position=c(.75, .86),
        legend.spacing.y=unit(0,"cm"),
        legend.text = element_text(family=fontfamily, size=smsize-1, face="italic"),
        legend.title=element_blank())
ggsave("../results/moisture_ms/actualMC_spread.pdf", width=col1, height=col1, unit="cm")


ggplot(burnt1, aes(actualMC_dry, spread, colour=genus)) +
  geom_point(size=2) +
  scale_colour_manual(name="Genus", breaks=gnsbreaks, labels=labels3, values=mycolours3) +
  xlab("Moisture content (%)") + ylab("Spread rate (mm/s)") +
  ritatheme +
  stat_smooth(data=burnt1, method="lm", se=FALSE, size=1.5)

ggsave("../results/plots/actualMC_Spread.pdf", width=9, height=6, dpi=ppi)
ggsave("../results/plots/actualMC_Spread.png", width=9, height=6, dpi=ppi)
