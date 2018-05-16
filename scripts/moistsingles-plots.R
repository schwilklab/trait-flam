# moistsingles-plots

source("theme-opts.R")
source("dry-down.R")
source("burn-moist.R")

## Figure 2: Plotting dry-down

xbreaks <- seq(0, 144, 24)
ybreaks <- seq(0, 700, 50)

ggplot(mc, aes(hour, MC_dry, colour=display.name)) +
  geom_point(size=1.5) +
  scale_colour_manual(name="Species", 
                      values=cbcolours1) +
  xlab("Hours since dry-down") + ylab("Moisture by dry weight (%)") +
  scale_x_continuous(breaks=xbreaks) +
  scale_y_continuous(breaks=ybreaks) +
  ritatheme +
  geom_smooth(method="glm",
              method.args=list(family=gaussian(link="log")), se=FALSE, size =1.5)

ggsave("../results/plots/dry_down.pdf", width=15, height=10, dpi=ppi)

#  Same figure, but with line type by family instead of species.

ggplot(mc, aes(hour, MC_dry, colour=family, linetype=family)) + #linetype added
  geom_point(size=2) +
  scale_colour_manual(name="Family", values=cbcolours1) +
  xlab("Hours since dry-down") + ylab("Moisture by dry weight (%)") +
  scale_x_continuous(breaks=xbreaks) +
  scale_y_continuous(breaks=ybreaks) +
  theme_bw() +
  theme(axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12),
        legend.position=c(0.8, 0.7)) + #added option for moving the legend inside the figure
  geom_smooth(method="glm",
              method.args=list(family=gaussian(link="log")), se=FALSE, size =1.5)


###############################################################################
## Figures 3-4: Plots of flammability and moisture levels

## Flammability and time since wetting ##
## by genus
#########################################

burnt1 <- subset(burnt,ignit==1)

ggplot(burnt, aes(hour, t2ignit, color=genus)) +
  geom_jitter(size=1.5) +
  scale_colour_manual(name="Genus", values=mycolours3) +
  xlab("Time since wetting (hour)") + ylab("Seconds to ignition") +
  scale_x_continuous(breaks=xbreaks) +
  scale_y_log10() +
  ritatheme +stat_smooth(data=burnt, method="lm", se=FALSE, size=1.5)
  

ggsave("../results/plots/hour_Ignit.pdf", width=9, height=6, dpi=ppi)
ggsave("../results/plots/hour_Ignit.png", width=9, height=6, dpi=ppi)

schwilkcolors <- c("#D68D18", "#836B43", "#A0AE6A", "#437683", "#18B0D6")
burnt2 <- droplevels(filter(burnt1, genus %in% c("Abies", "Pinus", "Quercus")))
ggplot(burnt2, aes(hour, spread, colour=genus)) +
  geom_jitter(size=1.5) +
  scale_colour_manual(name="Genus", values=schwilkcolors) +
  xlab("Time since wetting (hour)") + ylab("Spread rate (mm/s)") +
  stat_smooth(data= burnt2, method="lm", se=FALSE, size=1.5, na.omit=TRUE) +
  prestheme +
  theme(legend.text = element_text(face = "italic"),
    legend.justification = c("left", "top"),
        legend.key.height = unit(0.5, "lines"),
        legend.position = c(0, 1),
        legend.title=element_blank())

ggsave("../results/plots/hour_Spread.png", width=10, height=7, unit="cm")

## ggplot(burnt2, aes(hour, consum, colour=genus)) +
##   geom_jitter(size=1.5) +
##   scale_colour_manual(name="Genus", values=schwilkcolors) +
##   xlab("Time since wetting (hour)") + ylab("Consumability") +
##   ritatheme +
##   stat_smooth(method="lm", se=FALSE, size=1.5)
## ggsave("../results/plots/hour_consum.png", width=9, height=6, dpi=ppi)



burnt3 <-  droplevels(filter(burnt1, spcode %in% c("Pije", "Pipo"))) %>% mutate(consum=consum/100.0)
logmod <- glm(consum ~ hour, data=burnt3)
summary(logmod)
burnt3$fitted <- logmod$fitted.values

ggplot(burnt3, aes(hour, consum)) +
  geom_jitter(size=1.5) +
#  scale_colour_manual(name="Genus", values=schwilkcolors) +
  xlab("Time since wetting (hour)") + ylab("Consumability") +
  prestheme +
  geom_smooth(method = "glm", 
  method.args = list(family = "binomial"), 
  se = FALSE, color="black", size=1.5) 
ggsave("../results/plots/hour_consum_logistic.png", width=10, height=7, unit="cm")

###############################################################################
## Flammability and moisture content ##
## Individual plots by genus ##


ggplot(burnt, aes(actualMC_dry, t2ignit, color=genus)) +
  geom_point(size=2) +
  scale_colour_manual(name="Genus", values=mycolours3) +
  xlab("Moisture content (%)") + ylab("Seconds to ignition") + 
  scale_y_log10() + ritatheme +
  stat_smooth(data=burnt, method="lm", se=FALSE, size=1.5)

ggsave("../results/plots/actualMC_Ignit.pdf", width=9, height=6, dpi=ppi)
ggsave("../results/plots/actualMC_Ignit.png", width=9, height=6, dpi=ppi)

ggplot(burnt1, aes(actualMC_dry, spread, colour=genus)) +
  geom_point(size=2) +
  scale_colour_manual(name="Genus", breaks=gnsbreaks, labels=labels3, values=mycolours3) +
  xlab("Moisture content (%)") + ylab("Spread rate (mm/s)") +
  ritatheme +
  stat_smooth(data=burnt1, method="lm", se=FALSE, size=1.5)

ggsave("../results/plots/actualMC_Spread.pdf", width=9, height=6, dpi=ppi)
ggsave("../results/plots/actualMC_Spread.png", width=9, height=6, dpi=ppi)
