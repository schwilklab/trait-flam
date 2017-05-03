# moistsingles-plots

source("theme-opts.R")
source("dry-down.R")
source("burn-moist.R")

# This theme is here for now. I am having trouble with pubtheme and embeded fonts. Will open issue
ritatheme <- theme_grey() +
  theme(axis.title.y = element_text(size = 12, angle = 90, vjust=0.3),
        axis.title.x = element_text(size = 12, vjust=-0.3),
        axis.ticks = element_line(colour = "black"),
        panel.background = element_rect(size = 1.6, fill = NA),
        panel.border = element_rect(size = 1.6, fill=NA),
        axis.text.x  = element_text(size=11, color="black"),
        axis.text.y  = element_text(size=11, color = "black"),
        ## strip.text.x = element_text(family=fontfamily, size = 11, face="italic"),
        ## strip.text.y = element_text(family=fontfamily, size = 11, face="italic"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11, face="italic"),
        legend.key = element_rect(fill=NA),
        panel.grid.major = element_line(colour = "grey90", size = 0.2),
        panel.grid.minor = element_line(colour = "grey95", size =0.5),
        #    panel.grid.minor = element_blank(),
        #    panel.grid.major = element_bank(),
        strip.background = element_rect(fill = "grey80", colour = "grey50")      
  )

## Figure 2: Plotting dry-down

xbreaks <- seq(0, 144, 24)
ybreaks <- seq(0, 700, 50)

p <- ggplot(mc, aes(hour, MC_dry, colour=display.name)) +
  geom_point(size=1.5) +
  scale_colour_brewer(palette="Reds", name="") +
  xlab("Hours since dry-down") + ylab("Moisture by dry weight (%)") +
  scale_x_continuous(breaks=xbreaks) +
  scale_y_continuous(breaks=ybreaks) +
  pubtheme

p.exp <- p + geom_smooth(method="glm", method.args = list(family = "binomial"), se=FALSE, size=1)
p.exp
ggsave("../results/plots/moisture.png", width=9, height=5, dpi=ppi)

# and cut off the y axis due to oaks holding a ton of water:
p.exp + ylim(c(0,350))
ggsave("../results/plots/moisture2.png", width=9, height=5, dpi=ppi)

## Works better ... having issues with fontfamily and geom_smooth
ggplot(mc, aes(hour, MC_dry, colour=display.name)) +
  geom_point(size=1.5) +
  scale_colour_manual(name="Species", 
                      values=cbcolours1) +
  xlab("Hours since dry-down") + ylab("Moisture by dry weight (%)") +
  scale_x_continuous(breaks=xbreaks) +
  scale_y_continuous(breaks=ybreaks) +
  ritatheme +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se=FALSE, size = 1.5)

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

## Figures 3 & 4: Plotting di and max water retention against traits ##
## No longer using this in the paper##

ggplot(newmctr.avg, aes(lt.mean, dimean)) +
  geom_point(size=3) +
  scale_x_continuous("Particle length / thickness") +
  scale_y_continuous("Desiccation index") + 
  geom_pointrange(aes(ymin=dimean-di.sd,
                      ymax=dimean+di.sd),
                  size=0.5) +
  #geom_errorbarh(aes(xmin = lt.mean-lt.sd,xmax = lt.mean+lt.sd))+
  geom_smooth(method="lm",se = F, color = "black", size=1.0) +
  ritatheme

ggsave("../results/plots/di_lt.pdf", width=9, height=6, dpi=ppi)
ggsave("../results/plots/di_lt.png", width=9, height=6, dpi=ppi)

ggplot(newmctr.avg, aes(lt.mean, maxMCmean)) +
  geom_point(size=3) +
  scale_x_continuous("Particle length / thickness") +
  scale_y_continuous("Maximum water retention (log)") +
  geom_pointrange(aes(ymin=maxMCmean-maxMC.sd,
                      ymax=maxMCmean+maxMC.sd),
                  size=0.5) +
  #geom_errorbarh(aes(xmin = lt_mean-lt_sd,xmax = lt_mean+lt_sd)) +
  geom_smooth(method="lm",se = F, color = "black", size=1.0) +
  ritatheme

ggsave("../results/plots/maxMC_lt.pdf", width=9, height=6, dpi=ppi)
ggsave("../results/plots/maxMC_lt.png", width=9, height=6, dpi=ppi)  

ggplot(newmctrbd, aes(bulk.mean, dimean)) +
  geom_point(size=3) +
  scale_x_continuous(expression(paste("Litter bulk density (", g.cm^-3,")"))) +
  scale_y_continuous("Desiccation index") + 
  geom_pointrange(aes(ymin=dimean-di.sd,
                      ymax=dimean+di.sd),
                  size=0.5) +
  #geom_errorbarh(aes(xmin = t_mean-t_sd,xmax = t_mean+t_sd))+
  geom_smooth(method="lm",se = F, color = "black", size=1.0) +
  ritatheme

ggsave("../results/plots/di_bd.pdf", width=9, height=6, dpi=ppi)
ggsave("../results/plots/di_bd.png", width=9, height=6, dpi=ppi)

ggplot(newmctrbd, aes(bulk.mean, maxMCmean)) +
  geom_point(size=3) +
  scale_x_continuous(expression(paste("Litter bulk density (", g.cm^-3,")"))) +
  scale_y_continuous("Maximum water retention (log)") + 
  geom_pointrange(aes(ymin=maxMCmean-maxMC.sd,
                      ymax=maxMCmean+maxMC.sd),
                  size=0.5) +
  #geom_errorbarh(aes(xmin = t_mean-t_sd,xmax = t_mean+t_sd)) +
  geom_smooth(method="lm",se = F, color = "black", size=1.0) +
  ritatheme 

ggsave("../results/plots/maxMC_bd.pdf", width=9, height=6, dpi=ppi)
ggsave("../results/plots/maxMC_bd.png", width=9, height=6, dpi=ppi)

###############################################################################
## Figures 3-4: Plots of flammability and moisture levels

flammoistplot <-  function(resp.var, ylab) {
  pred.var = "MC_dry"
  r <- ggplot(burnt, aes_string(pred.var, resp.var, colour="spcode")) +
    geom_point(size=1.5) +
    scale_colour_manual(name="Species", breaks=spbreaks, labels=labels2,
                        values=cbcolours1) +
    xlab("Moisture content (%)") + ylab(ylab) +
    pubtheme +
    stat_smooth(data=burnt, method="lm", se=FALSE, size=1)
  ggsave(file.path("..", "results", "plots",
                   paste(pred.var, "_", resp.var, ".pdf", sep="")), plot=r)
  return(r)
}

flammoistplot("log(t2ignit)", "Ignitability (s)")
flammoistplot("spread", "Spread rate (mm/s)")
flammoistplot("combust", "Combustability (mm)")
flammoistplot("consum", "Consumability (%)")
flammoistplot("sustain", "Sustainability (s)")

## Flammability and time since wetting ##
## by genus
#########################################

labels3 <- c(expression(italic("Abies")), 
             expression(italic("Calocedrus")), 
             expression(italic("Pinus")),  
             expression(italic("Quercus")), 
             expression(italic("Sequoiadendron")))

gnsbreaks <- c("Abies", "Calocedrus", "Pinus", "Quercus", "Sequoiadendron")

mycolours3 <- c("slateblue1", "green", "turquoise3", "navy", "forestgreen")

burnt1 <- subset(burnt,ignit=1)

ggplot(burnt, aes(hour, t2ignit, color=genus)) +
  geom_jitter(aes(size=actualMC_dry)) +
  labs(size="Moisture content (%)") +
  scale_colour_manual(name="Genus", breaks=gnsbreaks, labels=labels3, values=mycolours3) +
  xlab("Time since wetting (hour)") + ylab("Seconds to ignition") +
  scale_x_continuous(breaks=xbreaks) +
  scale_y_log10() +
  ritatheme +
  stat_smooth(data=burnt, method="lm", se=FALSE, size=1.5)

ggsave("../results/plots/hour_Ignit.pdf", width=9, height=6, dpi=ppi)
ggsave("../results/plots/hour_Ignit.png", width=9, height=6, dpi=ppi)

ggplot(burnt1, aes(hour, spread, colour=genus)) +
  geom_jitter(aes(size=actualMC_dry)) +
  scale_colour_manual(name="Genus", breaks=gnsbreaks, labels=labels3, values=mycolours3) +
  xlab("Time since wetting (hour)") + ylab("Spread rate (mm/s)") +
  ritatheme +
  stat_smooth(data=burnt1, method="lm", se=FALSE, size=1.5)

###############################################################################
## Flammability and moisture content ##
## Individual plots by species ##

ggplot(burnt, aes(MC_dry, t2ignit, colour=genus)) +
  geom_point(size=1.5) +
  scale_colour_manual(name="Species", breaks=spbreaks, labels=labels2, values=cbcolours1) +
  xlab("Moisture content (%)") + ylab("Seconds to ignition") +
  scale_y_log10() +
  ritatheme +
  stat_smooth(data=burnt, method="lm", se=FALSE, size=1)

ggsave("../results/plots/MC_Ignit.pdf", width=9, height=6, dpi=ppi)
ggsave("../results/plots/MC_Ignit.png", width=9, height=6, dpi=ppi)

burnt1 <- subset(burnt,ignit=1)

ggplot(burnt1, aes(actualMC_dry, spread, colour=genus)) +
  geom_point(size=2) +
  scale_colour_manual(name="Species", breaks=spbreaks, labels=labels2, values=cbcolours1) +
  xlab("Moisture content (%)") + ylab("Spread rate (mm/s)") +
  ritatheme +
  stat_smooth(data=burnt1, method="lm", se=FALSE, size=1)

ggsave("../results/plots/MC_Spread.pdf", width=9, height=6, dpi=ppi)
ggsave("../results/plots/MC_Spread.png", width=9, height=6, dpi=ppi)

## Moisture plots by genus ##

ggplot(burnt, aes(actualMC_dry, t2ignit, color=genus)) +
  geom_point(size=2) +
  scale_colour_manual(name="Genus", breaks=gnsbreaks, labels=labels3, values=mycolours3) +
  xlab("Moisture content (%)") + ylab("Seconds to ignition") + 
  scale_y_log10() + ritatheme +
  stat_smooth(data=burnt, method="lm", se=FALSE, fullrange=TRUE, size=1.5)

ggsave("../results/plots/actualMC_Ignit.pdf", width=9, height=6, dpi=ppi)
ggsave("../results/plots/actualMC_Ignit.png", width=9, height=6, dpi=ppi)

burnt1 <- subset(burnt,ignit=1)

ggplot(burnt1, aes(actualMC_dry, spread, colour=genus)) +
  geom_point(size=2) +
  scale_colour_manual(name="Genus", breaks=gnsbreaks, labels=labels3, values=mycolours3) +
  xlab("Moisture content (%)") + ylab("Spread rate (mm/s)") +
  ritatheme +
  stat_smooth(data=burnt1, method="lm", se=FALSE, size=1.5)

ggsave("../results/plots/actualMC_Spread.pdf", width=9, height=6, dpi=ppi)
ggsave("../results/plots/actualMC_Spread.png", width=9, height=6, dpi=ppi)
