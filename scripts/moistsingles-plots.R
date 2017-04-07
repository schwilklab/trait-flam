# moistsingles-plots

source("theme-opts.R")
source("dry-down.r")
source("burn-moist.r")

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
  scale_colour_manual(name="Species", labels=labels2,
                      values=cbcolours1) +
  xlab("Hours since dry-down") + ylab("Moisture by dry weight (%)") +
  scale_x_continuous(breaks=xbreaks) +
  scale_y_continuous(breaks=ybreaks) +
  theme_bw() +
  theme(axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12)) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se=FALSE, size = 1.5)

ggsave("../results/plots/dry_down.pdf", width=15, height=10, dpi=ppi)


## Figures 3 & 4: Plotting di and max water retention against traits ##

ggplot(newmctr.avg, aes(lt.mean, dimean)) +
  geom_point(size=3) +
  scale_x_continuous("Particle length / thickness") +
  scale_y_continuous("Desiccation index") + 
  geom_pointrange(aes(ymin=dimean-di.sd,
                      ymax=dimean+di.sd),
                  size=0.5) +
  #geom_errorbarh(aes(xmin = lt.mean-lt.sd,xmax = lt.mean+lt.sd))+
  geom_smooth(method="lm",se = F, color = "black", size=1.0) +
  theme_bw() +
  theme(axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12)) 

ggsave("../results/plots/di_lt.pdf", width=9, height=6, dpi=ppi)
ggsave("../results/plots/di_lt.png", width=9, height=6, dpi=ppi)

ggplot(newmctr.avg, aes(lvol.mean, dimean)) +
  geom_point(size=3) +
  scale_x_continuous(expression(paste("Leaf volume (", mm^3,")")))+
  scale_y_continuous("Desiccation index") + 
  geom_pointrange(aes(ymin=dimean-di.sd,
                      ymax=dimean+di.sd),
                  size=0.5) +
  #geom_errorbarh(aes(xmin = lt.mean-lt.sd,xmax = lt.mean+lt.sd))+
  geom_smooth(method="lm",se = F, color = "black", size=1.0) +
  theme_bw() +
  theme(axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12)) 

ggsave("../results/plots/di_lvol.pdf", width=9, height=6, dpi=ppi)
ggsave("../results/plots/di_lvol.png", width=9, height=6, dpi=ppi)

ggplot(newmctr.avg, aes(lt.mean, maxMCmean)) +
  geom_point(size=3) +
  scale_x_continuous("Particle length / thickness") +
  scale_y_continuous("Maximum water retention (log)") +
  geom_pointrange(aes(ymin=maxMCmean-maxMC.sd,
                      ymax=maxMCmean+maxMC.sd),
                  size=0.5) +
  #geom_errorbarh(aes(xmin = lt_mean-lt_sd,xmax = lt_mean+lt_sd)) +
  geom_smooth(method="lm",se = F, color = "black", size=1.0) +
  theme_bw() +
  theme(axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12)) 

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
  theme_bw() +
  theme(axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12)) 

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
  theme_bw() +
  theme(axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12)) 

ggsave("../results/plots/maxMC_bd.pdf", width=9, height=6, dpi=ppi)
ggsave("../results/plots/maxMC_bd.png", width=9, height=6, dpi=ppi)


## Figures 5-7: Plots of flammability and moisture levels

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

## Individual plots by species ##
ggplot(burnt, aes(MC_dry, t2ignit, colour=spcode)) +
  geom_point(size=1.5) +
  scale_colour_manual(name="Species", breaks=spbreaks, labels=labels2, values=cbcolours1) +
  xlab("Moisture content (%)") + ylab("Seconds to ignition") +
  scale_y_log10() +
  theme_bw() +
  theme(axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12)) +
  stat_smooth(data=burnt, method="lm", se=FALSE, size=1)

ggsave("../results/plots/MC_Ignit.pdf", width=9, height=6, dpi=ppi)
ggsave("../results/plots/MC_Ignit.png", width=9, height=6, dpi=ppi)

burnt1 <- subset(burnt,ignit=1)

ggplot(burnt1, aes(MC_dry, spread, colour=spcode)) +
  geom_point(size=2) +
  scale_colour_manual(name="Species", breaks=spbreaks, labels=labels2, values=cbcolours1) +
  xlab("Moisture content (%)") + ylab("Spread rate (mm/s)") +
  theme_bw() +
  theme(axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12)) +
  stat_smooth(data=burnt1, method="lm", se=FALSE, size=1)

ggsave("../results/plots/MC_Spread.pdf", width=9, height=6, dpi=ppi)
ggsave("../results/plots/MC_Spread.png", width=9, height=6, dpi=ppi)

ggplot(burnt, aes(MC_dry, combust, colour=spcode)) +
  geom_point(size=2) +
  scale_colour_manual(name="Species", breaks=spbreaks, labels=labels2, values=cbcolours1) +
  xlab("Moisture content (%)") + ylab("Maximum flame height (mm)") +
  theme_bw() +
  theme(axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12)) +
  stat_smooth(data=burnt, method="lm", se=FALSE, size=1)

ggsave("../results/plots/MC_Combust.pdf", width=9, height=6, dpi=ppi)
ggsave("../results/plots/MC_Combust.png", width=9, height=6, dpi=ppi)

## Individual plots by genus ##

ggplot(burnt, aes(actualMC_dry, t2ignit, color=genus)) +
  geom_point(size=2) +
  scale_colour_manual(name="Genus", breaks=gnsbreaks, labels=labels3, values=mycolours3) +
  xlab("Moisture content (%)") + ylab("Seconds to ignition") + 
  scale_y_log10() +
  theme_bw() +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=14)) +
  stat_smooth(data=burnt1, method="lm", se=FALSE, fullrange=TRUE, size=1.5)

ggsave("../results/plots/actualMC_Ignit.pdf", width=9, height=6, dpi=ppi)
ggsave("../results/plots/actualMC_Ignit.png", width=9, height=6, dpi=ppi)

burnt1 <- subset(burnt,ignit=1)

ggplot(burnt1, aes(actualMC_dry, spread, colour=genus)) +
  geom_point(size=2) +
  scale_colour_manual(name="Genus", breaks=gnsbreaks, labels=labels3, values=mycolours3) +
  xlab("Moisture content (%)") + ylab("Spread rate (mm/s)") +
  theme_bw() +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=14)) +
  stat_smooth(data=burnt1, method="lm", se=FALSE, size=1.5)

ggsave("../results/plots/actualMC_Spread.pdf", width=9, height=6, dpi=ppi)
ggsave("../results/plots/actualMC_Spread.png", width=9, height=6, dpi=ppi)

ggplot(burnt, aes(actualMC_dry, combust, colour=genus)) +
  geom_point(size=2) +
  scale_colour_manual(name="Genus", breaks=gnsbreaks, labels=labels3, values=mycolours3) +
  xlab("Moisture content (%)") + ylab("Maximum flame height (mm)") +
  theme_bw() +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=14)) +
  stat_smooth(data=burnt1, method="lm", se=FALSE, size=1.5)

ggsave("../results/plots/actualMC_Combust.pdf", width=9, height=6, dpi=ppi)
ggsave("../results/plots/actualMC_Combust.png", width=9, height=6, dpi=ppi)

