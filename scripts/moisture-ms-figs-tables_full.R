# Figures for moisture manuscript
# 2019-05-13


library(xtable)
library(ggrepel)

RESULTS <- "../ms/figs_tables/"

source("theme-opts.R")
source("dry-down.R")
source("burn-moist.R")

## Figure 1: Plotting dry-down
xbreaks <- seq(0, 144, 24)
ybreaks <- seq(0, 700, 50)

ggplot(mc, aes(hour, MC_dry, group=display.name, color=genus)) +
  geom_point(size=1.2, alpha=0.7, stroke=0) +
  geom_smooth(method="glm",
              method.args=list(family=gaussian(link="log")), se=FALSE, size =0.6) +
  scale_colour_manual(values=schwilkcolors) +
  xlab("Hours since wetting") + ylab("Moisture by dry weight (%)") +
  scale_x_continuous(breaks=xbreaks) +
  scale_y_continuous(breaks=ybreaks) +
  pubtheme.nogridlines +
  theme(legend.position=c(0.75, 0.86),
        legend.title=element_blank(),
        legend.text = element_text(family=fontfamily, size=smsize, face="italic"))
 #       legend.key.height=unit(smsize,"pt"))

ggsave(file.path(RESULTS, "fig1_drydown-curves.pdf"), width=col1, height=col1, units="cm")

ggplot(mc, aes(hour, MC_dry, group=display.name, color=genus)) +
  geom_point(size=1.2, alpha=0.7, stroke=0) +
  geom_smooth(method="lm", se=FALSE, size =0.6) +
  scale_colour_manual(values=schwilkcolors) +
  xlab("Hours since wetting") + ylab("Moisture by dry weight (%)") +
  scale_x_continuous(breaks=xbreaks) +
  scale_y_log10() +
  pubtheme.nogridlines +
  theme(legend.position=c(0.75, 0.86),
        legend.title=element_blank(),
        legend.text = element_text(family=fontfamily, size=smsize, face="italic"))

ggsave(file.path(RESULTS, "fig2_drydown-curves_logged.pdf"), width=col1, height=col1, units="cm")

## # stats for fig 1
tab1 <- nice(dry.mixed)
names(tab1)[4] <- "p value"
#tab1 <- tab1[ ,2:4]
tab1 <- xtable(tab1)

print(tab1, file=file.path(RESULTS, "drydown-tab.ltx"),  booktabs=TRUE, floating=FALSE,
      include.rownames=FALSE)

# SI table
tabS1 <- summary(dry.mod)$coefficient
print(xtable(tabS1), file=file.path(RESULTS, "drydown-coef-tab.ltx"),  booktabs=TRUE, floating=FALSE)

tabS2 <- dry.emm
print(xtable(tabS2), file=file.path(RESULTS, "drydown-emmeans.ltx"),  booktabs=TRUE, floating=FALSE,
      include.rownames=FALSE)


## Figure S1
dry.mod <- lmer(log(MC_dry) ~ hour*display.name + (1 | tray), data=mc)
dry.emm <- emmeans(dry.mod, "display.name")
figS1 <- plot(dry.emm) + #, comparisons=TRUE) +
  xlab("Estimated marginal mean (log water content)") +
  ylab("Species") +
  pubtheme.nogridlines +
  theme(axis.text.y = element_text(face = "italic"))
figS1  
ggsave(file.path(RESULTS, "figS1_max_water_emmeans.pdf"), width=col2, height=col1, units="cm")

dry.trends <- emtrends(dry.mod, "display.name", var="hour")
figS2 <- plot(dry.trends) + #, comparisons=TRUE) +
  xlab(expression(paste("Estimated marginal mean dessication rate (", hr^-1, ")"))) +
  ylab("Species") +
  pubtheme.nogridlines +
  theme(axis.text.y = element_text(face = "italic"))
figS2
ggsave(file.path(RESULTS, "figS2_dessication_emmeans.pdf"), width=col2, height=col1, units="cm")



### Traits and drydown:

# 2011 trait data:
oldtraits <- read.csv("../data/moisture/traits.csv", stringsAsFactors=FALSE)

#mtrait <- left_join(newmctr, species)
mtrait <- left_join(mc.sum, flam.sp.avg)
mtrait <- left_join(mtrait, oldtraits)

## bulk density and max retention
## ggplot(mtrait, aes(bulk.mean, maxMC)) +
##   geom_point(size=1.2, alpha=0.7, stroke=0) +
##   geom_errorbar(aes(ymin=maxMC+maxMC.se, ymax=maxMC-maxMC.se))+
##   geom_errorbarh(aes(xmin=bulk.mean+(bulk.sd/sqrt(6)), xmax=bulk.mean-(bulk.sd/sqrt(6))))+
##   geom_label_repel(aes(label=display.name),
##                   nudge_x=0.02,
##                   nudge_y=0.5,
##                    fontface="italic") +
##   scale_colour_manual(values=schwilkcolors) +
##   scale_y_log10() +
##   xlab(expression(paste("Bulk density (", g/cm^3, ")")) ) +
##   ylab("Maximum moisture retention by dry weight ln(%)") +
##   pubtheme.nogridlines


## leaf area and max retention
ggplot(mtrait, aes(SLA, maxMC)) +
  geom_point(size=3) +
#  geom_errorbar(aes(ymin=maxMC+maxMC.se, ymax=maxMC-maxMC.se))+
#  geom_errorbarh(aes(xmin=leaf.area.mean+(leaf.area.sd/sqrt(6)), xmax=leaf.area.mean-(leaf.area.sd/sqrt(6))))+
  geom_label_repel(aes(label=display.name),
                  nudge_x=0.02,
                  nudge_y=-0.05,
                  fontface="italic") +
#  scale_y_log10() + 
#  scale_colour_manual(values=schwilkcolors) +
  xlab(expression(paste("Specific leaf area (", cm^3/g, ")" ))) +
  ylab("Maximum moisture retention by dry weight ln(%)") +
  pubtheme.nogridlines
ggsave(file.path(RESULTS, "fig3-SLA-maxMC.pdf"), width=col2, height=col1*1.5, units="cm")


## bulk density and dessication
ggplot(mtrait, aes(bd.mean, abs(di))) +
  geom_point(size=3, stroke=0) +
#  geom_errorbar(aes(ymin=abs(di)+di.se, ymax=abs(di)-di.se))+
#  geom_errorbarh(aes(xmin=bd.mean+(bd.sd/sqrt(6)), xmax=bd.mean-(bd.sd/sqrt(6))))+
  scale_colour_manual(values=schwilkcolors) +
  xlab(expression(paste("Bulk density (", g/cm^3,")"))) +
  ylab(expression(paste("Desiccation rate (", hr^-1, ")"))) +
 geom_label_repel(aes(label=display.name),
                  nudge_x=0.005,
                  nudge_y=-0.001,
                  fontface="italic") +
  pubtheme.nogridlines

ggsave(file.path(RESULTS, "fig4_di_by_bd.pdf"), width=col2, height=col1*1.5, units="cm")

# simple model:
cor(mtrait$bulk.mean, mtrait$SLA)

mc.mod <- lm(maxMC ~  SLA * density, data=mutate(mtrait, density=bulk.mean))
summary(mc.mod)
anova(mc.mod)

di.mod <- lm(di ~   density * SLA, data=mutate(mtrait, density=bulk.mean))
summary(di.mod)
anova(di.mod)

# stats for figures above  TODO
tab2 <- anova(mc.mod)
names(tab2)[5] <- "p value"
print(xtable(tab2), file=file.path(RESULTS, "mc-anova.ltx"),  booktabs=TRUE, floating=FALSE,
      include.rownames=FALSE)


tab3 <- anova(di.mod)
names(tab3)[5] <- "p value"
print(xtable(tab3), file=file.path(RESULTS, "di-anova.ltx"),  booktabs=TRUE, floating=FALSE,
      include.rownames=FALSE)


###############################################################################
## Figures 3-4: Plots of flammability and moisture levels

## Flammability and time since wetting by genus
#################################################
oldflam <- read.csv("../data/burn-trials/flamdt.csv") %>% left_join(species) %>%
  mutate(hour=144, vpd=2.44, t2ignit=ignit, combust=combus) #, actualMC_dry=5)
b <- oldflam %>% filter(nchar(as.character(spcode)) < 5) %>% bind_rows(burnt)

# fix coding errors in data:
b <- b %>% mutate(didburn = !(ignit==0 & spread==0 & consum <0.000000001),
                  consum = case_when(consum==100 ~ 0, TRUE ~ consum),
                  taxon = case_when(genus=="Sequoiadendron" |
                                      genus=="Calocedrus" ~ "Cupressaceae", TRUE ~ genus))
b.burned <- filter(b, didburn)

b.burned.scaled <- b.burned %>%  mutate_if(is.numeric, scale)


###############################################################################
## Flammability and moisture content ##
## Individual plots by genus ##

# ignition
ggplot(b, aes(actualMC_dry, t2ignit, color=taxon)) +
  geom_point(size=1.5, alpha=0.7, stroke=0) +
  scale_colour_manual(values=schwilkcolors) +
  xlab("Moisture content (%)") + ylab("Seconds to ignition") + 
  scale_y_log10() +
  pubtheme.nogridlines +
  stat_smooth(data=b, method="lm", se=FALSE, size=0.8) +
  theme(legend.position=c(.23, .86),
        legend.spacing.y=unit(0,"cm"),
        legend.text = element_text(family=fontfamily,
                                   size=smsize-1, face="italic"),
        legend.title=element_blank())
ggsave(file.path(RESULTS, "actualMC_ignit.pdf"), width=col1, height=col1, unit="cm")

ignit.moist.mod <- lmer(t2ignit ~ actualMC_dry + taxon + taxon:hour +
                          vpd + (1 | spcode), data=b.burned.scaled) 
summary(ignit.moist.mod)
anova(modignit1)

ignit.moist.mod.mixed <- lm(t2ignit ~ actualMC_dry + taxon + taxon:hour + vpd,
                            data=b.burned.scaled) 
anova(ignit.moist.mod.mixed)

## tab5 <- nice(modignit.mixed)
## names(tab5)[4] <- "p value"
## print(xtable(tab5), file=file.path(RESULTS, "ignit-anova.ltx"),  booktabs=TRUE, floating=FALSE,
##       include.rownames=FALSE)

## tabS4 <- summary(modignit)$coefficient
## print(xtable(tabS4), file=file.path(RESULTS, "ignit-coef-tab.ltx"),  booktabs=TRUE, floating=FALSE)

# spread rate

ggplot(b.burned, aes(actualMC_dry, spread, color=taxon)) +
  geom_point(size=1.5, alpha=0.7, stroke=0) +
  scale_colour_manual(values=schwilkcolors) +
  xlab("Moisture content (%)") + ylab("Spread rate (mm/s)") +
  pubtheme.nogridlines +
  stat_smooth(data=b.burned, method="lm", se=FALSE, size=0.8, drop=TRUE) +
  theme(legend.position=c(.75, .86),
        legend.spacing.y=unit(0,"cm"),
        legend.text = element_text(family=fontfamily, size=smsize-1, face="italic"),
        legend.title=element_blank())
ggsave(file.path(RESULTS, "actualMC_spread.pdf"), width=col1, height=col1, unit="cm")


spread.moist.mod <- lm(spread ~ actualMC_dry + taxon + taxon:hour + vpd,
                         data=b)

#                         data=filter(b.burned, taxon != "Cupressaceae")) 
summary(spread.moist.mod)
anova(spread.moist.mod)

tab.spread.moist <- anova(spread.moist.mod)
names(tab.spread.moist)[5] <- "p value"
print(xtable(tab.spread.moist), file=file.path(RESULTS, "spread-moist-anova.ltx"),
      booktabs=TRUE, floating=FALSE, include.rownames=FALSE)

tab.spread.moist.coef <- summary(spread.moist.mod)
print(xtable(tab.spread.moist.coef), file=file.path(RESULTS, "spread-moist-coef.ltx"),  booktabs=TRUE, floating=FALSE)


# flame height
ggplot(b.burned, aes(actualMC_dry, combust, color=taxon)) +
  geom_point(size=1.5, alpha=0.7, stroke=0) +
  scale_colour_manual(values=schwilkcolors) +
  xlab("Moisture content (%)") + ylab("Flame height (mm)") + 
  pubtheme.nogridlines +
  stat_smooth(data=b.burned, method="lm", se=FALSE, size=0.8) +
  theme(legend.position=c(.806, .86),
        legend.spacing.y=unit(0,"cm"),
        legend.text = element_text(family=fontfamily, size=smsize-1, face="italic"),
        legend.title=element_blank())
ggsave(file.path(RESULTS, "actualMC_flameheight.pdf"), width=col1, height=col1, unit="cm")


# Consumability
ggplot(b.burned, aes(actualMC_dry, consum, color=taxon)) +
  geom_point(size=1.5, alpha=0.7, stroke=0) +
  scale_colour_manual(values=schwilkcolors) +
  xlab("Moisture content (%)") + ylab("Fuel consumed (%)") + 
  pubtheme.nogridlines +
  stat_smooth(data=b.burned, method="lm", se=FALSE, size=0.8) +
  theme(legend.position=c(.807, .86),
        legend.spacing.y=unit(0,"cm"),
        legend.text = element_text(family=fontfamily, size=smsize-1, face="italic"),
        legend.title=element_blank())
ggsave(file.path(RESULTS, "actualMC_consume.pdf"), width=col1, height=col1, unit="cm")



### Flammability by time since wetting


# spread rate by time
ggplot(b.burned, aes(hour, spread, color=taxon)) +
  geom_jitter(width=2, size=1.5, alpha=0.7, stroke=0) +
  geom_smooth(method="lm", se=FALSE, size=1.2) +
  scale_colour_manual(values=schwilkcolors, drop=TRUE) +
  xlab("Time since wetting (hr)") +  ylab("Spread rate (mm/s)") +
  scale_x_continuous(breaks=xbreaks) +
  #scale_y_log10() +
  pubtheme.nogridlines +
  theme(legend.position=c(.3, .86),
        legend.title=element_blank())
ggsave(file.path(RESULTS, "fig5_spread_by_time.pdf"), width=col1, height=col1, units="cm")

spread.time.mod <- lmer(spread ~ hour + taxon + taxon:hour + vpd + (1 | spcode), data=b.burned.scaled) 
summary(spread.time.mod)
anova(spread.time.mod)

spread.mod.mixed <- mixed(spread ~ hour + taxon + taxon:hour + vpd + (1 | spcode), data=b.burned.scaled) 
anova(spread.mod.mixed)

## spread.time.mod.lm <- lm(spread ~ hour + taxon + taxon:hour + vpd, data=b.burned) 
## anova(spread.time.mod.lm)

tab.spread.time <- nice(spread.mod.mixed)
names(tab.spread.time)[4] <- "p value"
print(xtable(tab.spread.time), file=file.path(RESULTS, "spreadrate-anova.ltx"),
      booktabs=TRUE, floating=FALSE, include.rownames=FALSE)

tab.spread.time.coef <- summary(spread.mod.mixed)$coefficient
print(xtable(tab.spread.time.coef), file=file.path(RESULTS, "spreadrate-coef-tab.ltx"),  booktabs=TRUE, floating=FALSE)


# Consumability by time since wetting
ggplot(b.burned, aes(hour, consum, color=taxon)) +
  pubtheme.nogridlines +
  theme(legend.position=c(.806, .35),
        legend.spacing.y=unit(0,"cm"),
        legend.text = element_text(family=fontfamily, size=smsize-1, face="italic"),
        legend.title=element_blank()) +
  geom_jitter(width=2, size=1.5, alpha=0.7, stroke=0) +
  scale_colour_manual(values=schwilkcolors) +
  xlab("Time since wetting (hr)") + ylab("Fuel consumed (%)") + 
  stat_smooth(data=b.burned, method="lm", se=FALSE, size=0.8)
ggsave(file.path(RESULTS, "time_consume.pdf"), width=col1, height=col1, unit="cm")


# Flame height by time since wetting
ggplot(b.burned, aes(hour, combust, color=taxon)) +
  pubtheme.nogridlines +
  theme(legend.position=c(.2, .86),
        legend.spacing.y=unit(0,"cm"),
        legend.text = element_text(family=fontfamily, size=smsize-1, face="italic"),
        legend.title=element_blank()) +
  geom_jitter(width=2, size=1.5, alpha=0.7, stroke=0) +
  scale_colour_manual(values=schwilkcolors) +
  xlab("Time since wetting (hr)") + ylab("Flame height (mm)") + 
  stat_smooth(data=b.burned, method="lm", se=FALSE, size=0.8)
ggsave(file.path(RESULTS, "time_flamh.pdf"), width=col1, height=col1, unit="cm")


# ignition by time since wetting
ggplot(b, aes(hour, t2ignit, color=taxon)) +
  geom_jitter(width=2, size=1.5, alpha=0.7, stroke=0) +
  geom_smooth(method="lm", se=FALSE, size=1.2) +
  scale_colour_manual(values=schwilkcolors) +
  xlab("Time since wetting (hr)") + ylab("Time to ignition (s)") +
  scale_x_continuous(breaks=xbreaks) +
  scale_y_log10() +
  pubtheme.nogridlines +
  theme(legend.position=c(0.75, 0.86),
    legend.title=element_blank(),
    legend.text = element_text(family=fontfamily, size=smsize, face="italic"))
ggsave(file.path(RESULTS, "fig2_ignit_by_time.pdf"), width=col1, height=col1, units="cm")

ignit.time.mod <- lmer(t2ignit ~ hour + taxon + taxon:hour + vpd + (1 | spcode), data=b) 
summary(ignit.time.mod)
anova(ignit.time.mod)

ignite.time.mod.mixed <- mixed(t2ignit ~ hour + taxon + taxon:hour + (1 | spcode), data=b) 
anova(ignit.time.mod.mixed)

tab.ignit.time <- nice(ignite.time.mod.mixed)
names(tab.ignit.time)[4] <- "p value"
print(xtable(tab.ignit.time), file=file.path(RESULTS, "ignit-time-anova.ltx"),
      booktabs=TRUE, floating=FALSE, include.rownames=FALSE)

tab.ignit.time.coef <- summary(ignit.time.mod)$coefficient
print(xtable(tab.ignit.time.coef), file=file.path(RESULTS, "ignit-coef-tab.ltx"),
      booktabs=TRUE, floating=FALSE)
