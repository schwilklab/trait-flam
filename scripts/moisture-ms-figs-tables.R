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

## ggplot(mc, aes(hour, MC_dry, group=display.name, color=genus)) +
##   geom_point(size=1.2, alpha=0.7, stroke=0) +
##   geom_smooth(method="lm", se=FALSE, size =0.6) +
##   scale_colour_manual(values=schwilkcolors) +
##   xlab("Hours since wetting") + ylab("Moisture by dry weight (%)") +
##   scale_x_continuous(breaks=xbreaks) +
##   scale_y_log10() +
##   pubtheme.nogridlines +
##   theme(legend.position=c(0.75, 0.86),
##         legend.title=element_blank(),
##         legend.text = element_text(family=fontfamily, size=smsize, face="italic"))

## ggsave(file.path(RESULTS, "fig1_drydown-curves_logged.pdf"), width=col1, height=col1, units="cm")

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
  mutate(hour=120, vpd=2.44)
b <- oldflam %>% filter(nchar(as.character(spcode)) < 5) %>% bind_rows(burnt)


ggplot(subset(b, ignit!=0),
       aes(hour, spread, color=genus)) +
  geom_jitter(width=2, size=1.5, alpha=0.7, stroke=0) +
  geom_smooth(method="lm", se=FALSE, size=1.2) +
  scale_colour_manual(values=schwilkcolors, drop=TRUE) +
  xlab("Time since wetting (hr)") +  ylab("Spread rate (mm/s)") +
  scale_x_continuous(breaks=xbreaks) +
  #scale_y_log10() +
  pubtheme.nogridlines +
  theme(legend.position=c(.3, .86),
        legend.title=element_blank())

ggsave(file.path(RESULTS, "fig4_spread_by_time.pdf"), width=col1, height=col1, units="cm")

modspread <- lmer(spread ~ hour + genus + genus:hour + vpd + (1 | spcode), data=b) 
summary(modspread)
anova(modspread)

modspread.mixed <- mixed(spread ~ hour + genus + genus:hour + vpd + (1 | spcode), data=b) 
anova(modspread.mixed)

ggplot(na.omit(burnt), aes(hour, t2ignit, color=genus)) +
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
ggsave(file.path(RESULTS, "actualMC_ignit.pdf"), width=col1, height=col1, unit="cm")


ggplot(burnt1, aes(actualMC_dry, spread, color=genus)) +
  geom_point(size=1.5, alpha=0.7, stroke=0) +
  scale_colour_manual(values=schwilkcolors) +
  xlab("Moisture content (%)") + ylab("Spread rate (mm/s)") +
  pubtheme.nogridlines +
  stat_smooth(data=burnt, method="lm", se=FALSE, size=0.8, drop=TRUE) +
  theme(legend.position=c(.75, .86),
        legend.spacing.y=unit(0,"cm"),
        legend.text = element_text(family=fontfamily, size=smsize-1, face="italic"),
        legend.title=element_blank())
ggsave(file.path(RESULTS, "actualMC_spread.pdf"), width=col1, height=col1, unit="cm")

