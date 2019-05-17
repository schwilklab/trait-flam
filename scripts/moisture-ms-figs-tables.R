# Figures for moisture manuscript
# 2019-05-16

library(xtable)
library(ggrepel)

RESULTS <- "../ms/figs_tables/"

source("theme-opts.R")
source("dry-down.R")
source("burn-moist.R")

###############################################################################
## Figure 1: dry-down
###############################################################################
xbreaks <- seq(0, 144, 24)
ybreaks <- seq(0, 700, 50)

fig1 <- ggplot(mc, aes(hour, MC_dry, group=display.name, color=genus)) +
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

ggsave(file.path(RESULTS, "fig1_drydown-curves.pdf"), plot=fig1,
       width=col1, height=col1, units="cm")

###############################################################################
# SI fig 1: dry down on semi log scale
###############################################################################

SI_fig1 <- ggplot(mc, aes(hour, MC_dry, group=display.name, color=genus)) +
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

ggsave(file.path(RESULTS, "SI_fig1_drydown-curves_logged.pdf"), plot=SI_fig1,
       width=col1, height=col1, units="cm")


###############################################################################
## Table 1: drydown by species ANOVA
###############################################################################
tab1 <- nice(dry.mixed)
names(tab1)[4] <- "p value"
#tab1 <- tab1[ ,2:4]
tab1 <- xtable(tab1)

print(tab1, file=file.path(RESULTS, "tab1_drydown_anova.ltx"),  booktabs=TRUE, floating=FALSE,
      include.rownames=FALSE)


###############################################################################
## SI Table 1: drydown by species model coefficients
###############################################################################
SI_tab1 <- summary(dry.mod)$coefficient
print(xtable(SI_tab1), file=file.path(RESULTS, "SI_tab1_drydown_coef.ltx"),  booktabs=TRUE, floating=FALSE)

###############################################################################
## SI Fig 2:  marginal means for max water content
###############################################################################
dry.mod <- lmer(log(MC_dry) ~ hour*display.name + (1 | tray), data=mc)
dry.emm <- emmeans(dry.mod, "display.name")
SI_fig2 <- plot(dry.emm) + #, comparisons=TRUE) +
  xlab("Estimated marginal mean (log water content)") +
  ylab("Species") +
  pubtheme.nogridlines +
  theme(axis.text.y = element_text(face = "italic"))

ggsave(file.path(RESULTS, "SI_fig2_max_water_emmeans.pdf"), plot=SI_fig2,
       width=col2, height=col1, units="cm")

###############################################################################
## SI Fig 3: Marginal means for dessication rate
###############################################################################
dry.trends <- emtrends(dry.mod, "display.name", var="hour")
SI_fig3 <- plot(dry.trends) + #, comparisons=TRUE) +
  xlab(expression(paste("Estimated marginal mean dessication rate (", hr^-1, ")"))) +
  ylab("Species") +
  pubtheme.nogridlines +
  theme(axis.text.y = element_text(face = "italic"))

ggsave(file.path(RESULTS, "SI_fig3_dessication_emmeans.pdf"), plot=SI_fig3,
       width=col2, height=col1, units="cm")


###############################################################################
### Traits and drydown
###############################################################################

# 2011 trait data:
oldtraits <- read.csv("../data/moisture/traits.csv", stringsAsFactors=FALSE)

#mtrait <- left_join(newmctr, species)
mtrait <- left_join(mc.sum, flam.sp.avg)
mtrait <- left_join(mtrait, oldtraits)


###############################################################################
## Fig. 2: leaf area and max retention
###############################################################################

fig2 <- ggplot(mtrait, aes(SLA, maxMC)) +
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
ggsave(file.path(RESULTS, "fig2_SLA_maxMC.pdf"), plot=fig2,
       width=col2, height=col1*1.5, units="cm")

###############################################################################
## Fig. 3: leaf traits and dessication rate
###############################################################################
fig3 <- ggplot(mtrait, aes(bd.mean, abs(di))) +
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

ggsave(file.path(RESULTS, "fig3_di_bd.pdf"), plot=fig3,
       width=col2, height=col1*1.5, units="cm")

###############################################################################
## leaf traits and drydown stats
###############################################################################

# simple model:
cor(mtrait$bulk.mean, mtrait$SLA)

mc.mod <- lm(maxMC ~  SLA * density, data=mutate(mtrait, density=bulk.mean))
summary(mc.mod)
anova(mc.mod)

di.mod <- lm(di ~   density * SLA, data=mutate(mtrait, density=bulk.mean))
summary(di.mod)
anova(di.mod)

###############################################################################
## Table 2: maximum water retention anova
###############################################################################

tab2 <- anova(mc.mod)
names(tab2)[5] <- "p value"
print(xtable(tab2), file=file.path(RESULTS, "tab2_mc_anova.ltx"),  booktabs=TRUE, floating=FALSE,
      include.rownames=FALSE)

###############################################################################
## Table 3: dessication rateanova
###############################################################################
tab3 <- anova(di.mod)
names(tab3)[5] <- "p value"
print(xtable(tab3), file=file.path(RESULTS, "tab3_di_anova.ltx"),  booktabs=TRUE, floating=FALSE,
      include.rownames=FALSE)


###############################################################################
## Flammability plots and tables
###############################################################################
oldflam <- read.csv("../data/burn-trials/flamdt.csv") %>% left_join(species) %>%
  mutate(hour=144, vpd=2.44, t2ignit=ignit, combust=combus) #, actualMC_dry=5)
b <- oldflam %>% filter(nchar(as.character(spcode)) < 5) %>% bind_rows(burnt)

# fix coding errors in data:
b <- b %>% mutate(consum = case_when(consum==100 ~ 0, TRUE ~ consum),
                  didburn = !(t2ignit==0 & spread==0 & consum <0.000000001),
                  taxon = case_when(genus=="Sequoiadendron" |
                                      genus=="Calocedrus" ~ "Cupressaceae", TRUE ~ genus)
                  )
b.scaled <- b %>% mutate_if(is.numeric, scale)

# not necessary, allready filtered according to Rita's notes:
#b.burned <- filter(b, didburn)
#b.burned.scaled <- b.burned %>%  mutate_if(is.numeric, scale)


###############################################################################
## Fig 4: Spread rate and moisture content
###############################################################################
fig4 <- ggplot(b, aes(actualMC_dry, spread, color=taxon)) +
  geom_point(size=1.5, alpha=0.7, stroke=0) +
  scale_colour_manual(values=schwilkcolors) +
  xlab("Moisture content (%)") + ylab("Spread rate (mm/s)") +
  pubtheme.nogridlines +
  stat_smooth(data=b.burned, method="lm", se=FALSE, size=0.8, drop=TRUE) +
  theme(legend.position=c(.75, .86),
        legend.spacing.y=unit(0,"cm"),
        legend.text = element_text(family=fontfamily, size=smsize-1, face="italic"),
        legend.title=element_blank())
ggsave(file.path(RESULTS, "fig4_spread_actualMC.pdf"), plot=fig4,
       width=col1, height=col1, unit="cm")

fig4
spread.moist.mod <- lmer(spread ~ actualMC_dry + taxon + taxon:hour + vpd + (1 | spcode),
                         data=b)
summary(spread.moist.mod)
anova(spread.moist.mod)

###############################################################################
## Table 3: Spread rate and moisture content ANOVA
###############################################################################
spread.moist.mod.mixed <- mixed(spread ~ actualMC_dry + taxon + taxon:hour + vpd + (1 | spcode),
                           data=b, method="KR")
tab.spread.moist <- anova(spread.moist.mod.mixed)
tab.spread.moist
names(tab.spread.moist)[4] <- "p value"
print(xtable(tab.spread.moist), file=file.path(RESULTS, "tab3_spread_moist_anova.ltx"),
      booktabs=TRUE, floating=FALSE, include.rownames=FALSE)

###############################################################################
## SI Table 2: Spread rate and moisture content coefficients
###############################################################################
tab.spread.moist.coef <- summary(spread.moist.mod)$coefficients
print(xtable(tab.spread.moist.coef), file=file.path(RESULTS, "SI_tab2_spread_moist_coef.ltx"),  booktabs=TRUE, floating=FALSE)


###############################################################################
## Fig 5: Fuel consumption by moisture content
###############################################################################
fig5 <- ggplot(b, aes(actualMC_dry, consum, color=taxon)) +
  geom_point(size=1.5, alpha=0.7, stroke=0) +
  scale_colour_manual(values=schwilkcolors) +
  xlab("Moisture content (%)") + ylab("Fuel consumed (%)") + 
  pubtheme.nogridlines +
  stat_smooth(method="lm", se=FALSE, size=0.8) +
  theme(legend.position=c(.8, .86),
        legend.spacing.y=unit(0,"cm"),
        legend.text = element_text(family=fontfamily, size=smsize-1, face="italic"),
        legend.title=element_blank())
ggsave(file.path(RESULTS, "fig5_consume_actualMC.pdf"), plot=fig5,
       width=col1, height=col1, unit="cm")

###############################################################################
## Table 4: Fuel consumptione and moisture content ANOVA
###############################################################################
consume.moist.mod <- lmer(consum ~ actualMC_dry + taxon + taxon:hour + vpd + (1 | spcode),
                         data=b)

summary(consume.moist.mod)
anova(consume.moist.mod)

consume.moist.mod.mixed <- mixed(consum ~ actualMC_dry + taxon + taxon:hour + vpd + (1 | spcode),
                           data=b, method="KR")
tab.consume.moist <- anova(consume.moist.mod.mixed)
tab.consume.moist
names(tab.consume.moist)[4] <- "p value"
print(xtable(tab.consume.moist), file=file.path(RESULTS, "tab4_consume_moist_anova.ltx"),
      booktabs=TRUE, floating=FALSE, include.rownames=FALSE)

###############################################################################
## SI Table 3: Consumption and moisture content coefficients
###############################################################################
tab.consume.moist.coef <- summary(consume.moist.mod)$coefficients
print(xtable(tab.consume.moist.coef), file=file.path(RESULTS, "SI_tab3_consume_moist_coef.ltx"),  booktabs=TRUE, floating=FALSE)


###############################################################################
### Flammability by time since wetting
###############################################################################




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
