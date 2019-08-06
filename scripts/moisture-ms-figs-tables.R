# Figures for moisture manuscript
# 2019-05-16

library(xtable)
library(ggrepel)
library(glmmADMB)

RESULTS <- "../ms/figs_tables/"

source("theme-opts.R")
source("dry-down.R")
source("burn-moist.R")
source("read-moistmix.R")

set.seed(100)

###############################################################################
## Figure 1: dry-down
###############################################################################
xbreaks <- seq(0, 144, 24)
ybreaks <- seq(0, 700, 50)

fig1 <- ggplot(mc, aes(hour, MC_dry, group=display.name, color=genus)) +
  geom_jitter(height=0, width=0.5, size=1.2, alpha=0.7, stroke=0) +
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
tab1 <- nice(dry.mixed, sig_symbols = rep("", 4))
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
  xlab(expression(paste("Estimated marginal mean drying rate (", hr^-1, ")"))) +
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
oldtraits$SLA <- oldtraits$SLA/1000
#mtrait <- left_join(newmctr, species)
mtrait <- left_join(mc.sum, flam.sp.avg)
mtrait <- left_join(mtrait, oldtraits)


###############################################################################
## SI Fig. 4: leaf area and max retention
###############################################################################

SI_fig4 <- ggplot(mtrait, aes(SLA, maxMC)) +
  geom_point(size=3) +
#  geom_errorbar(aes(ymin=maxMC+maxMC.se, ymax=maxMC-maxMC.se))+
#  geom_errorbarh(aes(xmin=leaf.area.mean+(leaf.area.sd/sqrt(6)), xmax=leaf.area.mean-(leaf.area.sd/sqrt(6))))+
  geom_label_repel(aes(label=display.name),
                   hjust= 0,
                  # direction="y",
                   nudge_x= 2,
                   nudge_y=100,
                  fontface="italic",
                  size=smsize-6,
                  max.iter=5000) +
#  scale_y_log10() + 
#  scale_colour_manual(values=schwilkcolors) +
  xlab(expression(paste("Specific leaf area (", cm^3/g, ")" ))) +
  ylab("Maximum moisture content (%)") +
  pubtheme.nogridlines

SI_fig4
ggsave(file.path(RESULTS, "SI_fig4_SLA_maxMC.pdf"), plot=SI_fig4,
       width=col1, height=col1, units="cm")

###############################################################################
## SI Fig 5: leaf traits and dessication rate
###############################################################################
SI_fig5 <- ggplot(mtrait, aes(bd.mean, abs(di))) +
  geom_point(size=3, stroke=0, alpha=0.8) +
#  geom_errorbar(aes(ymin=abs(di)+di.se, ymax=abs(di)-di.se))+
#  geom_errorbarh(aes(xmin=bd.mean+(bd.sd/sqrt(6)), xmax=bd.mean-(bd.sd/sqrt(6))))+
  scale_colour_manual(values=schwilkcolors) +
  xlab(expression(paste("Bulk density (", g/cm^3,")"))) +
  ylab(expression(paste("Drying rate (", hr^-1, ")"))) +
 geom_label_repel(aes(label=display.name),
                  nudge_x=0.005,
                  nudge_y=-0.001,
                    size=smsize-6,
                  fontface="italic") +
  pubtheme.nogridlines
SI_fig5
ggsave(file.path(RESULTS, "SI_fig5_di_bd.pdf"), plot=SI_fig5,
       width=col1, height=col1, units="cm")

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

tab2a <- anova(mc.mod)
tab2a$Effect <- rownames(tab2a)
tab2a <- tab2a[,c(6,1,2,3,4,5)]
names(tab2a) <- c("Effect", "df", "Sum of Squares", "Mean squares", "F", "p value")
tab2a.x <- xtable(tab2a, digits=c(0,0,0,1,2,2,3))
#tab2a.x[1,6] <- "<0.0001"
print(tab2a.x, file=file.path(RESULTS, "tab_mc_anova.ltx"),  booktabs=TRUE, floating=FALSE,
      include.rownames=FALSE)

###############################################################################
## Table 3: dessication rateanova
###############################################################################
tab2b <- as.data.frame(anova(di.mod))
tab2b$Effect <- rownames(tab2b)
rownames(tab2b) <- NULL
tab2b <- tab2b[,c(6,1,2,3,4,5)]
names(tab2b) <- c("Effect", "df", "Sum of Squares", "Mean squares", "F", "p value")
tab2b.x <- xtable(tab2b, digits=c(0, 0 ,0,-2,-2, 2,3))
print(tab2b.x, file=file.path(RESULTS, "tab_di_anova.ltx"), math.style.exponents=TRUE,
      booktabs=TRUE, floating=FALSE,
      include.rownames=FALSE)


##########################
## SI tables for traits anovas SI tab 2 and SI tab 3

SI.tab.moist.coef <- summary(mc.mod)$coefficients
print(xtable(SI.tab.moist.coef), file=file.path(RESULTS, "SI_tab2_mc_coef.ltx"),  booktabs=TRUE, floating=FALSE)

SI.tab.di.coef <- summary(di.mod)$coefficients
print(xtable(SI.tab.di.coef), file=file.path(RESULTS, "SI_tab3_di_coef.ltx"),  booktabs=TRUE, floating=FALSE)


###############################################################################
## Flammability plots and tables
###############################################################################
oldflam <- read.csv("../data/burn-trials/flamdt.csv") %>% right_join(species) %>%
  mutate(hour=144, vpd=2.44, t2ignit=ignit, combust=combus) #, actualMC_dry=5)
b <- oldflam %>% filter(nchar(as.character(spcode)) < 5) %>% bind_rows(burnt)

# fix coding errors in data:
b <- b %>% mutate(consum = case_when(consum==100 ~ 0, TRUE ~ consum),
                  didburn = !(t2ignit==0 & spread==0 & consum <0.000000001),
                  taxon = case_when(genus=="Sequoiadendron" |
                                    genus=="Calocedrus" ~ "Cupressaceae",
                       #             spcode=="Pila" ~ "P. lambertiana",
                                    TRUE ~ genus)
                  )
b.scaled <- b %>% mutate_if(is.numeric, scale)

# not necessary, allready filtered according to Rita's notes:
#b.burned <- filter(b, didburn)
#b.burned.scaled <- b.burned %>%  mutate_if(is.numeric, scale)


###############################################################################
## Fig 2: Spread rate and moisture content
###############################################################################
fig2 <- ggplot(b, aes(actualMC_dry, spread, color=taxon)) +
  geom_point(size=1.5, alpha=0.7, stroke=0) +
  scale_colour_manual(values=schwilkcolors) +
  xlab("Moisture content (%)") + ylab("Spread rate (cm/s)") +
  pubtheme.nogridlines +
  stat_smooth(method="lm", se=FALSE, size=0.8, drop=TRUE) +
  theme(legend.position=c(.75, .86),
        legend.spacing.y=unit(0,"cm"),
        legend.text = element_text(family=fontfamily, size=smsize-1, face="italic"),
        legend.title=element_blank())
ggsave(file.path(RESULTS, "fig2_spread_actualMC.pdf"), plot=fig2,
       width=col1, height=col1, unit="cm")

fig2
spread.moist.mod <- lmer(spread ~ actualMC_dry + taxon + taxon:actualMC_dry + vpd + (1 | spcode),
                         data=b)
summary(spread.moist.mod)
anova(spread.moist.mod)

###############################################################################
## Table 4: Spread rate and moisture content ANOVA
###############################################################################
spread.moist.mod.mixed <- mixed(spread ~ actualMC_dry + taxon + taxon:actualMC_dry + vpd +
                                  (1 | spcode), data=b, method="KR")
tab.spread.moist <- anova(spread.moist.mod.mixed)
tab.spread.moist
tab4 <- nice(spread.moist.mod.mixed,  sig_symbols = rep("", 4))
names(tab4)[4] <- "p value"
print(xtable(tab4), file=file.path(RESULTS, "tab4_spread_moist_anova.ltx"),
      booktabs=TRUE, floating=FALSE, include.rownames=FALSE)

###############################################################################
## SI Table 4: Spread rate and moisture content coefficients
###############################################################################
tab.spread.moist.coef <- summary(spread.moist.mod)$coefficients
print(xtable(tab.spread.moist.coef), file=file.path(RESULTS, "SI_tab4_spread_moist_coef.ltx"),  booktabs=TRUE, floating=FALSE)


###############################################################################
## Fig 3: Fuel consumption by moisture content
###############################################################################
b.consume <- b %>% mutate(spcode=factor(spcode), taxon=factor(taxon), consum=consum/100)
b.consume <- mutate(b.consume, consum.t = case_when(consum == 0 ~ consum+0.00001, TRUE ~ consum),
            consum=consum.t) %>% filter(!is.na(actualMC_dry))

opts <-admbControl(impSamp=0,maxfn=500,imaxfn=500,maxph=5,noinit=FALSE,shess=TRUE,
            run=TRUE, ZI_kluge=FALSE, poiss_prob_bound=TRUE)

consume.moist.mod <- glmmadmb(consum ~ actualMC_dry + taxon + taxon:actualMC_dry + (1 | spcode),
                             family="beta", link="logit", zeroInflation=FALSE, mcmc=TRUE,
                             data=b.consume, admb.opts=opts, verbose=TRUE)
summary(consume.moist.mod)


newdata <- expand.grid(taxon = levels(b.consume$taxon), actualMC_dry=c(0:80))
newdata$consum <- predict(consume.moist.mod, newdata, type="response")*100



fig3 <- ggplot(b, aes(actualMC_dry, consum, color=taxon)) +
  geom_jitter(height=1, width=1, size=1.5, alpha=0.7, stroke=0) +
  scale_colour_manual(values=schwilkcolors) +
  xlab("Moisture content (%)") + ylab("Fuel consumed (%)") + 
  pubtheme.nogridlines +
  #stat_smooth(method="lm", se=FALSE, size=0.8) +
  geom_line(data=newdata, size=0.8) +
  theme(legend.position=c(.8, .86),
        legend.spacing.y=unit(0,"cm"),
        legend.text = element_text(family=fontfamily, size=smsize-1, face="italic"),
        legend.title=element_blank())
fig3
ggsave(file.path(RESULTS, "fig3_consume_actualMC.pdf"), plot=fig3,
       width=col1, height=col1, unit="cm")

###############################################################################
## Table 5: Fuel consumption and moisture content ANOVA
###############################################################################
## consume.moist.mod <- lmer(consum ~ actualMC_dry + taxon + taxon:actualMC_dry + vpd + (1 | spcode),
##                          data=b)

## summary(consume.moist.mod)
## anova(consume.moist.mod)

## consume.moist.mod.mixed <- mixed(consum ~ actualMC_dry + taxon + taxon:actualMC_dry + vpd + (1 | spcode),
##                            data=b, method="KR")

consume.moist.mod.null0 <- glmmadmb(consum ~ 1 + (1 | spcode),
                             family="beta", link="logit", zeroInflation=FALSE,
                             data=b.consume, admb.opts=opts, verbose=TRUE)
consume.moist.mod.null1 <- glmmadmb(consum ~ taxon + (1 | spcode),
                             family="beta", link="logit", zeroInflation=FALSE,
                             data=b.consume, admb.opts=opts, verbose=TRUE)
consume.moist.mod.null2 <- glmmadmb(consum ~ taxon + actualMC_dry + (1 | spcode),
                             family="beta", link="logit", zeroInflation=FALSE,
                             data=b.consume, admb.opts=opts, verbose=TRUE)

tab.consume.moist <- anova(consume.moist.mod, consume.moist.mod.null0,
                           consume.moist.mod.null1, consume.moist.mod.null2)

tab.consume.moist


#names(tab.consume.moist)[4] <- "p value"
print(xtable(tab.consume.moist), file=file.path(RESULTS, "tab5_consume_moist_anova.ltx"),
      booktabs=TRUE, floating=FALSE, include.rownames=FALSE)

###############################################################################
## SI Table 5: Consumption and moisture content coefficients
###############################################################################
tab.consume.moist.coef <- summary(consume.moist.mod)$coefficients
print(xtable(tab.consume.moist.coef), file=file.path(RESULTS, "SI_tab5_consume_moist_coef.ltx"),  booktabs=TRUE, floating=FALSE)


###############################################################################
### Flammability by time since wetting
###############################################################################

###############################################################################
## Fig 4: # spread rate by time
###############################################################################

fig4 <- ggplot(b, aes(hour, spread, color=taxon)) +
  geom_jitter(width=2, height=0, size=1.5, alpha=0.7, stroke=0) +
  geom_smooth(method="lm", se=FALSE, size=1.2) +
  scale_colour_manual(values=schwilkcolors, drop=TRUE) +
  xlab("Time since wetting (hr)") +  ylab("Spread rate (cm/s)") +
  scale_x_continuous(breaks=xbreaks) +
  #scale_y_log10() +
  pubtheme.nogridlines +
  theme(legend.position=c(.3, .86),
        legend.title=element_blank())
fig4
ggsave(file.path(RESULTS, "fig4_spread_time.pdf"), plot=fig4,
       width=col1, height=col1, units="cm")

spread.time.mod <- lmer(spread ~ hour + taxon + taxon:hour + vpd + (1 | spcode), data=b.scaled) 
summary(spread.time.mod)
anova(spread.time.mod)

spread.mod.mixed <- mixed(spread ~ hour + taxon + taxon:hour + vpd + (1 | spcode), data=b.scaled) 
anova(spread.mod.mixed)

## spread.time.mod.lm <- lm(spread ~ hour + taxon + taxon:hour + vpd, data=b.burned) 
## anova(spread.time.mod.lm)

###############################################################################
## Table 6: # spread rate by time
###############################################################################

tab.spread.time <- nice(spread.mod.mixed, sig_symbols = rep("", 4))
names(tab.spread.time)[4] <- "p value"
print(xtable(tab.spread.time), file=file.path(RESULTS, "tab6_spread_time_anova.ltx"),
      booktabs=TRUE, floating=FALSE, include.rownames=FALSE)

###############################################################################
## SI Table 6:  spread rate by time coefficients
###############################################################################

tab.spread.time.coef <- summary(spread.mod.mixed)$coefficient
print(xtable(tab.spread.time.coef), file=file.path(RESULTS, "SI_tab6_spread_time_coef.ltx"),  booktabs=TRUE, floating=FALSE)



###############################################################################
## Fuel consumption by time since wetting
###############################################################################
library(glmmADMB)
library(betareg)
#library(glmmTMB)

b.new <- b %>% mutate(taxon = case_when(genus=="Sequoiadendron" |
                                    genus=="Calocedrus" ~ "Cupressaceae",
                                    spcode=="Pila" ~ "P. lambertiana",
                                    TRUE ~ genus)
                  )


b1 <- b.new %>% mutate(spcode=factor(spcode), taxon=factor(taxon), consum=consum/100)
b2 <- data.frame(consum = b1$consum, spcode=b.scaled$spcode,  taxon=b.scaled$taxon,
                 vpd=b.scaled$vpd, hour=b.scaled$hour)
b3 <- mutate(b2, consum.t = case_when(consum == 0 ~ consum+0.001, TRUE ~ consum))
b4<- mutate(b1, consum.t = case_when(consum == 0 ~ consum+0.001, TRUE ~ consum),
            consum=consum.t)


## # add in zero consumption for non ignition
## contrasts(b4$taxon) <- contr.sum(levels(b4$taxon))
## colnames(contrasts(b4$taxon)) <- head(levels(b4$taxon), -1)

### beta regression appraoch
opts <-admbControl(impSamp=0,maxfn=500,imaxfn=500,maxph=5,noinit=FALSE,shess=TRUE,
            run=TRUE, ZI_kluge=FALSE, poiss_prob_bound=TRUE)

consume.time.mod <- glmmadmb(consum ~ hour + taxon + taxon:hour,
                             family="beta", link="logit", zeroInflation=FALSE,
                             data=b4, admb.opts=opts, verbose=TRUE)
summary(consume.time.mod)


newdata <- expand.grid(taxon = levels(b4$taxon), hour=c(0:144))
newdata$consum <- predict(consume.time.mod, newdata, type="response")*100


###############################################################################
## Fig 5: Fuel consumption by time
###############################################################################
# clean the predicted data to correct ranges:
newdata1 <- newdata %>% mutate(hour = case_when(taxon=="Abies" & hour < 48 ~ NA_real_,
                                               taxon=="P. lambertiana" & hour < 5 ~ NA_real_,
                                               taxon=="Quercus" & hour < 70 ~ NA_real_,
                                               TRUE ~ as.numeric(hour)))

fig5 <- ggplot(b, aes(hour, consum, color=taxon)) +
  scale_colour_manual(values=schwilkcolors) +
  xlab("Time since wetting (hr)") + ylab("Fuel consumed (%)") +
  scale_y_continuous(limits=c(0,100)) +
  pubtheme.nogridlines +
  #stat_smooth(method="loess", se=FALSE, size=0.8) +
  geom_line(data=newdata1, size=0.8) +
  theme(legend.position=c(.175, .51),
        legend.spacing.y=unit(0,"cm"),
        legend.text = element_text(family=fontfamily, size=smsize-1, face="italic"),
        legend.title=element_blank()) +
  geom_jitter(width=2, size=1.5, alpha=0.7, stroke=0)
fig5
ggsave(file.path(RESULTS, "fig5_consume_time.pdf"), plot=fig5,
       width=col1, height=col1, unit="cm")


###############################################################################
## Table 7: Fuel consumption by time
###############################################################################
consume.time.mod.null0 <- glmmadmb(consum ~ 1,
                             family="beta", link="logit", zeroInflation=FALSE,
                             data=b4, admb.opts=opts, verbose=TRUE)
consume.time.mod.null1 <- glmmadmb(consum ~ taxon,
                             family="beta", link="logit", zeroInflation=FALSE,
                             data=b4, admb.opts=opts, verbose=TRUE)
consume.time.mod.null2 <- glmmadmb(consum ~ taxon + hour,
                             family="beta", link="logit", zeroInflation=FALSE,
                             data=b4, admb.opts=opts, verbose=TRUE)
tab.consume.time <- anova(consume.time.mod.null0, consume.time.mod.null1, consume.time.mod.null2, consume.time.mod)

tab.consume.time[,1] <- c("consum ~ 1","consum ~ taxon","consum ~ taxon + hour","consum ~ hour + taxon + taxon:hour")

names(tab.consume.time) <- c("Model", "Log Lik.", "df", "Deviance", "p value")

print(xtable(tab.consume.time), file=file.path(RESULTS, "tab7_consume_time_anova.ltx"),
      booktabs=TRUE, floating=FALSE, include.rownames=FALSE)


###############################################################################
## SI Table 7: Consumption by time coefficients
###############################################################################
tab.consume.time.coef <- summary(consume.time.mod)$coefficients
SI_tab5 <- xtable(tab.consume.time.coef)
print(SI_tab5,file=file.path(RESULTS, "SI_tab7_consume_time_coef.ltx"),
      booktabs=TRUE, floating=FALSE)


##############################################################################
## Mixtures
##############################################################################


###############################################################################
## Fig SI 4: Drydown for mixtures
###############################################################################
SI_fig6 <- ggplot(mmc, aes(hour, MC_dry, color=spcode)) +
  geom_jitter(height=0, width=0.5, size=1.2, alpha=0.7, stroke=0) +
  geom_smooth(method="glm",
              method.args=list(family=gaussian(link="log")), se=FALSE, size =0.6) +
  scale_colour_manual(values=schwilkcolors) +
  xlab("Time since wetting (hr)") + ylab("Moisture by dry weight (%)") +
  scale_x_continuous(breaks=xbreaks) +
  scale_y_continuous(breaks=ybreaks) +
  pubtheme.nogridlines +
  theme(legend.position=c(0.75, 0.86),
        legend.title=element_blank())
#        legend.text = element_text(family=fontfamily, size=smsize, face="italic"))
#       legend.key.height=unit(smsize,"pt"))
SI_fig6
ggsave(file.path(RESULTS, "SI_fig6_mixture_drydown-curves.pdf"), plot=SI_fig6,
       width=col1, height=col1, units="cm")

# stats
mix_drydown_mod <- lmer(log(MC_dry) ~ hour * spcode + (1 | rep), data=mmc)
summary(mix_drydown_mod)
anova(mix_drydown_mod)

mix_drydown_mixed <- mixed(log(MC_dry) ~ hour * spcode + (1 | rep), data=mmc)

###############################################################################
## SI Table 8: drydown by mixture ANOVA
###############################################################################
SItab8 <- nice(mix_drydown_mixed,  sig_symbols = rep("", 4))
names(SItab8)[4] <- "p value"
#tab1 <- tab1[ ,2:4]
SItab8.x <- xtable(SItab8)

print(SItab8.x, file=file.path(RESULTS, "SI_tab8_mixtures_drydown_anova.ltx"),  booktabs=TRUE, floating=FALSE, include.rownames=FALSE)


#SI Tab9
tab_mix_drydown_coef <- summary(mix_drydown_mod)$coefficients
SI_tab9 <- xtable(tab_mix_drydown_coef)
print(SI_tab9,file=file.path(RESULTS, "SI_tab9_mix_drydown_coef.ltx"),
      booktabs=TRUE, floating=FALSE)


###############################################################################
## Fig 6: Observed vs predicted moisture
###############################################################################

fig6 <- ggplot(mmc.sum, aes(MC_dry_pred, MC_dry.mean, color=spcode, shape=factor(hour))) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin=MC_dry.mean-MC_dry.sd,
                      ymax=MC_dry.mean+MC_dry.sd),
                  size=0.5) +
  scale_shape_discrete(name="Time since wetting") + 
  scale_colour_manual(name="Litter mixture", values=schwilkcolors) +
#  scale_colour_discrete(name="Hour since saturation") +
  xlab("Predicted moisture content (%)") + ylab("Observed moisture content (%)") +
  geom_abline(intercept=0, slope=1) +
  pubtheme.nogridlines

  ## theme(legend.position=c(.1, .86),
  ##       legend.title=element_blank())

fig6
ggsave(file.path(RESULTS, "fig6_mixture_obs_vs_pred_mc.pdf"), plot=fig6,
       width=col2, height=col1, units="cm")


## test:
mix_mc_mod <- lmer(MC_dry ~ MC_dry_pred + (1 | spcode), data=mmc)
summary(mix_mc_mod)
library(car)


# conclusion, slope of .455 significantly higher than zero (p <0.0001).

###############################################################################
## Fig 7: Observed vs predicted spread rate
###############################################################################
names(mflam)[names(mflam) == 'spcode'] <- 'mix' # rename

getmean <- function(df, hr, s1, s2, s3, var) {
  res <- mean(dplyr::filter(df, hour==hr & spcode %in% c(s1,s2,s3))[[var]])
  if(is.nan(res)) {
    res <- 0
  }
  return(res)
}

## getmeanco <- function(df) {
##   return(mean(df$consum))
## }

b.sum <- b %>% group_by(spcode, hour) %>% summarize(spread = mean(spread), consum = mean(consum), actualMC_dry = mean(actualMC_dry))


b.mix.pred <- dplyr::select(mflam, mix, hour, sp1, sp2, sp3) %>%
  unique() %>% rowwise() %>%
  mutate(spread.m = getmean(b.sum, hour, sp1,sp2,sp3, "spread"),
         consum.m = getmean(b.sum, hour, sp1,sp2,sp3, "consum"))

b.mix.sum <- left_join(mflam, b.mix.pred)



fig7 <- ggplot(mflam, aes(hour, spread)) +
  facet_grid(. ~ mix) +
  geom_jitter() +
  #  xlim(0.0, 0.3) +
  geom_point(aes(hour, spread.m), size=3, shape=5, data = b.mix.pred) +
  xlab("Time since wetting (h)") + ylab("Spread rate (cm/s)") +
  pubtheme.nogridlines

fig7
ggsave(file.path(RESULTS, "fig7_mixture_obs_vs_pred_spread.pdf"), plot=fig7,
       width=col2, height=col1, units="cm")

# test:
mix_spread_mod <- lmer(spread-spread.m ~ scale(hour) + (1 | mix), data = b.mix.sum)
summary(mix_spread_mod)


###############################################################################
## Fig 8: Observed vs predicted fuel consumption
###############################################################################
fig8 <- ggplot(mflam, aes(hour, consum)) +
  facet_grid(. ~ mix) +
  geom_jitter() +
  #  xlim(0.0, 0.3) +
  geom_point(aes(hour, consum.m), size=3, shape=5, data = b.mix.pred) +
  xlab("Time since wetting (h)") + ylab("Percent fuel consumed") +
  pubtheme.nogridlines

## fig11 <- ggplot(b.mix.sum, aes(hour, consum-consum.m)) +
##   facet_grid(. ~ mix) +
##   geom_jitter(width=3) +
##   geom_hline(aes(yintercept=0), color="black") +
##     geom_smooth(method="lm", se=FALSE, color="black") +
##   #  xlim(0.0, 0.3) +
## #  geom_point(aes(hour, spread.m, color=hour), size=3, shape=5, data = b.mix.pred) +
##   xlab("Time since wetting (h)") + ylab("Relative percent fuel consumed") +
##   pubtheme.nogridlines

fig8
ggsave(file.path(RESULTS, "fig8_mixture_obs_vs_pred_consume.pdf"), plot=fig8,
       width=col2, height=col1, units="cm")

# test:
mix_consum_mod <- lmer(consum-consum.m ~ scale(hour) + (1 | mix), data = b.mix.sum)
summary(mix_consum_mod)
# So, some non additivity n drying but switches.  Positive non additivity for spread rate -- probably same mechanisms as described in 2011. No non addiviticvty for consume except for driest litters had postiive non additivity.



