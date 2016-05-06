## Creates plots for the decomposition data

source("decomp-flam.R")
source("theme-opts.R")


# actual and predicted spread rate by year
ggplot(pred.allyears %>% filter(year<2) %>% mutate(yearf = paste("year", year)),
       aes(lt_mean, spread.mean, group=spcode)) +
    geom_line(size=1) +
    geom_point(aes(color=yearf), size=4) +
    scale_x_continuous("Particle length / thickness") +
    scale_y_continuous("Spread rate (m/s)") +
    scale_color_brewer(palette="Set1") +
    pubtheme +
    guides(colour=guide_legend(title=("")))
ggsave("../results/plots/observed-pred-spread-rate-shift.pdf", width=col1*1.5, height=col1*1.2, units="cm")


# does mass loss predict change in length/thickness
flamdecomp.sum <- flamdecomp.sum %>% left_join(flamdecomp.sum %>% filter(year==0) %>%
                                               mutate(initial.lt_mean = lt_mean) %>%
                                               select(spcode, initial.lt_mean))

ggplot(flamdecomp.sum %>% filter(year==1), aes(massloss_mean/2, lt_mean - initial.lt_mean)) +
  geom_point(size=3) +
  scale_x_continuous("Proportional mass loss") +
  scale_y_continuous(expression (Delta~" length / thickness")) + #CHECK UNITS
  pubtheme
ggsave("../results/plots/mass-vs-lt.pdf", width=col1, height=col1, units="cm") 


# local version of decomp data just for figures (adds display names, drops year
# 2, reorders species names levels (almost by spread rate, but with pines
# together)
decomp.forplots <- decomp %>% filter(year < 2) %>% left_join(species) %>%
  mutate(yearc=paste("year", year))


# leaf l/t vs bulk density
ggplot(flamdecomp.sum %>% filter(year==0), aes(lt_mean, bulk.mean)) +
  geom_point(size=3) +
  geom_errorbar(aes(ymin=bulk.mean-bulk.se, ymax=bulk.mean+bulk.se), 
                size=0.5)+
  scale_x_continuous("Particle length / thickness", limits=c(0.0,400)) +
  scale_y_continuous(expression(paste("Litter density (", gcm^-3,")"))) + 
  scale_linetype_manual(values = c(1,3,2)) +
  scale_color_manual(values = c( "gray50", "black")) +
  pubtheme
  ## geom_text(aes(label=display.name),
  ##           hjust=-0.1, vjust=0.5, family=fontfamily,
  ##           fontface="italic")
ggsave("../results/plots/bd_vs_lt.pdf", width=col1, height=col1, units="cm")

# leaf length over thickness and spread rate for Year 0
ggplot(flamdecomp.sum %>% filter(year==0), aes(lt_mean, spread.mean)) +
  geom_point(size=3) +
  geom_errorbar(aes(ymin=spread.mean-spread.se, ymax=spread.mean+spread.se), 
                size=0.5)+
  scale_x_continuous("Particle length / thickness", limits=c(0.0,400)) +
  scale_y_continuous("Spread rate (cm/s)") +
  scale_linetype_manual(values = c(1,3,2)) +
  scale_color_manual(values = c( "gray50", "black")) +
  theme_bw() + pubtheme + geom_text(aes(label=display.name),
                                     hjust=-0.2, vjust=0.5, family=fontfamily, size=2,
                                     fontface="italic")
ggsave("../results/plots/spread_vs_lt.pdf", width=col1, height=col1, units="cm")
# Note: should add x-axis error to graphs above


# length over thickness change year 0 to year 1
ggplot(decomp.forplots, aes(x=l/t, color=yearc)) +
  geom_density(size=1) +
  scale_x_log10("Length / thickness", limits = c(1, 600)) +
  pubtheme +
  theme(strip.text.y = element_text(family=fontfamily,
                                    size = smsize, face="italic", angle=0)) +
  scale_colour_grey(start=0, end=0.5) +
  guides(colour=FALSE) +
  geom_vline(xintercept=80) +
  facet_grid( display.name ~ .)
ggsave("../results/plots/decomp-lt-shift.pdf" )

# alternative
decomp.forplots.sum <- decomp.forplots %>% group_by(year, display.name) %>%
  summarize(lt.mean = mean(l/t, na.rm=TRUE), lt.sd = sd(l/t, na.rm=TRUE))

ggplot(decomp.forplots, aes(x=factor(year), y=l/t)) +
  geom_jitter(alpha=0.3, position=position_jitter(w=0.3)) +
 # stat_sum_single(mean) +
  scale_x_discrete("Year") +
  scale_y_continuous("Length / thickness", limits = c(1, 1000)) +
  pubtheme.nogridlines +
  theme(strip.text.x = element_text(family=fontfamily,
                                    size = smsize, face="italic")) +
  scale_colour_grey(start=0, end=0.5) +
  guides(colour=FALSE) +
  facet_grid(. ~  display.name) +
  theme(panel.background = element_rect(size = 1.6, fill = "gray92"),
        panel.border = element_blank()) +
  ## layer(data = decomp.forplots.sum, mapping = aes(x=factor(year), y = lt.mean,
  ##                                                 ymin = lt.mean-lt.sd, ymax = lt.mean+lt.sd),
  ##       geom = "errorbar", size = 1, width = 0.4) +
  geom_abline(intercept=80, slope=0)
ggsave("../results/plots/decomp-lt-shift2.pdf", width=col2*1.5, height=col2, units="cm" )

###############################################################################

# Doing boxplots instead of density plots

decompsp <- decomp %>% left_join(species)

ggplot(decompsp, aes(display.name, l, fill=factor(year))) +
  geom_boxplot() +
  xlab("Species") +
  ylab("Length (mm)") +
  scale_fill_brewer(palette="Greys", direction=-1, name="Year") +
  theme_bw() +
  theme(axis.text.x  = element_text(face="italic")) 

ggsave("../results/plots/Decomp_L_box_bw.pdf", width=15, height=10)

# leaf length over thickness and spread rate for Year 0
ggplot(flamdecomp.sum, aes(lt_mean, spread.mean)) +
  geom_point(size=3) +
  geom_pointrange(aes(ymin=spread.mean-spread.se, ymax=spread.mean+spread.se), 
                  size=0.5)+
  scale_x_continuous("Particle length / thickness", limits=c(0.0,400)) +
  scale_y_continuous("Spread rate (cm/s)") +
  scale_color_manual(values = c( "gray50", "black")) +
  theme_bw() + geom_text(aes(label=display.name),
                         hjust=-0.1, vjust=0.5, family=fontfamily,
                         fontface="italic")

ggsave("../results/plots/spread_vs_lt_y0.pdf")

# leaf length over thickness and spread rate for Year 1

ggplot(pred.y1sum, aes(lt_mean, pred_spread_mean)) +
  geom_point(size=3) +
  geom_errorbar(aes(ymin=pred_spread_mean-pred_spread_sd, ymax=pred_spread_mean+pred_spread_sd),size=0.5)+
  scale_x_continuous("Particle length / thickness", limits=c(0.0,400)) +
  scale_y_continuous("Spread rate (cm/s)", limits=c(0.0,0.45)) +
  scale_color_manual(values = c( "gray50", "black")) +
  theme_bw() + geom_text(aes(label=display.name), hjust=-0.1, 
                         vjust=0.5, family="Arial",
                         fontface="italic")  
# Trying to put both sets of values in one plot
## ggplot(flamdecomp.sum, aes(l_mean/t_mean, spread.mean)) +
##     geom_point(size=3, color="black")  +
##     geom_point(data=pred.y1, size=3,color="gray50") +
##     geom_point(aes(l_mean/t_mean, pred.spread), size=3, color="blue")

ggsave("../results/plots/spread_vs_lt_y1.pdf")