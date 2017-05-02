## vol-plots.R

## Volatiles graphs.

library(ggplot2)
source("theme-opts.R")
source("read-vol.R")

#ggplot(vol.sum, aes(group_less, sum_low_fp)) + geom_point() + facet_grid(. ~ spcode)

ggplot(filter(vol, flash_point<100, area>0), aes(spcode, flash_point, color=group_less)) +
  geom_jitter(aes(size=area)) +
  xlab("Species") + ylab(expression(paste("Flash point ( ", degree~C, ")"))) +
  scale_color_discrete(name="Volatile group") +
  pubtheme +
  theme(axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12)) + 
  geom_hline(yintercept=34, linetype="dashed", color="red", size=1) #historical maximum temperature
 
ggsave("../results/plots/volatiles.pdf", width=9, height=6)
ggsave("../results/plots/volatiles.png", width=9, height=6, dpi=ppi)

# Stacked bars visualization

ggplot(data=vol.sum.grp, aes(display.name, mean_area, fill=group_less)) +
  geom_bar(stat="identity") + 
  scale_fill_brewer(palette="Spectral", name="Volatile group") + 
  labs(y="Abundance", x="Species") + 
  theme_bw(base_size = 12, base_family = "") + 
  theme(axis.text.x  = element_text(face="italic", angle=45, vjust=1, hjust=1))

ggsave("../results/plots/volatiles_stacked.pdf", width=9, height=6)
ggsave("../results/plots/volatiles_stacked.png", width=9, height=6, dpi=ppi)

# Terpene content vs other volatiles

vol.tt.mean$terpene <- factor(vol.tt.mean$terpene, levels=c("0", "1", "2"), 
                              labels=c("Other volatiles", "Terpenes", "Terpenoids"))

ggplot(data=vol.tt.mean, aes(display.name, mean_area, fill=terpene)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("#00CCFF", "#CC0000", "#660066"), name="") + 
  labs(y="Abundance", x="Species") + theme_bw() +
  theme(axis.text.x  = element_text(face="italic", angle=45, vjust=1, hjust=1))

ggsave("../results/plots/terpenes_others.pdf", width=9, height=6)
ggsave("../results/plots/terpenes_others.png", width=9, height=6, dpi=ppi)


# Total volatile content per species

ggplot(vol.tv.mean, aes(display.name, mean_area)) + geom_point(size=3) +
  xlab("Species") +  ylab("Abundance") + 
  geom_pointrange(aes(ymin=mean_area-sd_area,
                      ymax=mean_area+sd_area),
                  size=0.5) +
  theme_bw() +
  theme(axis.title.x = element_text(size=14),
        axis.text.x  = element_text(face="italic", angle=45, vjust=1, hjust=1, size=12),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12)) 

ggsave("../results/plots/volcontent.pdf", width=9, height=6)
ggsave("../results/plots/volcontent.png", width=9, height=6, dpi=ppi)

# Flammability components vs terpene content

ggplot(vol.flam.avg, aes(log(mean_area+1), spread_mean)) + 
  geom_point(size=1.5) + xlab("Terpene abundance (log 10)") +
  ylab("Flame spread rate (cm/s)") + theme_bw() +
  theme(axis.title.x = element_text(size=10),
        axis.text.x  = element_text(size=9),
        axis.title.y = element_text(size=10),
        axis.text.y  = element_text(size=9)) + bestfit

ggsave("../results/plots/vol_spread.pdf", width=11, height=7, units="cm")
ggsave("../results/plots/vol_spread.png", width=11, height=7, units="cm", dpi=ppi)

ggplot(vol.flam.avg, aes(log(mean_area+1), ignit_mean)) + 
  geom_point(size=1.5) + xlab("Terpene abundance (log 10)") +
  ylab("Time to ignition (s)") + theme_bw() +
  theme(axis.title.x = element_text(size=10),
        axis.text.x  = element_text(size=9),
        axis.title.y = element_text(size=10),
        axis.text.y  = element_text(size=9)) + bestfit 

ggsave("../results/plots/vol_ignit.pdf", width=11, height=7, units="cm")
ggsave("../results/plots/vol_ignit.png", width=11, height=7, units="cm", dpi=ppi)

ggplot(vol.flam.avg, aes(log(mean_low_fp+1), spread_mean)) + 
  geom_point(size=4) + theme_bw() +
  theme(axis.title.x = element_text(size=10),
        axis.text.x  = element_text(size=9),
        axis.title.y = element_text(size=10),
        axis.text.y  = element_text(size=9)) 

ggplot(vol.flam.avg, aes(log(mean_low_fp+1), ignit_mean)) + 
  geom_point(size=4) + theme_bw() +
  theme(axis.title.x = element_text(size=10),
        axis.text.x  = element_text(size=9),
        axis.title.y = element_text(size=10),
        axis.text.y  = element_text(size=9)) 


# Total volatile content and leaf size

ggplot(vol.decomp.sum, aes(lt_mean, mean_area)) + 
  geom_point(size=1.5) + xlab("Leaf thinness (mm/mm)") +
  ylab("Volatile abundance") + theme_bw() +
  theme(axis.title.x = element_text(size=10),
        axis.text.x  = element_text(size=9),
        axis.title.y = element_text(size=10),
        axis.text.y  = element_text(size=9))  + bestfit

ggsave("../results/plots/vol_lt.pdf", width=11, height=7, units="cm")
ggsave("../results/plots/vol_lt.png", width=11, height=7, units="cm", dpi=ppi)

ggplot(vol.decomp.sum, aes(lvol_mean, mean_area)) + 
  geom_point(size=4) + theme_bw() +
  theme(axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12))  + bestfit


# Total volatile content and decomposition

ggplot(vol.decomprates.sum, aes(mean_area, ldrate_mean)) + 
  geom_point(size=1.5) +  
  xlab("Volatile abundance") +
  ylab("Change in leaf particle size") + theme_bw() +
  theme(axis.title.x = element_text(size=10),
        axis.text.x  = element_text(size=9),
        axis.title.y = element_text(size=10),
        axis.text.y  = element_text(size=9)) + bestfit

ggsave("../results/plots/vol_ldrate.pdf", width=11, height=7, units="cm")
ggsave("../results/plots/vol_ldrate.png", width=11, height=7, units="cm", dpi=ppi)

