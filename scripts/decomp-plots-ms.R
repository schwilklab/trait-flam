## Creates the figures for the decomposition ms

source("decomp-flam.R") # needed for Fig 2
source("theme-opts.R")

library(directlabels) # for text labels on Fig 2

decompsp <- decomp %>% left_join(species)

## Figure 1: violin plots

ggplot(decompsp, aes(display.name, l, fill=factor(year))) +
  geom_violin(scale="width") +
  xlab("Species") +
  ylab("Leaf Particle Length (mm)") +
  scale_fill_brewer(palette="Greys", direction=-1, name="Year") +
  pubtheme +
  theme(axis.text.x  = element_text(face="italic", angle=45, vjust=1, hjust=1))

ggsave("../results/plots/Decomp_L_violin_bw.pdf", width=col2, height=col2/2,
       units="cm")
ggsave("../results/plots/Decomp_L_violin_bw.png", width=col2, height=col2/2,
       units= "cm", dpi=ppi)

## Figure 2: Spread rate vs. Leaf length
#  plotting 2 datasets in one plot

fig2 <- ggplot(flamdecomp, aes(lt_mean, spread_mean, group=year)) +
  geom_line(aes(group=display.name), size = 1.5, colour="grey66", 
             arrow=arrow(angle = 15, ends="first",type = "closed")) + 
  geom_linerange(aes(ymin=spread_lc, ymax=spread_uc), size=0.5) +
  geom_point(aes(colour = factor(year)), size=4, show.legend = FALSE) +
  scale_x_continuous("Leaf particle length/thickness (mm/mm)", limits=c(0.0,450)) +
  scale_y_continuous("Flame spread rate (actual and predicted; cm/s)") +
#  geom_dl(aes(label=display.name), method = list("smart.grid", cex = 1)) +
  pubtheme +
  scale_color_manual(values = c("black", "gray50"))

fig2

ggsave("../results/plots/spread_vs_lt_both.pdf", width=col2, height=col2/2,
       units="cm")
ggsave("../results/plots/spread_vs_lt_both.png", width=col2, height=col2/2,
       units="cm", dpi=ppi)

# Panelled Fig 2
flamdecomp$year <- as.factor(flamdecomp$year)
flamdecomp$year <- recode(flamdecomp$year, "0" = "Year 0",  "1" = "Year 1")

# apply jitter just to year 1 values. Need to move just 2.
x_jitter <- c(0,0,0,0,0,0,0,0,0,6,0,0,0,0,0,-7)

flamdecomp <- flamdecomp %>% ungroup() %>%
  mutate(lt_mean = lt_mean + x_jitter)

#flamdecomp$lt_mean[flamdecomp$year=="Year 1"] <- flamdecomp$lt_mean[flamdecomp$year=="Year 1"] + x_jitter

fig2.1 <- ggplot(flamdecomp, aes(lt_mean, spread_mean, group=year)) +
  geom_linerange(aes(ymin=spread_lc, ymax=spread_uc), size=0.5) +
  geom_point(aes(colour = factor(year)), size=4, alpha=0.8, show.legend = FALSE) +
  scale_x_reverse("Leaf particle length/thickness (mm/mm)", limits=c(450, 0)) +
  scale_y_continuous("Flame spread rate (cm/s)") +
  facet_grid(. ~ year) +
  pubtheme +
  scale_color_manual(values = c("black", "gray50"))

fig2.1

ggsave("../results/plots/spread_vs_lt_facet.pdf", width=col2, height=col2/2, units= "cm")
ggsave("../results/plots/spread_vs_lt_facet.png", width=col2, height=col2, units="cm", dpi=ppi)
