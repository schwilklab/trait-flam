## Creates the figures for the decomposition ms

source("decomp-flam.R") # needed for Fig 2
source("theme-opts.R")

library(directlabels) # for text labels on Fig 2

## Figure 1: Boxplots

decompsp <- decomp %>% left_join(species)

ggplot(decompsp, aes(display.name, l, fill=factor(year))) +
  geom_boxplot() +
  xlab("Species") +
  ylab("Length (mm)") +
  scale_fill_brewer(palette="Greys", direction=-1, name="Year") +
  theme_bw(base_size = 18) +
  theme(axis.text.x  = element_text(face="italic")) 

ggsave("../results/plots/Decomp_L_box_bw.pdf", width=15, height=10)

## Or violin plots

ggplot(decompsp, aes(display.name, l, fill=factor(year))) +
  geom_violin(scale="width") +
  xlab("Species") +
  ylab("Leaf Particle Length (mm)") +
  scale_fill_brewer(palette="Greys", direction=-1, name="Year") +
  theme_bw(base_size = 20) +
  theme(axis.text.x  = element_text(face="italic")) 

ggsave("../results/plots/Decomp_L_violin_bw.pdf", width=15, height=10)

## Figure 2: Spread rate vs. Leaf length
#  plotting 2 datasets in one plot

fig2 <- ggplot(flamdecomp, aes(lt_mean, spread_mean, group=year)) +
  ## geom_line(aes(group=display.name), size = 1.5, colour="grey66", 
  ##           arrow=arrow(angle = 15, ends="first",type = "closed")) + 
  geom_linerange(aes(ymin=spread_lc, ymax=spread_uc), size=0.5) +
  geom_point(aes(colour = factor(year)), size=4, show.legend = FALSE) +
  scale_x_continuous("Leaf particle length/thickness (mm/mm)", limits=c(0.0,450)) +
  scale_y_continuous("Flame spread rate (actual and predicted; cm/s)") +
  geom_dl(aes(label=display.name), method = list("smart.grid", cex = 1)) +
  pubtheme +
  scale_color_manual(values = c("black", "gray50"))

fig2

ggsave("../results/plots/spread_vs_lt_both.pdf", width=15, height=10, dpi=ppi)
