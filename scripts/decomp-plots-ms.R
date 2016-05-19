## Creates the figures for the decomposition ms

source("decomp-flam.R")
source("theme-opts.R")

## Figure 1: Boxplots

decompsp <- decomp %>% left_join(species)


ggplot(decompsp, aes(display.name, l, fill=factor(year))) +
  geom_boxplot() +
  xlab("Species") +
  ylab("Length (mm)") +
  scale_fill_brewer(palette="Greys", name="Year") +
  theme_bw(base_size = 18) +
  theme(axis.text.x  = element_text(face="italic")) 
ggsave("../results/plots/Decomp_L_box_bw.pdf", width=15, height=10)

## Or violin plots

ggplot(decompsp, aes(display.name, l, fill=factor(year))) +
  geom_violin(scale="width") +
  xlab("Species") +
  ylab("Leaf particle length (mm)") +
  scale_fill_brewer(palette="Greys", name="Year") +
  theme_bw(base_size = 20) +
  theme(axis.text.x  = element_text(face="italic")) 

ggsave("../results/plots/Decomp_L_violin_bw.pdf", width=15, height=10)

## Figure 2
# leaf length over thickness and spread rate for Year 0

ggplot(flamdecomp.sum.y0, aes(lt_mean, spread.mean)) +
  geom_point(size=3) +
  geom_pointrange(aes(ymin=spread.mean-spread.se, ymax=spread.mean+spread.se), 
                  size=0.5)+
  scale_x_continuous("Particle length / thickness", limits=c(0.0,400)) +
  scale_y_continuous("Spread rate (cm/s)") +
  scale_color_manual(values = c( "gray50", "black")) +
  theme_bw(base_size = 16) + geom_text(aes(label=display.name),
                         hjust=-0.1, vjust=0.5, fontface="italic", size=6) +
  annotate("text",label="A", x=400, y=0.45, size=8)

ggsave("../results/plots/spread_vs_lt_y0.pdf")

# leaf length over thickness and spread rate for Year 1

library(ggrepel)

ggplot(pred.y1sum, aes(lt_mean, pred_spread_mean)) +
  geom_point(size=3) +
  geom_errorbar(aes(ymin=pred_spread_mean-pred_spread_sd, ymax=pred_spread_mean+pred_spread_sd),size=0.5)+
  scale_x_continuous("Particle length / thickness", limits=c(0.0,400)) +
  scale_y_continuous("Spread rate (cm/s)", limits=c(0.0,0.45)) +
  scale_color_manual(values = c( "gray50", "black")) +
  theme_bw(base_size = 16) + geom_text_repel(aes(label=display.name), fontface="italic", size=6) +
  annotate("text",label="B", x=400, y=0.45, size=8)

ggsave("../results/plots/spread_vs_lt_y1.pdf")
