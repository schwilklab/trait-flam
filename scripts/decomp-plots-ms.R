## Creates the figures for the decomposition ms

source("decomp-flam.R")
source("theme-opts.R")

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

#plotting 2 datasets in one plot

library(directlabels)
fig2 <- ggplot(flamdecomppredjoin, aes(l_mean, spread_mean, group=year)) +
  geom_point(size=3) +
  geom_pointrange(aes(ymin=spread_mean-spread_sd,
                      ymax=spread_mean+spread_sd),
                  size=0.5) +
  scale_x_continuous("Leaf Particle length (mm)", limits=c(0.0,200)) +
  scale_y_continuous("Spread rate (cm/s)") +
  scale_color_manual(values = c( "gray50", "black")) +
  theme_bw(base_size = 16) +
  geom_line(aes(group=display.name)) + 
  geom_dl(aes(label=display.name), method = list("smart.grid", cex = 1))

fig2

ggsave("../results/plots/spread_vs_l_both.pdf", width=15, height=10)

## Dylan's code:

fig2 <- ggplot(flamdecomp.sum.y0, aes(l_mean, spread.mean)) +
  geom_point(size=3) +
  geom_pointrange(data=flamdecomp.sum.y0,
                  aes(ymin=spread.mean-spread.se,
                      ymax=spread.mean+spread.se),
                  size=0.5) +
  scale_x_continuous("Leaf Particle length (mm)", limits=c(0.0,230)) +
  scale_y_continuous("Spread rate (cm/s)") +
  scale_color_manual(values = c( "gray50", "black")) +
  theme_bw(base_size = 16) +
  geom_text(aes(label=display.name),
            hjust=-0.1, vjust=0.5, fontface="italic", size=5)

fig2
## add predicted:
fig2 <- fig2 +
  geom_point(data=pred.y1sum, aes(l_mean, pred_spread_mean), shape=1, size=3) +
  ggrepel::geom_text_repel(data=pred.y1sum, aes(l_mean, pred_spread_mean, label=display.name),
                           fontface="italic", size=5)

fig2 <- fig2 + geom_pointrange(data=pred.y1sum,
                aes(y=pred_spread_mean, ymin=pred_spread_mean - pred_spread_sd,
                    ymax=pred_spread_mean + pred_spread_sd),
                size=1)
fig2

ggsave("../results/plots/spread_vs_l_both.pdf", plot=fig2, width=15, height=10)

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
