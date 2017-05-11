## Some theme options for graphs to be sourced in

library(ggplot2)
library(gridExtra)
library(scales)
library(extrafont)
#font_import(pattern="Arial")
loadfonts()

# constants and theme for publications
bestfit <- geom_smooth(method="lm",se = F, color = "black", size=1.5)
textsize <- 12
smsize <- textsize-2
pt2mm <- 0.35146
smsize.mm <- smsize*pt2mm
fontfamily = "Arial"  # Some journals want this, others happy with open source fonts
col2 <- 17.5 # cm  -- adjust for journal specfic column sizes.
col1 <- 8.0 # cm -- make sure to indicate units when using ggsave!
ppi <- 300 # for raster formats, not recommended

pubtheme <- theme_grey() +
  theme(axis.title.y = element_text(family=fontfamily,
                                    size = textsize, angle = 90, vjust=0.3),
        axis.title.x = element_text(family=fontfamily, size = textsize, vjust=-0.3),
        axis.ticks = element_line(colour = "black"),
        panel.background = element_rect(size = 1.6, fill = NA),
        panel.border = element_rect(size = 1.6, fill=NA),
        axis.text.x  = element_text(family=fontfamily, size=smsize, color="black"),
        axis.text.y  = element_text(family=fontfamily, size=smsize, color = "black"),
        ## strip.text.x = element_text(family=fontfamily, size = smsize, face="italic"),
        ## strip.text.y = element_text(family=fontfamily, size = smsize, face="italic"),
        legend.title = element_text(family=fontfamily, size=textsize),
        legend.text = element_text(family=fontfamily, size=smsize, face="italic"),
        legend.key = element_rect(fill=NA),
        panel.grid.major = element_line(colour = "grey90", size = 0.2),
        panel.grid.minor = element_line(colour = "grey95", size =0.5),
        #    panel.grid.minor = element_blank(),
        #    panel.grid.major = element_blank(),
        strip.background = element_rect(fill = "grey80", colour = "grey50")      
        )

pubtheme.nogridlines <- pubtheme +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())

stat_sum_single <- function(fun, geom="point", ...) {
  stat_summary(fun.y=fun, geom=geom, size = 3, ...)
}


spbreaks <- c("Abco", "Abma", "Cade", "Pije", "Pila", "Pipo", "Quke", "Segi")

mspbreaks <- c("AbPiQu", "AbCaQu", "CaPiQu", "AbCaPi")

labels1 <- c("White Fir", "Red Fir", "Incense Cedar", "Jeffrey Pine",
            "Sugar Pine", "Ponderosa Pine", "Black Oak", "Sequoia")
labels2=c(expression(italic("Abies concolor")), 
         expression(italic("Abies magnifica")), 
         expression(italic("Calocedrus decurrens")), 
         expression(italic("Pinus jeffreyi")), 
         expression(italic("Pinus lambertiana")), 
         expression(italic("Pinus ponderosa")), 
         expression(italic("Quercus kelloggii")), 
         expression(italic("Sequoiadendron giganteum")))

#To modify the colour scheme into colours of my choosing:
mycolours <- c("#FF6600", "#CC0000", "#660066", "#000999", "#3366CC", "#00CCFF", 
               "#00FF33", "#006600")
# + scale_colour_manual(values=mycolours)

mycolours2 <- c("#228B22", "#FFC125", "#8B4513")

mycolours3 <- c("slateblue1", "green", "turquoise3", "navy", "forestgreen")

cbcolours1 <- c("#88CCEE", "#332288", "#44AA99", "#117733", "#DDCC77", "#CC6677", 
                "#882255", "#AA4499")

cbcolours2 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
                "#D55E00", "#CC79A7")
ritatheme <- theme_grey() +
  theme(axis.title.y = element_text(size = 14, angle = 90, vjust=0.3),
        axis.title.x = element_text(size = 14, vjust=-0.3),
        axis.ticks = element_line(colour = "black"),
        panel.background = element_rect(size = 1.6, fill = NA),
        panel.border = element_rect(size = 1.6, fill=NA),
        axis.text.x  = element_text(size=13, color="black"),
        axis.text.y  = element_text(size=13, color = "black"),
        ## strip.text.x = element_text(family=fontfamily, size = 11, face="italic"),
        ## strip.text.y = element_text(family=fontfamily, size = 11, face="italic"),
        legend.title = element_text(size=14),
        legend.text = element_text(size=13, face="italic"),
        legend.key = element_rect(fill=NA),
        panel.grid.major = element_line(colour = "grey90", size = 0.2),
        panel.grid.minor = element_line(colour = "grey95", size =0.5),
        #    panel.grid.minor = element_blank(),
        #    panel.grid.major = element_bank(),
        strip.background = element_rect(fill = "grey80", colour = "grey50")      
  )
