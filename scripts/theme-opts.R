## Some theme options for graphs to be sourced in

library(ggplot2)
library(gridExtra)

bestfit <- geom_smooth(method="lm",se = F, color = "black", size=1.5)
isoline <- geom_abline(color="black", size=1.5, linetype="dashed") 
textsize <- 14
themeopts <-  theme(axis.title.y = element_text(size = textsize, angle = 90, vjust=0.3),
                    axis.title.x = element_text(size = textsize, vjust=-0.3),
                    panel.background = element_rect(size = 2),
                    axis.text.x  = element_text(size=14),
                    axis.text.y  = element_text(size=14),
                    panel.grid.minor = element_line(colour = NA),
                    panel.grid.major = element_line(colour = NA)
                    )

ppi <- 300

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

cbcolours1 <- c("#88CCEE", "#332288", "#44AA99", "#117733", "#DDCC77", "#CC6677", 
                "#882255", "#AA4499")

cbcolours2 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
                "#D55E00", "#CC79A7")
