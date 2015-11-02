## Creates plots for the decomposition data

source("decomp-flam.R")

source("theme-opts.R")

# length over thickness
ggplot(decomp, aes(x=l/t)) + 
       geom_density() +
       scale_x_log10("Length / thickness", limits = c(1, 500)) +
       facet_grid( spcode ~ year)


# Doing the graph just for length (all years) and putting it in 1 column
a <- ggplot(subset(decomp, year=="0"), aes(x=l, colour=spcode)) + 
            geom_density() + 
            xlab("Length (mm)") + 
            annotate(geom = "text", label="Year 0", x=155, y=.025) +
            guides(colour=guide_legend(title=("Species"))) +
            scale_colour_manual(values=mycolours, breaks=spbreaks, labels=labels1) +
            themeopts + 
            theme_bw()

b <- ggplot(subset(decomp, year=="1"), aes(x=l, colour=spcode)) + 
            geom_density() + 
            xlim(0, 300) + 
            xlab("Length (mm)") + 
            annotate(geom = "text", label="Year 1", x=155, y=.12) +
            guides(colour=guide_legend(title=("Species"))) +
            scale_colour_manual(values=mycolours, breaks=spbreaks, labels=labels1) +
            themeopts + 
            theme_bw()

c <- ggplot(subset(decomp, year=="2"), aes(x=l, colour=spcode)) + 
            geom_density() + 
            xlim(0, 300) + 
            xlab("Length (mm)") + 
            annotate(geom = "text", label="Year 2", x=155, y=.15) +
            guides(colour=guide_legend(title=("Species"))) + 
            scale_colour_manual(values=mycolours, breaks=spbreaks, labels=labels1) +
            themeopts + 
            theme_bw()

png("../results/plots/Decomp_L.png", width=10*ppi, height=15*ppi, res=ppi)
      grid.arrange(a, b, c, ncol=1)
      #grid.text("Year 0",0.76,0.95, gp=gpar(fontsize=20))
      #grid.text("Year 1",0.76,0.45, gp=gpar(fontsize=20))
      #grid.text("Year 2",0.76,0.45, gp=gpar(fontsize=20))
dev.off()

# Doing boxplots instead of density plots

ggplot(decomp, aes(spcode, l)) + 
        geom_boxplot(aes(fill=year)) +
        xlab("Species") +
        ylab("Length (mm)") +
        scale_fill_manual(values=mycolours2, name="Year") +
        themeopts +
        theme_bw() 

ggsave("../results/plots/Decomp_L_box.pdf", width=15, height=10)

# Graphing leaf length and spread rate for Year 0

ggplot(flamdecomp, aes(l.mean, spread.mean)) +
        geom_point(size=3) +
        geom_errorbar(aes(ymin=spread.mean-spread.se, ymax=spread.mean+spread.se), 
                size=0.5)+
        scale_x_continuous(expression(paste("Leaf length (mm)"))) + #, limits=c(0.0, 2000)) +
        scale_y_continuous("Spread rate (cm/s)") + #, limits=c(0.0,0.5)) +
        scale_linetype_manual(values = c(1,3,2)) +
        scale_color_manual(values = c( "gray50", "black")) +
        theme_bw() + themeopts + geom_text(aes(label=spcode),hjust=0, vjust=0)


# Plotting the two graphs, showing predicted change in spread rate after decomposition

a <- ggplot(flamy0q, aes(l.mean)) +
            geom_point(aes(y=spread.mean), colour="blue") +
            geom_text(aes(y=spread.mean, label=spcode),hjust=0, vjust=0) +
            geom_errorbar(aes(ymin=spread.mean-spread.se, ymax=spread.mean+spread.se),
                size=0.5) +
            scale_x_continuous(expression(paste("Mean leaf length (mm)")),
                limits=c(0, 210)) +
            scale_y_continuous("Mean spread rate (cm/s)", limits=c(0, 0.33)) +
            annotate(geom = "text", label="Year 1", x=145, y=.4) +
            themeopts + 
            theme_bw()

b <- ggplot(new.dfy1q, aes(l1.mean, spread.prd, colour=spcode)) 
            + geom_boxplot(alpha=0, lwd=1) 
            + scale_colour_manual(values=mycolours, name="Species", 
                      breaks=spbreaks, 
                      labels=labels2) 
            + scale_x_continuous(expression(paste("Mean leaf length (mm)")), 
                     limits=c(0, 205)) 
            + scale_y_continuous("Mean spread rate (cm/s)")
            + themeopts 
            + theme_bw() 
            + theme(legend.justification=c(1,0), legend.position=c(1,0), 
                  legend.text = element_text(colour="black", size = 14, face = "italic"), 
                  legend.title = element_text(colour="black", size=14, face="bold")
                  )

png("../results/plots/Spread vs length2.png", width=10*ppi, height=15*ppi, res=ppi)
    grid.arrange(a, b, ncol=1)
    grid.text("Year 0",0.91,0.95, gp=gpar(fontsize=20))
    grid.text("Year 1",0.91,0.45, gp=gpar(fontsize=20))
dev.off()
