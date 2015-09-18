source("decomp-flam.R")

source("theme-opts.R")

# Doing the graph just for length and putting it in 1 column

a <- ggplot(subset(decomp, year=="0"), aes(x=l, colour=spcode)) + 
          geom_density() + 
          xlab("Length (mm)") + 
          guides(colour=guide_legend(title=("Species"))) +
          scale_colour_manual(values=mycolours) +
          themeopts + 
          theme_bw()

b <- ggplot(subset(decomp, year=="1"), aes(x=l, colour=spcode)) + 
            geom_density() + 
            xlim(0, 300) + 
            xlab("Length (mm)") + 
            guides(colour=guide_legend(title=("Species"))) +
            scale_colour_manual(values=mycolours) +
            themeopts + 
            theme_bw()

c <- ggplot(subset(decomp, year=="2"), aes(x=l, colour=spcode)) + 
            geom_density() + 
            xlim(0, 300) + 
            xlab("Length (mm)") + 
            guides(colour=guide_legend(title=("Species"))) + 
            scale_colour_manual(values=mycolours) +
            themeopts + 
            theme_bw()

png("Decomp_L.png", width=10*ppi, height=15*ppi, res=ppi)
      grid.arrange(a, b, c, ncol=1)
      grid.text("Year 0",0.76,0.95, gp=gpar(fontsize=20))
      grid.text("Year 1",0.76,0.45, gp=gpar(fontsize=20))
      grid.text("Year 2",0.76,0.45, gp=gpar(fontsize=20))
dev.off()

# Graphing leaf length and spread rate

ggplot(flamdecomp, aes(l.mean, spread.mean)) +
        geom_point(size=3) +
        geom_errorbar(aes(ymin=spread.mean-spread.se, ymax=spread.mean+spread.se), 
                      size=0.5)+
        scale_x_continuous(expression(paste("Leaf length (mm)"))) + #, limits=c(0.0, 2000)) +
        scale_y_continuous("Spread rate (cm/s)") + #, limits=c(0.0,0.5)) +
        scale_linetype_manual(values = c(1,3,2)) +
        scale_color_manual(values = c( "gray50", "black")) +
        theme_bw() + themeopts + geom_text(aes(label=spcode),hjust=0, vjust=0)

# Finding the relationship between the species 
lmfit <- lm(spread.mean~l.mean, data=flamdecomp)
summary(lmfit)

lmfit2 <- lm(spread.mean~l.mean + I(l.mean^2), data=flamdecomp)
summary(lmfit2)

### Quke seems to be an outlier, without it the relationship seems quadratic
flamy0q <- subset(flamdecomp, spcode!="Quke")

ggplot(flamy0q, aes(l.mean, spread.mean)) +
       geom_point(size=3) 

# Finding the relationship between the species without Quke
lmfit3 <- lm(spread.mean~l.mean, data=flamy0q)
summary(lmfit3)

lmfit4 <- lm(spread.mean~l.mean + I(l.mean^2), data=flamy0q)
summary(lmfit4)

### Without Quke the r2 is much better

# Creating new dataframe with the predicted spread rate after decomposition (year 1)
new.datay1 <- subset(decomp.avg, year=="1", select=c(spcode, l.mean))

prd <- predict(lmfit4, data=new.datay1$l.mean) 

new.datay1$spread.pred <- prd # does not work, says replacement is smaller than data

# Plotting the two graphs, showing predicted change in spread rate after decomposition

a <- ggplot(flamy0q, aes(l.m)) +
            geom_point(aes(y=spread.mean), colour="blue") +
            geom_text(aes(y=spread.mean, label=spcode),hjust=0, vjust=0) +
            geom_errorbar(aes(ymin=spread.mean-spread.se, ymax=spread.mean+spread.se),
                  size=0.5) +
            scale_x_continuous(expression(paste("Mean leaf length (mm)")),
                      limits=c(0, 205)) +
            scale_y_continuous("Mean spread rate (cm/s)", limits=c(0, 0.33)) +
            themeopts + 
            theme_bw()

b <- ggplot(new.data, aes(l1.mean, spread.prd, colour=spcode)) 
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

png("Spread vs length2.png", width=10*ppi, height=15*ppi, res=ppi)
    grid.arrange(a, b, ncol=1)
    grid.text("Year 0",0.91,0.95, gp=gpar(fontsize=20))
    grid.text("Year 1",0.91,0.45, gp=gpar(fontsize=20))
dev.off()