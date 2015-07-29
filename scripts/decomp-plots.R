## DWS: I don't really get this script, but am including it for now. Ther are a
## lot of porblems with tis code. Fr now, I'm just trying to refactor, however.


source("decomp-flam.R")

source("theme-opts.R")


ggplot(flamdecomp, aes(l.m, spread.mean)) +
        geom_point(size=3) +
        geom_errorbar(aes(ymin=spread.mean-spread.se, ymax=spread.mean+spread.se), size=0.5)+
        scale_x_continuous(expression(paste("Leaf length (mm)"))) + #, limits=c(0.0, 2000)) +
        scale_y_continuous("Spread rate (cm/s)") + #, limits=c(0.0,0.5)) +
       #bestfit +  # NOte this is not the correct line. you need to pull the coef from the model.
       #geom_smooth(method="lm", se=F, size=1.5, colour="black") + 
        scale_linetype_manual(values = c(1,3,2)) +
        scale_color_manual(values = c( "gray50", "black")) +
        theme_bw() + themeopts + geom_text(aes(label=spcode),hjust=0, vjust=0)
	   
lmfit <- lm(spread.mean~l.m, data=flamdecomp)
summary(lmfit)

lmfit2 <- lm(spread.mean~l.m + I(l.m^2), data=flamdecomp)
summary(lmfit2)


flamy0q <- subset(flamdecomp, spcode!="Quke")

ggplot(flamy0q, aes(l.m, spread.mean)) +
       geom_point(size=3) 

lmfit3 <- lm(spread.mean~l.m, data=flamy0q)
summary(lmfit3)

lmfit4 <- lm(spread.mean~l.m + I(l.m^2), data=flamy0q)
summary(lmfit4)

#Residual standard error: 0.03828 on 4 degrees of freedom
#Multiple R-squared: 0.8706,     Adjusted R-squared: 0.8059 
#F-statistic: 13.45 on 2 and 4 DF,  p-value: 0.01675

new.data <- subset(decomp.sum, year=="1", select=c(spcode, l.mean))

prd <- predict(lmfit4, data=new.data) 

new.data$spread.pred <- prd

mycolours <- c("#FF6600", "#CC0000", "#660066", "#000999", "#3366CC",
               "#00CCFF", "#00FF33", "#006600")

a <- ggplot(flamy0q, aes(l.mean)) +
    geom_point(aes(y=spread.mean), colour="blue") +
    geom_text(aes(y=spread.mean, label=spcode),hjust=0, vjust=0) +
    geom_errorbar(aes(ymin=spread.mean-spread.se, ymax=spread.mean+spread.se),
                  size=0.5) +
    scale_x_continuous(expression(paste("Mean leaf length (mm)")),
                      limits=c(0, 205)) +
    scale_y_continuous("Mean spread rate (cm/s)", limits=c(0, 0.33)) +
    themeopts + theme_bw()



b <- ggplot(new.data, aes(l1.mean, spread.prd, colour=spcode)) 
    + geom_boxplot(alpha=0, lwd=1) 
    + scale_colour_manual(values=mycolours, name="Species", 
                          breaks=c("Abco", "Abma", "Cade", "Pije", "Pila", "Pipo", "Quke", "Segi"), 
                          labels=c(expression(italic("Abies concolor")), 
                                   expression(italic("Abies magnifica")), 
                                   expression(italic("Calocedrus decurrens")), 
                                   expression(italic("Pinus jeffreyi")), 
                                   expression(italic("Pinus lambertiana")), 
                                   expression(italic("Pinus ponderosa")), 
                                   expression(italic("Quercus kelloggii")), 
                                   expression(italic("Sequoiadendron giganteum")))) 
    + scale_x_continuous(expression(paste("Mean leaf length (mm)")), limits=c(0, 205)) 
    + scale_y_continuous("Mean spread rate (cm/s)")
    + themeopts 
    + theme_bw() 
    + theme(legend.justification=c(1,0), legend.position=c(1,0), 
            legend.text = element_text(colour="black", size = 14, face = "italic"), 
            legend.title = element_text(colour="black", size=14, face="bold"))

## DWS: wrap!

library(gridExtra)
ppi <- 300
png("Spread vs length2.png", width=10*ppi, height=15*ppi, res=ppi)
grid.arrange(a, b, ncol=1)
grid.text("Year 0",0.91,0.95, gp=gpar(fontsize=20))
grid.text("Year 1",0.91,0.45, gp=gpar(fontsize=20))
dev.off()

