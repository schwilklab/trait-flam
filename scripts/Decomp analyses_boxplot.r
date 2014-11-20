library(ggplot2)
library(plyr)

Y0 <- read.csv("Decomp_Y0.csv")
Y1 <- read.csv("Decomp_Y1.csv")

Y0$spcode <- factor(Y0$spcode)
Y1$spcode <- factor(Y1$spcode)
Y0$year <- factor(Y0$year)
Y1$year <- factor(Y1$year)
Y0$tag <- factor(Y0$tag)
Y1$tag <- factor(Y1$tag)


y0.avg <- ddply(Y0, .(spcode), summarize, l.mean=mean(l))
y1.avg <- ddply(Y1, .(spcode, tag, year, asp, alt), summarize, l1.mean=mean(l))


flam <- read.csv("flamdt.csv")
flam$spcode <- factor(flam$spcode)

flam$mixture <- !(flam$spcode=="Abco" | flam$spcode=="Abma" | flam$spcode=="Pipo" | 
                        flam$spcode=="Quke" | flam$spcode=="Cade" | flam$spcode=="Pije" | 
                        flam$spcode=="Pila" | flam$spcode=="Segi")
flam.singles <- subset(flam, !mixture)
flam.singles$spcode <- factor(flam.singles$spcode)
flam.mixtures <- subset(flam, mixture)
flam$type <- factor(flam$mixture)
levels(flam$type) <- c("monoculture","mixture")

std.err <- function(x) {
  sd(x)/sqrt(length(x))
}

flam.avg <- ddply(flam, .(spcode,type), summarise, bulk.mean = mean(bulk), bulk.se = std.err(bulk), spread.mean = mean(spread), spread.se = std.err(spread),  ignit.mean = mean(ignit), ignit.se = std.err(ignit),  combus.mean = mean(combus), combus.se = std.err(combus),  consum.mean = mean(consum), consum.se = std.err(consum),  sustain.mean = mean(sustain), sustain.se = std.err(sustain),  b75.heat100.mean = mean(b75.heat100), b75.heat100.se = std.err(b75.heat100),  b75.numsecs100.mean = mean(b75.numsecs100), b75.numsecs100.se = std.err(b75.numsecs100),  leaf.area.mean = mean(leaf.area), leaf.area.se = std.err(leaf.area))

flamy0 <- merge(subset(flam.avg, type="monoculture"), y0.avg, by="spcode", sort=F)

bestfit <- geom_smooth(method="lm",se = F, color = "black", size=1.5)
isoline <- geom_abline(color="black", size=1.5, linetype="dashed") 
textsize <- 20
themeopts <-  theme( axis.title.y = element_text(size = textsize, angle = 90, vjust=0.3) ,axis.title.x = element_text(size = textsize, vjust=-0.3), panel.background = element_rect(size = 2), axis.text.x  = element_text(size=20), axis.text.y  = element_text(size=20), panel.grid.minor = element_line(colour = NA), panel.grid.major = element_line(colour = NA))

ggplot(flamy0, aes(l.mean, spread.mean)) +
    geom_point(size=3) +
    geom_errorbar(aes(ymin=spread.mean-spread.se, ymax=spread.mean+spread.se), size=0.5)+
    scale_x_continuous(expression(paste("Leaf length (mm)"))) + #, limits=c(0.0, 2000)) +
    scale_y_continuous("Spread rate (cm/s)") + #, limits=c(0.0,0.5)) +
       #bestfit +  # NOte this is not the correct line. you need to pull the coef from the model.
       #geom_smooth(method="lm", se=F, size=1.5, colour="black") + 
    scale_linetype_manual(values = c(1,3,2)) +
    scale_color_manual(values = c( "gray50", "black")) +
    theme_bw() + themeopts + geom_text(aes(label=spcode),hjust=0, vjust=0)
	   
lmfit <- lm(spread.mean~l.mean, data=flamy0)
summary(lmfit)

lmfit2 <- lm(spread.mean~l.mean + I(l.mean^2), data=flamy0)
summary(lmfit2)


flamy0q <- subset(flamy0, spcode!="Quke")

ggplot(flamy0q, aes(l.mean, spread.mean)) +
       geom_point(size=3) 

lmfit3 <- lm(spread.mean~l.mean, data=flamy0q)
summary(lmfit3)

lmfit4 <- lm(spread.mean~l.mean + I(l.mean^2), data=flamy0q)
summary(lmfit4)

#Residual standard error: 0.03828 on 4 degrees of freedom
#Multiple R-squared: 0.8706,     Adjusted R-squared: 0.8059 
#F-statistic: 13.45 on 2 and 4 DF,  p-value: 0.01675

beta <- coef(lmfit4) #extracts the coefficients for the model
beta
fitcurve <- function(x) {-0.6192 + 0.01405*x -0.00005043*x^2}

new.data <- subset(y1.avg, select=c(spcode,l1.mean))

new.data$spread.pred <- (-0.6192 + 0.01405*(new.data$l1.mean) -0.00005043*((new.data$l1.mean)^2))
new.data[new.data<0] = 0

mycolours <- c("#FF6600", "#CC0000", "#660066", "#000999", "#3366CC", "#00CCFF", "#00FF33", "#006600")

a <- ggplot(flamy0q, aes(l.mean)) + geom_point(aes(y=spread.mean), colour="blue") + geom_text(aes(y=spread.mean, label=spcode),hjust=0, vjust=0) + geom_errorbar(aes(ymin=spread.mean-spread.se, ymax=spread.mean+spread.se), size=0.5) + scale_x_continuous(expression(paste("Mean leaf length (mm)")), limits=c(0, 205))+ scale_y_continuous("Mean spread rate (cm/s)", limits=c(0, 0.33)) + themeopts + theme_bw()

b <- ggplot(new.data, aes(l1.mean, spread.prd, colour=spcode)) + geom_boxplot(alpha=0, lwd=1) + scale_colour_manual(values=mycolours, name="Species", breaks=c("Abco", "Abma", "Cade", "Pije", "Pila", "Pipo", "Quke", "Segi"), labels=c(expression(italic("Abies concolor")), expression(italic("Abies magnifica")), expression(italic("Calocedrus decurrens")), expression(italic("Pinus jeffreyi")), expression(italic("Pinus lambertiana")), expression(italic("Pinus ponderosa")), expression(italic("Quercus kelloggii")), expression(italic("Sequoiadendron giganteum")))) + scale_x_continuous(expression(paste("Mean leaf length (mm)")), limits=c(0, 205)) + scale_y_continuous("Mean spread rate (cm/s)")+ themeopts + theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0), legend.text = element_text(colour="black", size = 14, face = "italic"), legend.title = element_text(colour="black", size=14, face="bold"))

library(gridExtra)
ppi <- 300
png("Spread vs length2.png", width=10*ppi, height=15*ppi, res=ppi)
grid.arrange(a, b, ncol=1)
grid.text("Year 0",0.91,0.95, gp=gpar(fontsize=20))
grid.text("Year 1",0.91,0.45, gp=gpar(fontsize=20))
dev.off()

