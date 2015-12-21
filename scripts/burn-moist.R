## burn trials.R

# read data
burnt <- read.csv("../data/moisture/burn_moisture_trials.csv")
#burnt$spcode <- factor(burnt$spcode)

source("theme-opts.R")
source("read-decomp.R")

## Plots of flammability and moisture levels

flammoistplot <-  function(resp.var, ylab) {
    pred.var = "moisture_dry"
    r <- ggplot(burnt, aes_string(pred.var, resp.var, colour="spcode")) +
        geom_point(size=1.5) +
        scale_colour_manual(name="Species", breaks=spbreaks, labels=labels2,
                            values=cbcolours1) +
        xlab("Moisture content (%)") + ylab(ylab) +
        pubtheme +
        stat_smooth(data=burnt, method="lm", se=FALSE, size=1)
    ggsave(file.path("..", "results", "plots",
                     paste(pred.var, "_", resp.var, ".pdf", sep="")), plot=r)
    return(r)
}

flammoistplot("log(t2ignit)", "Ignitability (s)")
flammoistplot("spread", "Spread rate (mm/s)")
flammoistplot("combust", "Combustability (mm)")
flammoistplot("consum", "Consumability (%)")
flammoistplot("sustain", "Sustainability (s)")

# burntr <- merge(subset(decomp.sum, year=="0"), burnt, by="spcode", sort=F)

## Binomial analysis of ignition

ggplot(burnt, aes(moisture_dry, ignit, colour=spcode)) +
        geom_point(size=1.5) +
        stat_smooth(method="glm", family="binomial", se=F) +
      	scale_x_continuous(breaks=xbreaks, limits=c(10,65)) +
        xlab("Moisture content (%)") + ylab("Probability of ignition") +
        scale_colour_manual(name="Species", breaks=spbreaks, labels=labels2, 
                            values=cbcolours1) +
        pubtheme + theme(axis.title.x = element_text(size=16),
                         axis.text.x  = element_text(size=14),
                         axis.title.y = element_text(size=16),
                         axis.text.y  = element_text(size=14))

fit <- glm(ignit~moisture + spcode + rh + temp, data=burnt, family=binomial())
summary(fit)
