## burn trials.R

# read data
burnt <- read.csv("../data/moisture/burn_moisture_trials.csv")
burnt$spcode <- factor(burnt$spcode)

source("theme-opts.R")
#source("read-decomp.R")

## Plots of flammability and moisture levels

## DWS: why show lines when not sig? Why expect linear?
flammoistplot <-  function(resp.var, ylab) {
    pred.var = "moisture"
    r <- ggplot(burnt, aes_string(pred.var, resp.var, colour="spcode")) +
        geom_point(size=1.5) +
        scale_colour_discrete(name="Species", breaks=spbreaks, labels=labels1) +
        xlab("Moisture content (%)") + ylab(ylab) +
        pubtheme +
        stat_smooth(data=burnt, method="lm", se=FALSE, size=1)
    ggsave(file.path("..", "results", "plots",
                     paste(pred.var, "_", resp.var, ".png", sep="")), plot=r)
    # Why png? Why not vector format?
    return(r)
}

flammoistplot("log(t2ignit)", "Ignitability (s)")
flammoistplot("spread", "Spread rate (mm/s)")
flammoistplot("combust", "Combustability (mm)")
flammoistplot("consume", "Consumability (%)")
flammoistplot("sustain", "Sustainability (s)")

# burntr <- merge(subset(decomp.sum, year=="0"), burnt, by="spcode", sort=F)

## Binomial analysis of ignition

ggplot(burnt, aes(moisture, ignit, colour=spcode)) +
        geom_point(size=1.5) +
        stat_smooth(method="glm", family="binomial", se=F) +
        scale_colour_manual(name="Species", breaks=spbreaks, labels=labels2, 
                            values=cbcolours1) +
        pubtheme

fit <- glm(ignit~moisture + spcode + rh + temp, data=burnt, family=binomial())
summary(fit)
