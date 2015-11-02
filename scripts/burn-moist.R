## burn trials.R

# read data
burnt <- read.csv("../data/moisture/burn_moisture_trials.csv")

burnt$spcode <- factor(burnt$spcode)

source("theme-opts.R")
source("read-decomp.R")

# Plots of flammability and moisture levels

ggplot(burnt, aes(moisture, log(t2ignit), colour=spcode)) +
        geom_point(size=1.5) +
        scale_colour_discrete(name="Species", breaks=spbreaks, labels=labels1) +
        xlab("Moisture content (%)") + ylab("Ignitability (s)") +
        theme_bw() + themeopts +
        stat_smooth(data=burnt, method="lm", se=FALSE, size=1)

ggsave("../results/plots/moisture_ignit.png", width=9, height=5, dpi=ppi)

ggplot(burnt, aes(moisture, spread, colour=spcode)) +
        geom_point(size=1.5) +
        scale_colour_discrete(name="Species", breaks=spbreaks, labels=labels1) +
        xlab("Moisture content (%)") + ylab("Spread rate (mm/s)") +
        theme_bw() + themeopts +
        stat_smooth(data=burnt, method="lm", se=FALSE, size=1)

ggsave("../results/plots/moisture_spread.png", width=9, height=5, dpi=ppi)


ggplot(burnt, aes(moisture, combust, colour=spcode)) +
        geom_point(size=1.5) +
        scale_colour_discrete(name="Species", breaks=spbreaks, labels=labels1) +
        xlab("Moisture content (%)") + ylab("Combustability (mm)") +
        theme_bw() + themeopts +
        stat_smooth(data=burnt, method="lm", se=FALSE, size=1)

ggsave("../results/plots/moisture_combust.png", width=9, height=5, dpi=ppi)

ggplot(burnt, aes(moisture, consum, colour=spcode)) +
        geom_point(size=1.5) +
        scale_colour_discrete(name="Species", breaks=spbreaks, labels=labels1) +
        xlab("Moisture content (%)") + ylab("Consumability (%)") +
        theme_bw() + themeopts +
        stat_smooth(data=burnt, method="lm", se=FALSE, size=1)

ggsave("../results/plots/moisture_consum.png", width=9, height=5, dpi=ppi)

ggplot(burnt, aes(moisture, sustain, colour=spcode)) +
        geom_point(size=1.5) +
        scale_colour_discrete(name="Species", breaks=spbreaks, labels=labels1) +
        xlab("Moisture content (%)") + ylab("Sustainability (s)") +
        theme_bw() + themeopts +
        stat_smooth(data=burnt, method="lm", se=FALSE, size=1)

ggsave("../results/plots/moisture_sustain.png", width=9, height=5, dpi=ppi)

burntr <- merge(subset(decomp.sum2, year=="0"), burnt, by="spcode", sort=F)

## Binomial analysis of ignition

ggplot(burnt, aes(moisture, ignit, colour=spcode)) +
        geom_point(size=1.5) +
        stat_smooth(method="glm", family="binomial", se=F) +
        scale_colour_manual(name="Species", breaks=spbreaks, labels=labels2, 
                            values=cbcolours1) +
        theme_bw()

fit <- glm(ignit~moisture + spcode + rh + temp, data=burnt, family=binomial())
