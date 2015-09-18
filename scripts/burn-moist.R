## burn trials.R

# read data
burnt <- read.csv("../data/moisture/burn_moisture_trials.csv")

source("theme-opts.R")

# Plots of flammability and moisture levels

ggplot(burnt, aes(moisture, t2ignit, colour=spcode)) +
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


