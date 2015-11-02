## Read flammability data

library(plyr)

flam <- read.csv("../data/burn-trials/flamdt.csv")
flam$mixture <- !(flam$spcode=="Abco" | flam$spcode=="Abma" | flam$spcode=="Pipo"
                  | flam$spcode=="Quke" | flam$spcode=="Cade" | flam$spcode==
                  "Pije" | flam$spcode=="Pila" | flam$spcode=="Segi")
flam.singles <- subset(flam, !mixture)
flam.singles$spcode <- factor(flam.singles$spcode)
flam.mixtures <- subset(flam, mixture)
flam$type <- factor(flam$mixture)
levels(flam$type) <- c("monoculture","mixture")

std.err <- function(x) {
            sd(x)/sqrt(length(x))
            }

flam.avg <- ddply(flam, .(spcode,type), summarise,
                  bulk.mean = mean(bulk),
                  bulk.se = std.err(bulk),
                  spread.mean = mean(spread),
                  spread.se = std.err(spread),
                  ignit.mean = mean(ignit),
                  ignit.se = std.err(ignit),
                  combus.mean = mean(combus),
                  combus.se = std.err(combus),
                  consum.mean = mean(consum),
                  consum.se = std.err(consum),
                  sustain.mean = mean(sustain),
                  sustain.se = std.err(sustain),
                  b75.heat100.mean = mean(b75.heat100),
                  b75.heat100.se = std.err(b75.heat100),
                  b75.numsecs100.mean = mean(b75.numsecs100),
                  b75.numsecs100.se = std.err(b75.numsecs100),
                  leaf.area.mean = mean(leaf.area),
                  leaf.area.se = std.err(leaf.area)
                  )

species <- read.csv("../data/species.csv", stringsAsFactors=FALSE)
flam.avg <- merge(flam.avg, species)
