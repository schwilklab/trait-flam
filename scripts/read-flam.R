## Read flammability trial data

library(dplyr)

flam <- read.csv("../data/burn-trials/flamdt.csv", stringsAsFactors=FALSE)
flam <- flam %>% mutate(mixture=nchar(spcode) > 4,
                        type=factor(mixture, labels=c("monoculture", "mixture")))

std.err <- function(x) {
            sd(x)/sqrt(length(x))
            }

# DWS: Note that below could be made much shorter using dplyr::summarise_each.
# But default column names would hten end up like spread_mean and spread_se
# which would affect downstream code. It might be possible to change this name
# scheme in the summarise_each call, but I'm not sure how to do so.
flam.avg <- flam %>% group_by(spcode, type) %>%
    summarise(bulk.mean = mean(bulk),
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

# just monocultures:
species <- read.csv("../data/species.csv", stringsAsFactors=FALSE)
splevels = c("S. giganteum", "C. decurrens", "A. concolor", "A. magnifica", "P. ponderosa",
             "P. lambertiana", "P. jeffreyi", "Q. kelloggii")
species <- mutate(species, display.name = factor(display.name, levels=splevels))
flam.sp.avg <- merge(flam.avg, species)
