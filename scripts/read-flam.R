## Read flammability trial data

#library(dplyr)

flam <- read.csv("../data/burn-trials/flamdt.csv", stringsAsFactors=FALSE)
flam <- flam %>% mutate(mixture=nchar(spcode) > 4,
                        type=factor(mixture, labels=c("monoculture", "mixture")))

# DWS: Note that below could be made much shorter using dplyr::summarise_each.
# But default column names would hten end up like spread_mean and spread_se
# which would affect downstream code. It might be possible to change this name
# scheme in the summarise_each call, but I'm not sure how to do so.
flam.avg <- flam %>% group_by(spcode, type) %>%
    summarise(bulk.mean = mean(bulk),
              bulk.sd = sd(bulk),
              spread.mean = mean(spread),
              spread.sd = sd(spread),
              ignit.mean = mean(ignit),
              ignit.sd = sd(ignit),
              combus.mean = mean(combus),
              combus.sd = sd(combus),
              consum.mean = mean(consum),
              consum.sd = sd(consum),
              sustain.mean = mean(sustain),
              sustain.sd = sd(sustain),
              b75.heat100.mean = mean(b75.heat100),
              b75.heat100.sd = sd(b75.heat100),
              b75.numsecs100.mean = mean(b75.numsecs100),
              b75.numsecs100.sd = sd(b75.numsecs100),
              leaf.area.mean = mean(leaf.area),
              leaf.area.sd = sd(leaf.area)
              )

# just monocultures:
species <- read.csv("../data/species.csv", stringsAsFactors=FALSE)
splevels = c("S. giganteum", "C. decurrens", "A. magnifica", "A. concolor", "P. ponderosa",
             "P. lambertiana", "P. jeffreyi", "Q. kelloggii")
species <- mutate(species, display.name = factor(display.name, levels=splevels))
flam.sp.avg <- merge(flam.avg, species)
