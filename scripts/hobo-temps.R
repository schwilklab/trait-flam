# Read hobo data logger temperature data, split into sections by burn trial, and
# summarize for each trial
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

Sys.setenv(TZ = "America/Chicago") #set sys tz can fix timezone warning
TZ = "CST6CDT" #timezone does not work with getting error"

#source("./clean_up_trial.R") #grab basic trial summary data
## DWS: lets call this from parent script so it only gets called once

read_hobo_file <- function(filename) {
    hobo <- read.csv(filename, skip=2, header=FALSE)
    names(hobo)[1:3] <- c("row", "time", "temp")
    hobo <- hobo %>% select(time, temp) %>%
        filter(!is.na(temp)) %>%
        # we use floor_date() below to round to seconds so we can line up our
        # measurements across HOBOs
        mutate(time = floor_date(mdy_hms(time, tz=TZ), "second"))
    return(hobo) #change timezone
}


concat_hobo_files <- function(filelist, label){
    l <- lapply(filelist, read_hobo_file)
    r <- bind_rows(l)
    names(r) <- c("time", label)
    return(r)
}

# get sets for each of four thermocouple locations
hobo_list <- list()
for (ht in c("b", "m", "h")) {
  for (hloc in c("0", "75", "150")) {
    locname <- paste(ht, hloc, sep="")
    pat <- paste(hloc, ht, ".csv", sep="")
    hobo <- concat_hobo_files(list.files("../data/burn-trials/HOBO/",
                                         full.names=TRUE, pattern=pat,
                                         recursive=TRUE), locname)
    hobo_list[[locname]] <- hobo
  }
}

# Now merge all of these together to get one continuous time series (wide
# data). Do we even need this? Really only necessary if we ever compare temps
# across thermocouples.
thermocouples.wide <- plyr::join_all(hobo_list)

# read trial intervals
trials <- read.csv("../data/moisture/burn_moisture_trials.csv")
trials <- mutate(trials, year=2015,
                 start = paste(paste(year, month, day, sep="-"),
                                       paste(time, "0", sep=":")))

## TODO GET INTERVALS

## get a trial ID from a time point
get_trial_id <- function(time) {
    matches <- time %within% trials$interval
    if(! any(matches)) return(NA)
    return(trials$trial.id[which.max(matches)])
}


# assign trial ids
thermocouples.wide$trial.id <- unlist(sapply(thermocouples.wide$time, get_trial_id))




# throw away data outside of a trial
thermocouples.wide <- thermocouples.wide %>% filter(! is.na(trial.id))

# Long form data easier for making per thermcouple summaries below.
thermocouples.long <- thermocouples.wide %>% gather(location, temperature, -time, -trial.id)

## then do the summary
threshold=100 # temperature threshold in degrees C

temps.sum <- thermocouples.long %>% group_by(trial.id, location) %>%
    summarise(dur = sum(temperature > threshold),
              degsec = sum(temperature[temperature > threshold]),
              peak.temp = max(temperature, na.rm=TRUE),
              peak.time = time[which(peak.temp == temperature)[1]],
              num.NA = sum(is.na(temperature)))

# ok, dcast in reshape2 was nicer about m,ultiple value columns than is
# spread(). So since we just have two locations, I'll do this manually:

temps.sum.base <- filter(temps.sum, location=="base") %>% select(-location)
temps.sum.canopy <- filter(temps.sum, location=="canopy") %>% select(-location)
temps.sum <- left_join(temps.sum.base, temps.sum.canopy, by = "trial.id", suffix = c("_base", "_canopy"))

#clean up env
rm("concat_hobo_files", "get_trial_id", "read_hobo_file", "threshold",
   "thermocouples.wide", "thermocouples.long", "height10", "height20",
   "height40", "basea", "baseb", "temps.sum.base", "temps.sum.canopy")

# exports temps.sum to workspace

   
