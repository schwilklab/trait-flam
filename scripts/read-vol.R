# Volatiles

## read-vol.R

## Volatiles data.

## 1. Read in files, turn from wide to long
## 2. Create summary statistics data frame
## 3. Prepare data frames for stats (join with flammability and decomp)

library(tidyr)
library(dplyr)

species <- read.csv("../data/species.csv", stringsAsFactors=FALSE)

## 1. Read in files, turn from wide to long

vol_wide <- read.csv("../data/volatiles/vol_area_wide.csv", na.strings = c("","NA"))

vol <- vol_wide %>% gather(key=spcode, value=area, -name, -group_more, -group_less, -terpene, -flash_point) %>%
    separate(spcode, c("spcode", "replicate"), "\\.") %>%
    select(spcode, replicate, name, group_more, group_less, terpene, flash_point, area) %>%
    filter(complete.cases(.))

vol$terpene <- as.factor(vol$terpene)

## 2. Create summary statistics data frames

#By name, major volatile group, and flash point

vol.avg <- vol %>% group_by(spcode, name, group_less, terpene) %>% 
  summarise(mean_area=mean(area), sd_area=sd(area), 
            fp_wmean=weighted.mean(flash_point, area),
            fp_mean=mean(flash_point),
            sum_low_fp = sum(area[flash_point < 100]))

vol.sum.grp <- vol %>% group_by(spcode, replicate, group_less, terpene) %>%  
  summarise(area=sum(area), 
            fp_mean=mean(flash_point),
            sum_low_fp = sum(area[flash_point < 100])) %>%  
  group_by(spcode, group_less, terpene) %>%  
  summarise(mean_area=mean(area), sd_area=sd(area), 
            fp_wmean=weighted.mean(fp_mean, area),
            fp_mean=mean(fp_mean),
            sum_low_fp = mean(sum_low_fp)) %>%
  left_join(species, by="spcode")

vol.sum.fp <- vol %>% group_by(spcode, replicate, terpene, flash_point) %>% 
  summarise(area=sum(area)) %>% group_by(spcode, terpene, flash_point) %>%
  summarise(mean_area=mean(area), sd_area=sd(area))

#By species (total volatile content)
vol.tv <- vol %>% group_by(spcode, replicate) %>% 
  summarise(sum_area=sum(area), sum_low_fp = sum(area[flash_point < 100]))
  
vol.tv.mean <- vol.tv %>% group_by(spcode) %>% 
  summarise(mean_area=mean(sum_area), sd_area=sd(sum_area),
            mean_low_fp = mean(sum_low_fp), sd_low_fp = sd(sum_low_fp)) %>% 
           left_join(species, by="spcode") #for graphing


#By species (total terpene content only)
vol.tt <- vol %>% group_by(spcode, replicate, terpene) %>% 
  summarise(sum_area=sum(area), sum_low_fp = sum(area[flash_point < 100]))
vol.tt1 <- filter(vol.tt, terpene==1)

vol.tt.mean <- vol.tt %>% group_by(spcode, terpene) %>% 
  summarise(mean_area=mean(sum_area), sd_area=sd(sum_area), 
            mean_low_fp = mean(sum_low_fp), sd_low_fp = sd(sum_low_fp))  %>% 
            left_join(species, by="spcode")  #for graphing

## 3. Prepare data frames for stats (join with flammability and decomp)
## 3.1 Dataframe for flammability
source("read-flam.R")
flam.mono <- subset(flam, type=="monoculture") %>% group_by(spcode) %>% 
  summarise(spread_mean = mean(spread),
            spread_sd = sd(spread),
            ignit_mean = mean(ignit),
            ignit_sd = sd(ignit))
            
vol.flam <- vol.tv %>% left_join(flam.mono, by="spcode") 

vol.flam.avg <- vol.flam %>%  group_by(spcode) %>% 
  summarise(spread_mean = mean(spread_mean),
            spread_sd = mean(spread_sd),
            ignit_mean = mean(ignit_mean),
            ignit_sd = mean(ignit_sd),
            mean_area = mean(sum_area),
            area_sd = sd(sum_area),
            mean_low_fp = mean(sum_low_fp),
            low_fp_sd = sd(sum_low_fp))

vol.flamtt <- vol.tt %>% left_join(flam.mono, by="spcode")

## 3.2 Dataframes for leaf particle size and decomposition rate
source("read-decomp.R")

vol.decomp <- vol.tv %>% left_join(subset(decomp, year==0), by="spcode")

vol.decomp.sum <- vol.decomp %>% group_by(spcode) %>%
  summarize(mean_area=mean(sum_area), sd_area=sd(sum_area),
            mean_low_fp = mean(sum_low_fp), sd_low_fp = sd(sum_low_fp), 
            lt_mean = mean(lt),
            lt_sd = sd(lt),
            lvol_mean = mean(lvol),
            lvol_sd = sd(lvol))

decomp.rates <- decomp.allrates %>% group_by(tag, spcode) %>% 
  summarize(ldrate_mean = mean(ldrate),
            wdrate_mean = mean(wdrate))

vol.decomprates <- vol.tv %>% left_join(decomp.rates, by="spcode")

vol.decomprates.sum <- vol.decomprates %>% group_by(spcode) %>% 
  summarize(mean_area = mean(sum_area),
            sd_area = sd(sum_area),
            ldrate_mean = mean(ldrate_mean),
            ldrate_sd = sd(ldrate_mean),
            wdrate_mean = mean(wdrate_mean, na.rm=TRUE),
            wdrate_sd = sd(wdrate_mean, na.rm=TRUE))
