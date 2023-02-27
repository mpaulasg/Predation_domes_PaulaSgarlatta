rm(list=ls()) # cleaning memory

# libraries
library(tidyverse)
library(dplyr)
library(tidyr)
library(car)
library(glmmTMB)
library(DHARMa)
library(here)

######## Stats ########

data <- read.csv(here:: here("data/data_predation_squidpops_v2.csv"))

### Attacks - domes 

#Filter first for dome comparison

data_domes <- data %>% 
  filter (Method != "Squidpop",
          Treatment != "No_fish") %>% 
  dplyr::select(-Treatment)

# make unique identifier for each replicate

data_domes <- unite(data_domes, rep_ID, c("Site", "Replicate", "Time"), sep = "_", remove = FALSE)

attack_number <- data_domes %>%
  dplyr::group_by(Area,Site, rep_ID,Time, total_video_min)%>% # sum everything in the transect by species
  dplyr::summarise(attack=sum(attack_number,na.rm=T))

############# Trying with GLMM - Attacks domes 

#Adding offset

mod1 <- glmmTMB (attack   ~ Area + (1|Site) + (1|rep_ID) + offset (total_video_min),
                 data=attack_number, family = poisson()) 

summary(mod1)

Anova(mod1) # p=0.68 Area

attack_res_domes <- simulateResiduals(mod1)
plot(attack_res_domes) 

## Residuals look bad. Tried with nbiom1, nbinom2, Gamma, tweedie - all look bad.
#Next step, try transformation


mod1_poiss_log <- glmmTMB ((attack + 1)   ~ Area + (1|Site) + (1|rep_ID) + offset (total_video_min),
                 data=attack_number, family = poisson(link = "log")) 

summary(mod1_poiss_log)

Anova(mod1_poiss_log) # p=0.62 Area

attack_res_poiss_log <- simulateResiduals(mod1_poiss_log)
plot(attack_res_poiss_log) 

## Residuals look bad. Tried with nbiom1, nbinom2, Gamma, tweedie - all look bad.
#Next step, try using attacks per hour

attack_hour <- data_domes %>%
  dplyr::group_by(Area,Site, rep_ID,Time)%>% # sum everything in the transect by species
  dplyr::summarise(attack_h=sum(attack_hour,na.rm=T))

min_val <- attack_hour %>%
  dplyr::select(attack_h) %>%
  dplyr::filter(attack_h>0)

min_val <- min(min_val$attack_h)/2

attack_hour <- attack_hour %>% mutate(attack = attack_h + min_val) %>%
  drop_na(attack_h)# Make new variable and add half the minimum value


###################### end of code ####################