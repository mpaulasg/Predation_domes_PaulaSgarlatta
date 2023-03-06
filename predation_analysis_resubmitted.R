################################################################################
##
## Script for predation analysis
##
## Code by Paula Sgarlatta
##
##  1- Fig. 1 - Map of study sites
##
##  2- Fig. 3 - Comparison between tropics and temperate reefs using domes
##
##  3- Fig. 4 - Comparison between squidpops and domes in Malabar
##
##  4- Stats (help by Alistair Poore)
##
################################################################################

rm(list=ls()) # cleaning memory

# libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(sp)
library(rgdal)
library(raster)
library(ggpubr)
library(ggspatial)
library(ozmaps)
library(cowplot)
library(car)
library(glmmTMB)
library(DHARMa)
library(here)


#### 1 - Map of study sites ######


sites <- read.csv("data/sites_predation.csv")

color_type <- c("Dome"= "#d1495b", "Squidpop"= "#edae49")

city_data <- data.frame(city_name=c("Lizard Island", "Sydney"))
city_data$lat <- c(-14.664354450460095, -33.906516)
city_data$long <- c(145.45703254662746,151.245652)

#Plot of latitudinal

Lat_predation <- ggplot(data = ozmap(x="states")) + 
  geom_sf() +
  coord_sf(xlim = c(142,157), ylim = c(-40,-10), expand = FALSE)+
  geom_point(data=sites, aes(x=Longitude, y=Latitude, 
                                 colour = Data_type, size=2))+
  scale_color_manual(name="Data_type", values=color_type) +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "none",
        panel.border = element_rect(colour = "black",fill = NA),
        axis.text = element_text(size = (10), colour="black"), 
        axis.title = element_blank()) + scale_size(guide = "none")+
 guides(color = guide_legend(override.aes = list(size = 3) ) ) 

Lat_predation

# ggsave(Lat_predation, file="graphs/sites_lat_predation.jpeg",
#        height = 20, width = 18, unit = "cm")


#Plot of Australia/rectangle in the area of study

Aus <- ggplot(data = ozmap(x="country")) + 
  geom_sf(color="black", fill= "white") +
  geom_rect(xmin = 142, xmax = 156, ymin = -36, ymax = -14, 
            fill = NA, colour = "black", size = 0.5) +
  scale_fill_viridis_d(option = "plasma") +
  coord_sf(xlim = c(100.00, 160.00), ylim = c(-45.00, -10.00), expand = TRUE) +
  theme_void()

Aus

###Place figures together

map <- ggdraw(Lat_predation) +
  draw_plot(Aus, x = 0.5, y = 0.8, width = 0.24, height = 0.24)

map

# ggsave(map, file=here::here("graphs", "Figure1.jpeg"),
#        height = 20, width = 18, unit = "cm")

####### 2- Fig. 3 - Comparison between tropical and temperate reefs using domes #####

data <- read.csv(here:: here("data/data_predation.csv"))

#Filter first for dome comparison

data_domes <- data %>% 
  filter (Method != "Squidpop",
          Treatment != "No_fish") %>% 
  dplyr::select(-Treatment)
  

attack <- data_domes %>%
  dplyr::group_by(Area,Site, Replicate,Time, Trophic.group)%>% # sum everything in the transect by species
  dplyr::summarise(attack=sum(attack_hour,na.rm=T))%>% 
  ungroup%>% 
  tidyr::complete(Trophic.group, nesting(Area, Site, Replicate,Time),fill= list(attack=0)) %>% 
  replace(is.na(.), 0)%>% # replace the NaN resulting from dividing 0 by 0 for 0
  glimpse()

inspecting <- data_domes %>%
  dplyr::group_by(Area,Site,Replicate,Time,Trophic.group)%>% 
  dplyr::summarise(inspecting=sum(inspecting_hour,na.rm=T))%>% 
  ungroup%>% 
  tidyr::complete(Trophic.group, nesting(Area, Site,Replicate,Time),fill= list(inspecting=0)) %>% 
  replace(is.na(.), 0)%>% # replace the NaN resulting from dividing 0 by 0 for 0
  glimpse()

# #Calculate mean
# 
attack_toplot<- Rmisc::summarySE(attack, na.rm= T, measurevar=c("attack"),
                                         groupvars=c("Area", "Trophic.group"))

# Color graphs

dome_colours <- c(Sydney = "#2660A4", "Lizard Island" = "#DD614A")

mytheme <- theme(panel.background=element_rect(fill="white"), panel.grid.minor = element_blank(), axis.ticks = element_blank(),
                 panel.grid.major = element_blank(),axis.line = element_line(size = 1, colour = "black"),
                 axis.text = element_text(size = (16), color = "black"), axis.title = element_text(size= (18)),
                 legend.key = element_rect(fill = "white"), legend.text = element_text(size=20),
                 legend.title = element_blank(), plot.title = element_text(hjust = -0.05, size = 20, face = "bold"))

plot_attack_dome <- ggplot(attack, aes(x=Trophic.group, y=attack, fill=Area)) +
  geom_boxplot()+ 
  scale_y_continuous(trans="log1p")+
  geom_point(shape = 21, position = position_jitterdodge(jitter.width = 0.1), size = 2) +
  scale_fill_manual(name="Area", values=dome_colours) +
  labs(x="", y="Attack per hour") +
  mytheme + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust=1))+
  ggtitle(("a)"))

plot_attack_dome

# ggsave(plot_attack_dome, file=here::here("graphs/Figure_attack_dome.jpeg"),
# height = 16, width = 24, unit = "cm" )

plot_inspection_dome <- ggplot(inspecting,aes(x=Trophic.group, y=inspecting, fill=Area)) +
  geom_boxplot()+ 
  scale_y_continuous(trans="log1p", limits = c(0,20), breaks = seq(from=0, to=20, by=5))+
  geom_point(shape = 21, position = position_jitterdodge(jitter.width = 0.1), size = 2) +
  scale_fill_manual(name="Area", values=dome_colours) +
  labs(x="", y="Inspection per hour") +
  mytheme +theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust=1))+
  ggtitle(("b)"))

plot_inspection_dome

# ggsave(plot_inspection_dome, file=here::here("graphs/Figure_inspection_dome.jpeg"),
#        height = 16, width = 24, unit = "cm" )

#Now all together 

figure_3 <-ggarrange(plot_attack_dome, plot_inspection_dome, 
                     ncol = 2, nrow = 1)

figure_3


# ggsave(figure_3, file=here::here("graphs/Figure_3.jpeg"),
#  height = 16, width = 24, unit = "cm" )


#### Fig. 4 - Comparison between squidpops and domes in Malabar

method_colours <- c(Dome = "#91BBDE", "Squidpop" = "#214B6E")

data_squid_dome <- data %>% 
 dplyr:: filter (Area != "Lizard Island", 
          Site != "Malabar 1 ",
          Treatment != "No_fish") %>% 
 dplyr::select(-Treatment) %>% 
  mutate(Trophic.group = fct_relevel(Trophic.group, 
                            "Herbivore", "Invertivore", "Omnivore", "Piscivore",
                            "Planktivore"))

attack_squid_dome <- data_squid_dome %>%
  dplyr::group_by(Method,Site,Replicate,Time,Trophic.group)%>% # sum everything in the transect by species
  dplyr::summarise(attack=sum(attack_number,na.rm=T))%>% 
  ungroup%>% 
  tidyr::complete(Trophic.group, nesting(Method, Site,Replicate,Time),fill= list(attack=0)) %>% 
  replace(is.na(.), 0)%>% # replace the NaN resulting from dividing 0 by 0 for 0
  glimpse()

inspection_squid_dome <- data_squid_dome %>%
  dplyr::group_by(Method,Site,Replicate,Time, Trophic.group)%>% 
  dplyr::summarise(inspecting=sum(inspecting_hour,na.rm=T))%>% 
  ungroup%>% 
  tidyr::complete(Trophic.group, nesting(Method,Site,Replicate,Time),fill= list(inspecting=0)) %>% 
  replace(is.na(.), 0)%>% # replace the NaN resulting from dividing 0 by 0 for 0
  glimpse()


plot_attack_squid_dome <- ggplot(attack_squid_dome, aes(x=Trophic.group, y=attack, fill=Method)) +
  geom_boxplot()+ 
  scale_y_continuous(trans="log1p")+
  geom_point(shape = 21, position = position_jitterdodge(jitter.width = 0.1), size=2) +
  scale_fill_manual(name = "Method", values=method_colours) + 
  labs(x="", y="Attack per hour") +
  mytheme+ theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust=1))+
  ggtitle("a)")

plot_attack_squid_dome

# ggsave(plot_attack_squid_dome, file=here::here("graphs/Figure_attack_squid_dome.jpeg"),
#        height = 22, width = 25, unit = "cm" )

plot_inspect_squid_dome <- ggplot(inspection_squid_dome, aes(x=Trophic.group, y=inspecting, fill=Method)) +
  geom_boxplot()+ 
  geom_point(shape = 21, position = position_jitterdodge(jitter.width = 0.1), size=2) +
  scale_fill_manual(name = "Method", values=method_colours)+
  labs(x="", y="Inspection per hour") +
  mytheme+ 
  theme (legend.position = "none", axis.text.x = element_text(angle = 45, hjust=1))+
  ggtitle(("b)"))

plot_inspect_squid_dome

# ggsave(plot_inspect_squid_dome, file=here::here("graphs/Figure_inspect_squid_dome.jpeg"),
#        height = 22, width = 25, unit = "cm" )

#Now all together 

figure_4 <-ggarrange(plot_attack_squid_dome, plot_inspect_squid_dome, 
          ncol = 2, nrow = 1)

figure_4

# ggsave(figure_4, file=here::here("graphs/Figure_4.jpeg"),
# height = 16, width = 24, unit = "cm" )


### Fig. 4b - Comparison between squidpops and domes in Malabar - MaxN


data_squid_dome_maxN <- read.csv(here:: here("data/data_maxN.csv")) %>% 
  mutate(Trophic.group = fct_relevel(Trophic.group, 
                                     "Herbivore", "Invertivore", "Omnivore", "Piscivore",
                                     "Planktivore"))

maxN_squid_dome <- data_squid_dome_maxN %>%
  dplyr::group_by(Method,Site,Replicate,Time,Trophic.group)%>% # sum everything in the transect by species
  dplyr::summarise(maxN=sum(maxN,na.rm=T))%>% 
  ungroup%>% 
  tidyr::complete(Trophic.group, nesting(Method, Site,Replicate,Time),fill= list(maxN=0)) %>% 
  replace(is.na(.), 0)%>% # replace the NaN resulting from dividing 0 by 0 for 0
  glimpse()

plot_maxN_squid_dome <- ggplot(maxN_squid_dome, aes(x=Trophic.group, y=maxN, fill=Method)) +
  geom_boxplot()+ 
  #scale_y_continuous(trans="log1p")+
  geom_point(shape = 21, position = position_jitterdodge(jitter.width = 0.1), size=2) +
  scale_fill_manual(name = "Method", values=method_colours) + 
  labs(x="", y="maxN per hour") +
  mytheme+ theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust=1))+
  ggtitle("c)")

plot_maxN_squid_dome

# ggsave(plot_maxN_squid_dome, file=here::here("graphs/Figure_maxN_squid_dome.jpeg"),
#        height = 22, width = 25, unit = "cm" )

#Now all together 

figure_4 <-ggarrange(plot_attack_squid_dome, plot_inspect_squid_dome, plot_maxN_squid_dome,
                     ncol = 2, nrow = 2)

figure_4

# ggsave(figure_4, file=here::here("graphs/Figure_4_v2.jpeg"),
# height = 16, width = 24, unit = "cm" )


######## Stats ########

data <- read.csv(here:: here("data/data_predation.csv"))

#Filter first for dome comparison

data_domes <- data %>% 
  filter (Method != "Squidpop",
          Treatment != "No_fish") %>% 
  dplyr::select(-Treatment)

# make unique identifier for each replicate

data_domes <- unite(data_domes, rep_ID, c("Site", "Replicate", "Time"), sep = "_", remove = FALSE)


######### Option with offset ######

attack_number <- data_domes %>%
  dplyr::group_by(Area,Site, rep_ID,Time, total_video_min)%>% 
  dplyr::summarise(attack=sum(attack_number,na.rm=T))

inspecting_number <- data_domes %>%
  dplyr::group_by(Area,Site,rep_ID,Time, total_video_min)%>% 
  dplyr::summarise(inspecting=sum(inspecting_number,na.rm=T))


mod1 <- glmmTMB (attack   ~ Area + (1|Site) + (1|rep_ID) + offset (log(total_video_min)), 
                 data=attack_number, family = poisson()) 

summary(mod1)

Anova(mod1) # p=0.28

attack_res_domes <- simulateResiduals(mod1)
plot(attack_res_domes) #Good


mod2 <- glmmTMB (inspecting ~ Area + (1|Site) + (1|rep_ID) + offset (log(total_video_min)), 
                 data=inspecting_number, family = nbinom2 ()) 

summary(mod2)

Anova(mod2) #p = 0.19

attack_res_domes_mod2 <- simulateResiduals(mod2)
plot(attack_res_domes_mod2) # Good

####Let's try just one trophic group per time

attack_total <- data_domes %>%
    dplyr::group_by(Area,Site, rep_ID,Time, total_video_min, Trophic.group)%>%
    dplyr::summarise(attack=sum(attack_number,na.rm=T))%>%
    ungroup%>%
    tidyr::complete(Trophic.group, nesting(Area, Site, rep_ID,Time, total_video_min),fill= list(attack=0)) %>%
    replace(is.na(.), 0)%>% # replace the NaN resulting from dividing 0 by 0 for 0
    glimpse()


piscivores <- attack_total %>% 
  filter(Trophic.group == "Piscivore")


pisc <- glmmTMB (attack ~ Area + (1|Site)+ (1|rep_ID) + offset (log(total_video_min)), 
                 data=piscivores,
                 family = poisson()) 


Anova(pisc) #p=0.03

pis_res_domes <- simulateResiduals(pisc)
plot(pis_res_domes) # Good

## Investigating per trophic group

inspect_total <- data_domes %>%
  dplyr::group_by(Area,Site, rep_ID,Time, total_video_min, Trophic.group)%>%
  dplyr::summarise(inspect=sum(inspecting_number,na.rm=T))%>%
  ungroup%>%
  tidyr::complete(Trophic.group, nesting(Area, Site, rep_ID,Time, total_video_min),fill= list(inspect=0)) %>%
  replace(is.na(.), 0)%>% # replace the NaN resulting from dividing 0 by 0 for 0
  glimpse()

piscivores_inv <- inspect_total %>% 
  filter(Trophic.group == "Piscivore")


pisc_inv <- glmmTMB(inspect  ~ Area + (1|Site) + (1|rep_ID) + offset (log(total_video_min)), 
                    data=piscivores_inv, 
                    family = poisson())

summary(pisc_inv)
Anova(pisc_inv) # p= 0.0006

pisc_inv_res_domes <- simulateResiduals(pisc_inv)
plot(pisc_inv_res_domes) #Good


planktivore_inv <- inspect_total %>% 
  filter(Trophic.group == "Planktivore")

plank_inv <- glmmTMB(inspect ~ Area + (1|Site) + (1|rep_ID) + offset (log(total_video_min)), data=planktivore_inv, 
                     family = nbinom1())

Anova(plank_inv) # p=0.8923

plank_inv_res_domes <- simulateResiduals(plank_inv)
plot(plank_inv_res_domes) # Good!


############## Comparison domes/squidpops  #############

data_squid <- data %>% 
  dplyr:: filter (Area != "Lizard Island", 
                  Site != "Malabar 1 ",
                  Treatment != "No_fish") %>% 
  dplyr::select(-Treatment)

data_squid <- unite(data_squid, rep_ID, c("Site", "Replicate", "Time"), sep = "_", remove = FALSE)

######### Option with offset 

attack_number_squid <- data_squid %>%
  dplyr::group_by(Method, Area,Site, rep_ID,Time, total_video_min)%>% 
  dplyr::summarise(attack=sum(attack_number,na.rm=T))

inspecting_number_squid <- data_squid %>%
  dplyr::group_by(Method, Area,Site,rep_ID,Time, total_video_min)%>% 
  dplyr::summarise(inspecting=sum(inspecting_number,na.rm=T))


attack_squid <- glmmTMB (attack   ~ Method + (1|Site) + (1|rep_ID) + offset (total_video_min), data=attack_number_squid, 
                           family = nbinom1()) 

summary(attack_squid)
Anova(attack_squid) # method - p<0.00002

attack_res_squid <- simulateResiduals(attack_squid)
plot(attack_res_squid) # Good

############## Inspecting

inspect_squid <- glmmTMB (inspecting    ~ Method + (1|Site) + (1|rep_ID) + offset (log(total_video_min)),
                          data= inspecting_number_squid, 
                          family = poisson()) 

Anova(inspect_squid) # p=0.53

inspec_res_squid <- simulateResiduals(inspect_squid)
plot(inspec_res_squid) # Good 

#######Let's try just one trophic group per time. 


attack_total_squid <- data_squid %>%
  dplyr::group_by(Method,Site, rep_ID,Time, Trophic.group, total_video_min)%>% # sum everything in the replicate by species
  dplyr::summarise(attack=sum(attack_number,na.rm=T)) %>% 
  ungroup%>%
  tidyr::complete(Trophic.group, nesting(Method, Site,rep_ID,Time, total_video_min), fill= list(attack=0)) %>%
  replace(is.na(.), 0)%>% # replace the NaN resulting from dividing 0 by 0 for 0
  glimpse()
  
  
  inspecting_total_squid <- data_squid %>%
  dplyr::group_by(Method, Site, rep_ID,Time, Trophic.group, total_video_min)%>% # sum everything in the replicate by species
  dplyr::summarise(inspecting=sum(inspecting_number,na.rm=T)) %>% 
  ungroup%>%
  tidyr::complete(Trophic.group, nesting(Method, Site,rep_ID,Time, total_video_min), fill= list(inspecting=0)) %>%
  replace(is.na(.), 0)%>% # replace the NaN resulting from dividing 0 by 0 for 0
  glimpse()
  

Omnivore_squid <- attack_total_squid %>% 
  filter(Trophic.group == "Omnivore")

omn_squid <- glmmTMB(attack   ~ Method + (1|Site) + (1|rep_ID) + offset (log(total_video_min)),  
                     data = Omnivore_squid, 
                     family = poisson())

Anova(omn_squid) #p=0.009

omn_attack_res_squid <- simulateResiduals(omn_squid)
plot(omn_attack_res_squid) # Good

## Inspecting

planktivore_inv <- inspecting_total_squid %>% 
  filter(Trophic.group == "Planktivore")

plank_inv <- glmmTMB(inspecting  ~ Method + (1|Site) + offset (total_video_min),
                     data=planktivore_inv,
                     family = nbinom2())

Anova(plank_inv) # p<0.0001

plank_inv_res_squid <- simulateResiduals(plank_inv)
plot(plank_inv_res_squid) # Good! 


#################### maxN  ########

data_squid_dome_maxN <- read.csv(here:: here("data/data_maxN.csv")) %>% 
  mutate(Trophic.group = fct_relevel(Trophic.group, 
                                     "Herbivore", "Invertivore", "Omnivore", "Piscivore",
                                     "Planktivore"))

data_squid_dome_maxN <- unite(data_squid_dome_maxN, rep_ID, c("Site", "Replicate", "Time"), sep = "_", remove = FALSE)

maxN_total_stat <- data_squid_dome_maxN  %>%
  dplyr::group_by(Method, Site, rep_ID, Time, total_video_min)%>% 
  dplyr::summarise(maxN=sum(maxN,na.rm=T))



maxN_total <- glmmTMB(maxN  ~ Method + (1|Site) + (1|rep_ID) + offset (total_video_min),
                     data=maxN_total_stat,
                     family = nbinom2(link = "log"))

Anova(maxN_total) # p<0.0001

maxN_total_res <- simulateResiduals(maxN_total)
plot(maxN_total_res) # Not good at all...

#Let's try by trophic group

maxN_squid_dome <- data_squid_dome_maxN %>%
  dplyr::group_by(Method,Site,Replicate,Time,Trophic.group, rep_ID, total_video_min)%>% # sum everything in the transect by species
  dplyr::summarise(maxN=sum(maxN,na.rm=T))%>% 
  ungroup%>% 
  tidyr::complete(Trophic.group, nesting(Method, Site,Replicate,Time, rep_ID, total_video_min),fill= list(maxN=0)) %>% 
  replace(is.na(.), 0)%>% # replace the NaN resulting from dividing 0 by 0 for 0
  glimpse()

planktivore <- maxN_squid_dome %>% 
  filter(Trophic.group == "Planktivore")

plank_maxN <- glmmTMB(maxN  ~ Method + (1|Site) + (1|rep_ID) + offset (total_video_min),
                     data=planktivore, 
                     family = nbinom2())

Anova(plank_maxN) 

plank_maxN_res <- simulateResiduals(plank_maxN)
plot(plank_maxN_res) # Residuals really bad

omnivore <- maxN_squid_dome %>% 
  filter(Trophic.group == "Omnivore")

omn_maxN <- glmmTMB(maxN  ~ Method + (1|Site) + (1|rep_ID) + offset (total_video_min),
                      data=omnivore,
                      family = nbinom2(link = "log"))

Anova(omn_maxN) 

omn_maxN_res <- simulateResiduals(omn_maxN)
plot(omn_maxN_res) # Not working


######## Option with attack-inspection/hour #######

data <- read.csv(here:: here("data/data_predation.csv"))

#Filter first for dome comparison

data_domes <- data %>% 
  filter (Method != "Squidpop",
          Treatment != "No_fish") %>% 
  dplyr::select(-Treatment)

# make unique identifier for each replicate

data_domes <- unite(data_domes, rep_ID, c("Site", "Replicate", "Time"), sep = "_", remove = FALSE)

attack_hour <- data_domes %>%
  dplyr::group_by(Area,Site, rep_ID,Time)%>% 
  dplyr::summarise(attack=sum(attack_hour,na.rm=T))

inspecting_hour <- data_domes %>%
  dplyr::group_by(Area,Site,rep_ID,Time)%>% 
  dplyr::summarise(inspecting=sum(inspecting_hour,na.rm=T))
  
    min_val_attack <- attack_hour %>%
    dplyr::select(attack) %>%
    dplyr::filter(attack>0)
  
    min_val_attack <- min(min_val_attack$attack)/2
  
  attack_hour <- attack_hour %>% mutate(attack_min = attack + min_val_attack) %>%
    drop_na(attack)# Make new variable and add half the minimum value
  

  
  mod1_hour <- glmmTMB (attack_min ~ Area + (1|Site) + (1|rep_ID), 
                   data=attack_hour, family = Gamma(link = "log")) 
  
  summary(mod1_hour)
  
  Anova(mod1_hour) #0.32

attack_res_domes_hour <- simulateResiduals(mod1_hour)
plot(attack_res_domes_hour) #Good


min_val_ins <- inspecting_hour %>%
  dplyr::select(inspecting) %>%
  dplyr::filter(inspecting>0)

min_val_ins <- min(min_val_ins$inspecting)/2

inspecting_hour <- inspecting_hour %>% mutate (inspecting_min = inspecting + min_val_ins) %>%
  drop_na(inspecting)# Make new variable and add half the minimum value



mod2_hour <- glmmTMB (inspecting_min ~ Area + (1|Site) + (1|rep_ID), 
                 data=inspecting_hour, family = Gamma(link = "log")) 

summary(mod2_hour)

Anova(mod2_hour) #p=0.05


ins_res_domes_hour <- simulateResiduals(mod2_hour)
plot(ins_res_domes_hour) #Good!


####Let's try just one trophic group per time

attack_hour_total <- data_domes %>%
  dplyr::group_by(Area,Site, rep_ID,Time, Trophic.group)%>%
  dplyr::summarise(attack=sum(attack_hour,na.rm=T))%>%
  ungroup%>%
  tidyr::complete(Trophic.group, nesting(Area, Site, rep_ID,Time),fill= list(attack=0)) %>%
  replace(is.na(.), 0)%>% # replace the NaN resulting from dividing 0 by 0 for 0
  glimpse()

min_val_attack_tg <- attack_hour_total %>%
  dplyr::select(attack) %>%
  dplyr::filter(attack>0)

min_val_attack_tg <- min(min_val_attack_tg$attack)/2

attack_hour_total <- attack_hour_total %>% mutate(attack_min = attack + min_val_attack_tg) %>%
  drop_na(attack)# Make new variable and add half the minimum value


piscivores_hour <- attack_hour_total %>% 
  filter(Trophic.group == "Piscivore")

pisc_hour <- glmmTMB (attack_min ~ Area + (1|Site)+ (1|rep_ID), 
                 data=piscivores_hour, 
                 family = Gamma(link = "log")) 


Anova(pisc_hour) #p=0.02

pisc_hour_res <- simulateResiduals(pisc_hour)
plot(pisc_hour_res) # I think is good??

## Investigating per trophic group

inspect_hour_total <- data_domes %>%
  dplyr::group_by(Area,Site, rep_ID,Time, Trophic.group)%>%
  dplyr::summarise(inspect=sum(inspecting_hour,na.rm=T))%>%
  ungroup%>%
  tidyr::complete(Trophic.group, nesting(Area, Site, rep_ID,Time),fill= list(attack=0)) %>%
  replace(is.na(.), 0)%>% # replace the NaN resulting from dividing 0 by 0 for 0
  glimpse()

min_val_inspect_tg <- inspect_hour_total %>%
  dplyr::select(inspect) %>%
  dplyr::filter(inspect>0)

min_val_inspect_tg <- min(min_val_inspect_tg$inspect)/2

inspect_hour_total <- inspect_hour_total %>% mutate(inspect_min = inspect + min_val_inspect_tg) %>%
  drop_na(inspect)# Make new variable and add half the minimum value

pisc_hour_inspec <- inspect_hour_total %>% 
  filter(Trophic.group == "Piscivore")


pisc_inv_h <- glmmTMB(inspect_min  ~ Area + (1|Site) + (1|rep_ID), 
                    data=pisc_hour_inspec, 
                    family = Gamma(link = "log"))

summary(pisc_inv_h)
Anova(pisc_inv_h) # p<0.0001

pisc_inv_h_res <- simulateResiduals(pisc_inv_h)
plot(pisc_inv_h_res) #Good


planktivore_inv_h <- inspect_hour_total %>% 
  filter(Trophic.group == "Planktivore")

plank_inv_hour <- glmmTMB(inspect_min ~ Area + (1|Site) + (1|rep_ID),
                          data=planktivore_inv_h, 
                     family = Gamma(link = "log"))

Anova(plank_inv_hour) # p=0.653

plank_inv_hour_res <- simulateResiduals(plank_inv_hour)
plot(plank_inv_hour_res) #Not working

####### maxN per hour #####

data_squid_dome_maxN <- read.csv(here:: here("data/data_maxN.csv")) %>% 
  mutate(Trophic.group = fct_relevel(Trophic.group, 
                                     "Herbivore", "Invertivore", "Omnivore", "Piscivore",
                                     "Planktivore"))

data_squid_dome_maxN <- unite(data_squid_dome_maxN, rep_ID, c("Site", "Replicate", "Time"), sep = "_", remove = FALSE)

maxN_hour <- data_squid_dome_maxN  %>%
  dplyr::group_by(Method, Site, rep_ID, Time)%>% 
  dplyr::summarise(maxN=sum(maxN_hour,na.rm=T))

maxN_total <- glmmTMB(maxN  ~ Method + (1|Site) + (1|rep_ID),
                      data=maxN_hour,
                      family = Gamma(link = "log"))

Anova(maxN_total) 

maxN_total_res <- simulateResiduals(maxN_total)
plot(maxN_total_res) # Good but I don't think it's actually good

#Let's try by trophic group

maxN_squid_dome <- data_squid_dome_maxN %>%
  dplyr::group_by(Method,Site,Replicate,Time,Trophic.group, rep_ID)%>% # sum everything in the transect by species
  dplyr::summarise(maxN=sum(maxN_hour,na.rm=T))%>% 
  ungroup%>% 
  tidyr::complete(Trophic.group, nesting(Method, Site,Replicate,Time, rep_ID),fill= list(maxN=0)) %>% 
  replace(is.na(.), 0)%>% # replace the NaN resulting from dividing 0 by 0 for 0
  glimpse()

min_val_maxN <- maxN_squid_dome %>%
  dplyr::select(maxN) %>%
  dplyr::filter(maxN>0)

min_val_maxN <- min(min_val_maxN$maxN)/2

maxN_squid_dome <- maxN_squid_dome %>% mutate(maxN_min = maxN + min_val_maxN) %>%
  drop_na(maxN)# Make new variable and add half the minimum value

planktivore <- maxN_squid_dome %>% 
  filter(Trophic.group == "Planktivore")

plank_maxN <- glmmTMB(maxN_min  ~ Method + (1|Site) + (1|rep_ID),
                      data=planktivore, 
                      family = Gamma(link = "log"))

Anova(plank_maxN) 

plank_maxN_res <- simulateResiduals(plank_maxN)
plot(plank_maxN_res) # Not sure this is correct




omnivore <- maxN_squid_dome %>% 
  filter(Trophic.group == "Omnivore")

omn_maxN <- glmmTMB(maxN_min  ~ Method + (1|Site) + (1|rep_ID),
                    data=omnivore,
                    family = Gamma())

omn_maxN <- glmmTMB(maxN_min  ~ Method + Site ,
                    data=omnivore,
                    family = Gamma())

omn_maxN <- aov(maxN_min  ~ Method + Site ,
                    data=omnivore)


summary(omn_maxN) 

plot(omn_maxN, 1)
leveneTest(maxN_min  ~ Method*Site ,
            data=omnivore)

omn_maxN_res <- simulateResiduals(omn_maxN)
plot(omn_maxN_res) # Not sure this is correct
  
  


  
################################### end of code #####################################################################
  
  
  
  
  ########################### EXTRAS ########  
  
  attack <- attack %>% 
  filter(Trophic.group != "Planktivore",
         Trophic.group != "Invertivore")

attack_2 <- data_domes %>%
  dplyr::group_by(Area,Site, Replicate,Time)%>% # sum everything in the transect by species
  dplyr::summarise(attack=sum(attack_hour,na.rm=T))%>% 
  
#Try with binomial data and log data

attack <- attack %>% 
  mutate(binary=ifelse(attack >0, 1, 0)) %>% 
  mutate(log_attack = log10(attack + 1))

############# Trying with GLMM - Attacks domes 

## Interactions are giving problems so let's try without them 

attack_domes <- glmmTMB (attack  ~ Area:Trophic.group + (1|Site)+ (1|Replicate), data=attack, 
                         family = poisson(link="log")) 


Anova(attack_domes) # The interaction is working p=0.0002286

attack_res_domes <- simulateResiduals(attack_domes)
plot(attack_res_domes) 
testDispersion(attack_res_domes) #Good enough

insepect_domes <- glmmTMB (inspecting ~ Area:Trophic.group + (1|Site) + (1|Replicate), data=inspecting, 
                           family = poisson(link = "log")) 

Anova(insepect_domes) #Significant - p=0.00010

inspec_res_domes <- simulateResiduals(insepect_domes)
plot(inspec_res_domes) # Good! 


attack_squid <- attack_squid %>% 
  filter(Trophic.group != "Invertivore")

attack_squid$Replicate <- factor(attack_squid$Replicate)

attack_squid_glmm <- glmmTMB (attack  ~ Method:Trophic.group + (1|Site) + (1|Replicate),  data=attack_squid, 
                              family = poisson(link = "log")) 

Anova(attack_squid_glmm)

attack_squid_glmm_res_domes <- simulateResiduals(attack_squid_glmm)
plot(attack_squid_glmm_res_domes) # Good!


inspecting_squid$Replicate <- factor(inspecting_squid$Replicate)

inspecting_squid$Method <- factor(inspecting_squid$Method)

inspect_squid_glmm <- glmmTMB (log(inspecting + 1) ~ Method:Trophic.group + (1|Site) + (1|Replicate), data=inspecting_squid, 
                               family = poisson(link="log")) 


Anova(inspect_squid_glmm) #Significant - p<0.001

inspec_res_squid <- simulateResiduals(inspect_squid_glmm)
plot(inspec_res_squid) # Residuals not good, let's take a closer look

par(mfrow=c(1,2))

plotResiduals(inspec_res_squid, inspecting_squid$Trophic.group)
plotResiduals(inspec_res_squid, inspecting_squid$Method) #Perfect!

#### PERFECTTTTT THIS IS THE FINAL ONE


Herbivores <- read.csv("data/Herbivore_specialisation.csv", header = TRUE)


Herb_community <- Herbivores[5:11]
library(vegan)

Herb_community.mds <- metaMDS(comm = Herb_community, 
                              distance = "bray", trace = FALSE, autotransform = FALSE)


plot(Herb_community.mds$points)

MDS_xy <- data.frame(Herb_community.mds$points)
MDS_xy$Habitat <- Herbivores$Habitat
MDS_xy$DayNight <- Herbivores$DayNight
library(ggplot2)
ggplot(MDS_xy, aes(MDS1, MDS2, color = Habitat)) +
  geom_point() +
  theme_bw()

ggplot(MDS_xy, aes(MDS1, MDS2, color = DayNight)) +
  geom_point() +
  theme_bw()

# # Pivot_wider the data to do nMDS

attack_dome_MDS <- attack %>% 
  group_by (Trophic.group) %>% 
  dplyr::select(-binary, -log_attack) %>% 
  mutate(row=row_number()) %>% 
  pivot_wider(names_from = Trophic.group, values_from = attack) %>% 
  dplyr::select(-row) %>% 
  replace(is.na(.),0) 

attack_dome_MDS <- attack_dome_MDS[-c(1,5,6,10,12, 13, 14, 15, 16, 17,22),]

attack_dome_comm <- attack_dome_MDS[5:7]

attack_dome_comm.mds <- metaMDS(comm = attack_dome_comm, 
                              distance = "bray", trace = FALSE, autotransform = FALSE)


plot(attack_dome_comm.mds$points)

MDS_xy <- data.frame(attack_dome_comm.mds$points)
MDS_xy$Area <- attack_dome_MDS$Area
MDS_xy$DayNight <- Herbivores$DayNight
library(ggplot2)
ggplot(MDS_xy, aes(MDS1, MDS2, color = Area)) +
  geom_point() +
  theme_bw()

ggplot(MDS_xy, aes(MDS1, MDS2, color = DayNight)) +
  geom_point() +
  theme_bw()












### More trials 

### Attacks - domes 

attack <- attack %>% 
  filter(Trophic.group != "Planktivore",
         Trophic.group != "Invertivore")

#Try with binomial data and log data

attack <- attack %>% 
  mutate(binary=ifelse(attack >0, 1, 0)) %>% 
  mutate(log_attack = log10(attack + 1))

############# Trying with GLMM - Attacks domes 

## Interactions are giving problems so let's try without them 

attack_domes <- glmmTMB (log(attack +1) ~ Area + (1|Site) + (1|Replicate), data=attack, 
                         family = poisson()) 

summary(attack_domes)
Anova(attack_domes) # The interaction is not working 

attack_res_domes <- simulateResiduals(attack_domes)
plot(attack_res_domes) 
testDispersion(attack_res_domes) #Good enough
# 
#awesome lets do post hoc tests

pairs(emmeans(attack_domes,spec=~Area|Trophic.group, type="response")) # Piscivores = 0.0053


## Try with binomial

attack_domes <- glmmTMB (binary ~ Area + (1|Site) + (1|Replicate), data=attack, 
                         family = binomial()) 

summary(attack_domes)
Anova(attack_domes) 
attack_res_domes <- simulateResiduals(attack_domes)
plot(attack_res_domes) #Works better, but doesn't make a lot of sense without the interaction
testDispersion(attack_res_domes) 

emmeans(attack_domes, pairwise ~Area|Trophic.group)


#Let's try just one trophic group per time. 

piscivores <- attack %>% 
  filter(Trophic.group == "Piscivore")

pisc <- glmmTMB (attack ~ Area + (1|Site) + (1|Replicate), data=piscivores, 
                 family = poisson(link = "log")) 


summary(pisc)
Anova(pisc) #p=0.0003991

#This one  works!

pis_res_domes <- simulateResiduals(pisc)
plot(pis_res_domes) # Good!


pisc <- glmmTMB (binary ~ Area + (1|Site) + (1|Replicate), data=piscivores, 
                 family = binomial()) 


summary(pisc)
Anova(pisc) #p=0.0003991

#This one  works! But p=0.08

pis_res_domes <- simulateResiduals(pisc)
plot(pis_res_domes) # Good!



Omnivore <- attack %>% 
  filter(Trophic.group == "Omnivore")

omn <- glmmTMB(attack  ~ Area + (1|Site) + (1|Replicate),  data = Omnivore,
               family = ziGamma(link = "log"), ziformula =~1)

Anova(omn) #Cannot run the model, the model doesn't fit.  

omn <- glmmTMB(binary  ~ Area + (1|Site) + (1|Replicate), data = Omnivore,
               family = binomial())

Anova(omn) #Cannot run the model, the model doesn't fit.  


Herbivore <- attack %>% 
  filter(Trophic.group == "Herbivore")

herb <- glmmTMB(attack ~ Area + (1|Site) + (1|Replicate), data=Herbivore,
                family= poisson(link = "log"))

Anova(herb)

herb <- glmmTMB(binary ~ Area + (1|Site) + (1|Replicate), data=Herbivore,
                family= binomial())


Anova(herb)

## No differences with binary data.

####### Let's try LMER

attack_domes <- lmer(log_attack  ~ Area + (1|Site) + (1|Replicate), data = attack)

Anova(attack_domes,type="II")

qqnorm(residuals(attack_domes))
scatter.smooth(residuals(attack_domes) ~ fitted(attack_domes))

plot(attack_domes) # Good
attack_domes_res <- simulateResiduals(attack_domes)
plot(attack_domes_res) # Not good

#Let's try just one trophic group per time. 

piscivores <- attack %>% 
  filter(Trophic.group == "Piscivore")

pisc <- lmer (log_attack ~ Area + (1|Site/Replicate), data=piscivores) 

Anova(pisc)

plot(pisc) #This one looks good
pisc_res <- simulateResiduals(pisc)
plot(pisc_res) # Not ok
bartlett.test(attack ~ Area, data=piscivores)  


Omnivore <- attack %>% 
  filter(Trophic.group == "Omnivore")

omn <- lmer(log(attack + 1)~ Area + (1|Site/Replicate), data=Omnivore)

omn <- aov(log(attack + 1)~ Area + Site, data=Omnivore)

Anova(omn)

plot(omn) #This one looks good
qqnorm(residuals(omn))
omn_res <- simulateResiduals(omn)
plot(omn_res) # Not ok
bartlett.test(log(attack + 1)~ Area, data=Omnivore)  # Not ok at all

Herbivore <- attack %>% 
  filter(Trophic.group == "Herbivore")

herb <- lmer(binary~ Area + (1|Site/Replicate), data=Herbivore)

Anova(herb)

plot(herb) 
herb_res <- simulateResiduals(herb)
plot(herb_res) # Not ok
bartlett.test(log(attack + 1)~ Area, data=Herbivore)  # Not ok at all


############ investigating - domes ####

insepect_domes <- glmmTMB (inspecting ~ Area*Trophic.group + (1|Site) + (1|Replicate), data=inspecting, 
                           family = poisson(link = "log")) 

summary(insepect_domes)
Anova(insepect_domes) #Significant - p=0.00014

inspec_res_domes <- simulateResiduals(insepect_domes)
plot(inspec_res_domes) # Good!  

#lets do post hoc tests

pairs(emmeans(insepect_domes,spec=~Area|Trophic.group, type="response")) # Piscivores - p:0.02

#Let's try just one trophic group per time. 

piscivores_inv <- inspecting %>% 
  filter(Trophic.group == "Piscivore")

pisc_inv <- glmmTMB(inspecting ~ Area + (1|Site) + (1|Replicate), data=piscivores_inv,
                    family = poisson(link = "log"))

summary(pisc_inv)
Anova(pisc_inv) #Significant - p=0.00014

pisc_inv_res_domes <- simulateResiduals(pisc_inv)
plot(pisc_inv_res_domes) # Good! 


Omnivore_inv <- inspecting %>% 
  filter(Trophic.group == "Omnivore")

omn_inv <- glmmTMB(inspecting ~ Area + (1|Site) + (1|Replicate), data=Omnivore_inv,
                   family = poisson(link = "log"))
summary(omn_inv)
Anova(omn_inv)

omn_inv_res_domes <- simulateResiduals(omn_inv)
plot(omn_inv_res_domes) # Good!

Herbivore_inv <- inspecting %>% 
  filter(Trophic.group == "Herbivore")

herb_inv <- glmmTMB(inspecting ~ Area + (1|Site) + (1|Replicate), data=Herbivore_inv,
                    family = poisson(link = "log"))
summary(herb_inv)
Anova(herb_inv)

herb_inv_res_domes <- simulateResiduals(herb_inv)
plot(herb_inv_res_domes) # Good!


planktivore_inv <- inspecting %>% 
  filter(Trophic.group == "Planktivore")

plank_inv <- glmmTMB(inspecting ~ Area + (1|Site) + (1|Replicate), data=planktivore_inv,
family = poisson(link = "log"))

summary(plank_inv)
Anova(plank_inv)

plank_inv_res_domes <- simulateResiduals(plank_inv)
plot(plank_inv_res_domes) # Good!



##################Attack - squidpop vs dome ####


attack_squid <- attack_squid %>% 
  filter(Trophic.group != "Invertivore")

attack_squid$Replicate <- factor(attack_squid$Replicate)

attack_squid_glmm <- glmmTMB (attack ~ Method*Trophic.group + (1|Site) + (1|Replicate),  data=attack_squid, 
                              family = poisson(link = "log")) 

summary(attack_squid_glmm) # Doesn't work!
Anova(attack_squid_glmm)

attack_squid_glmm_res_domes <- simulateResiduals(attack_squid_glmm)
plot(attack_squid_glmm_res_domes) # Good!

#lets do post hoc tests

pairs(emmeans(attack_squid_glmm,spec=~Method|Trophic.group, type="response")) # Piscivores - p:0.02

## (try per group but shouldn't add more info)

### investigating - domes/squidpops

inspect_squid_glmm <- glmmTMB (inspecting ~ Method*Trophic.group  + (1|Site) + (1|Replicate), data=inspecting_squid, 
                               family = poisson(link = "log")) 


summary(inspect_squid_glmm)
Anova(inspect_squid_glmm) #Significant - p=0.002

inspec_res_squid <- simulateResiduals(inspect_squid_glmm)
plot(inspec_res_squid) # Good!


### Try per trophic group separated in individual models and
## that's it 


######################################################################




### investigating - domes ####

## There are problems with GLMM

insepect_domes <- glmmTMB (inspecting ~ Area*Trophic.group + (1|Site), data=inspecting, 
                           family = poisson(link = "log")) 

summary(insepect_domes)
Anova(insepect_domes) #Significant - p=0.00014

inspec_res_domes <- simulateResiduals(insepect_domes)
plot(inspec_res_domes) # Good!  

#lets do post hoc tests

pairs(emmeans(insepect_domes,spec=~Area|Trophic.group, type="response")) # Piscivores - p:0.02

## Let's try with ANOVA

#Let's try just one trophic group per time. 

piscivores_inv <- inspecting %>% 
  filter(Trophic.group == "Piscivore")

pisc_inv <- aov (log(inspecting + 1) ~ Area + Site, data=piscivores_inv)

summary(pisc_inv)

Omnivore_inv <- inspecting %>% 
  filter(Trophic.group == "Omnivore")

omn_inv <- aov(log(inspecting + 1)~ Area + Site, data=Omnivore_inv)
summary(omn_inv)

Herbivore_inv <- inspecting %>% 
  filter(Trophic.group == "Herbivore")

herb_inv <- aov(log(inspecting + 1)~ Area + Site, data=Herbivore_inv)
summary(herb_inv)

planktivore_inv <- inspecting %>% 
  filter(Trophic.group == "Planktivore")

plank_inv <- aov(log(inspecting + 1)~ Area + Site, data=planktivore_inv)
summary(plank_inv)



### Attacks - domes/squidpops ####

attack_squid <- attack_squid %>% 
  filter(Trophic.group != "Invertivore")

attack_squid_glmm <- glmmTMB (attack ~ Method*Trophic.group + (1|Site),  data=attack_squid, 
                              family = poisson(link = "log")) 

## Let's try ANOVA 


summary(attack_squid_glmm) # Doesn't work!

attack_squid_aov <- aov(log(attack +1) ~ Method + Site, data = attack_squid)

summary(attack_squid_aov)

attack_squid_plank <- attack_squid %>% 
  filter(Trophic.group == "Planktivore")

attack_squid_plank_aov <- aov(log(attack +1) ~ Method , data = attack_squid_plank)

summary(attack_squid_plank_aov)#p=0.002

par(mfrow = c(1, 2)) # This code put two plots in the same window
hist(attack_squid_plank_aov$residuals)
plot(attack_squid_plank_aov, which = 2)

plot(attack_squid_plank_aov, which = 1) # I think this is not great


attack_squid_omn <- attack_squid %>% 
  filter(Trophic.group == "Omnivore")

attack_squid_omn_aov <- aov(log(attack +1) ~ Method , data = attack_squid_omn)

summary(attack_squid_omn_aov)

attack_squid_pis <- attack_squid %>% 
  filter(Trophic.group == "Piscivore")

attack_squid_pis_aov <- aov(log(attack +1) ~ Method , data = attack_squid_pis)

summary(attack_squid_pis_aov)


### investigating - domes/squidpops

inspect_squid_glmm <- glmmTMB (inspecting ~ Method + Trophic.group , ziformula = ~1, data=inspecting_squid, 
                               family = poisson(link = "log")) 


summary(inspect_squid_glmm)
Anova(inspect_squid_glmm) #Significant - p=0.002

inspec_res_squid <- simulateResiduals(inspect_squid_glmm)
plot(inspec_res_squid) # Residuals not good

## This one works but the residuals are not good

## Let's try ANOVA 

inspecting_squid_plank <- inspecting_squid %>% 
  filter(Trophic.group == "Planktivore")

inspecting_squid_plank_aov <- aov(log(inspecting +1) ~ Method , data = inspecting_squid_plank)

summary(inspecting_squid_plank_aov)

par(mfrow = c(1, 2)) # This code put two plots in the same window
hist(inspecting_squid_plank_aov$residuals)
plot(inspecting_squid_plank_aov, which = 2)

plot(inspecting_squid_plank_aov, which = 1) # I think this is not great


inspecting_squid_omn <- inspecting_squid %>% 
  filter(Trophic.group == "Omnivore")

inspecting_squid_omn_aov <- aov(log(inspecting +1) ~ Method , data = inspecting_squid_omn)

summary(inspecting_squid_omn_aov)

inspecting_squid_pis <- inspecting_squid %>% 
  filter(Trophic.group == "Piscivore")

inspecting_squid_pis_aov <- aov(log(inspecting +1) ~ Method , data = inspecting_squid_pis)

summary(inspecting_squid_pis_aov)


##### Let's try lmer #####

### Attack - domes

attack <- attack %>% 
  filter(Trophic.group != "Planktivore",
         Trophic.group != "Invertivore")


attack_domes <- lmer(log(attack + 1) ~ Area*Trophic.group + (1|Site), data = attack)

Anova(attack_domes, type = "II")

plot(attack_domes) # Still fan-shape?

emmeans(attack_domes, list(pairwise ~Area*Trophic.group), adjust = "tukey")

pairs(emmeans(attack_domes=~Area|Trophic.group, type="response")) # Not working
pairs(emmeans(attack_domes=~Trophic.group:Area, type="response"))

### investigating - domes 

inv_domes <- lmer(log(inspecting + 1) ~ Area*Trophic.group + (1|Site), data = inspecting)

Anova(inv_domes, type = "II")

plot(inv_domes) # Good!


emmeans(inv_domes, list(pairwise ~Area*Trophic.group), adjust = "tukey")
#I'm surprise there are not differences between i.e., piscivores between Lizard and Sydney

pairs(emmeans(inv_domes=~Area|Trophic.group, type="response")) # Not working
pairs(emmeans(attack_domes=~Trophic.group:Area, type="response"))

### Attacks - domes/squidpops ####

attack_squid <- attack_squid %>% 
  filter(Trophic.group != "Invertivore")

attack_squid_lmer <- lmer(log(attack + 1) ~ Method*Trophic.group + (1|Site), data = attack_squid)

Anova(attack_squid_lmer, type = "II")

plot(attack_squid_lmer) # Good!

emmeans(attack_squid_lmer, list(pairwise ~Method*Trophic.group), adjust = "tukey")

pairs(emmeans(attack_domes=~Area|Trophic.group, type="response")) # Not working
pairs(emmeans(attack_domes=~Trophic.group:Area, type="response"))

### investigating - domes/squidpops

ins_squid_lmer <- lmer(log(inspecting + 1) ~ Method*Trophic.group + (1|Site), data = inspecting_squid)

Anova(ins_squid_lmer, type = "II")

plot(ins_squid_lmer) # Good!

emmeans(ins_squid_lmer, list(pairwise ~Method*Trophic.group), adjust = "tukey")



#### Trials ####

## Let's try a nested ANOVA - first model just with the total community to see if the attacks are different. 

attack_domes <- aov(attack ~ Area/Site, data = attack)

summary(attack_domes) # Not significant 

#Because p>0.25, I don't need a nested ANOVA (is this correct?)

attack_domes <- aov(attack ~ Area + Site, data = attack)

summary(attack_domes)# Same results!

# Question here: should I delete Site?

attack_domes <- aov(attack ~ Area, data = attack)
summary(attack_domes)# Same results!

hist(attack_domes$residuals) #Lots of zeros so transform

attack_domes <- aov(log(attack +1)~ Area + Site, data = attack)

summary(attack_domes)

hist(attack_domes$residuals) # Still not normal 

#Lets try with manyglm

attack <- manyglm(attack ~ Area, family = "tweedie", data = attack)

#Doesn't work













## mvabund #####


### Attacks - domes ####

# # Pivot_wider the data to use it in mvabund

attack_dome_stats <- attack %>% 
  group_by (Trophic.group) %>% 
  mutate(row=row_number()) %>% 
  pivot_wider(names_from = Trophic.group, values_from = attack) %>% 
   dplyr::select(-row) %>% 
  replace(is.na(.),0) %>% 
 dplyr:: select(-Invertivore, -Planktivore)
 

# Sub-setting the variables and converting it to an mvabund object format

categories <- mvabund(attack_dome_stats[, 6:7])

# Check the spread of the data

par(mar = c(2, 10, 2, 2)) # adjusts the margins
boxplot(categories, horizontal = TRUE, las = 2, main = "Points")

#Piscivores dominating


#Check mean-variance relationship

meanvar.plot(categories)


plot(categories ~ as.factor(attack_dome_stats$Area), cex.axis = 0.4, cex = 0.4)

## Let's try with a GLM

mod1 <- manyglm(categories ~ attack_dome_stats$Area, family = "poisson")

plot(mod1)

#Looks like a fan shape so we will use negative binomial instead

mod2 <- manyglm(categories ~ attack_dome_stats$Area, family = "negative_binomial")

plot(mod2) #Looks better!

anova(mod2) #Significant effect of area - p = 0.002

# Now let's check which categories are different

test <- anova(mod2, p.uni = "adjusted") 

## The only two different between areas are piscivore (p=0.02) and

#omnivore (p=0.02)

### Investigating - domes

# # Pivot_wider the data to use it in mvabund

investig_dome_stats <- inspecting %>% 
  group_by (Trophic.group) %>% 
  mutate(row=row_number()) %>% 
  pivot_wider(names_from = Trophic.group, values_from = inspecting) %>% 
  dplyr::select(-row) %>% 
  replace(is.na(.),0)


# Sub-setting the variables and converting it to an mvabund object format

categories_inv <- mvabund(investig_dome_stats[, 5:9])

# Check the spread of the data

par(mar = c(2, 10, 2, 2)) # adjusts the margins
boxplot(categories_inv, horizontal = TRUE, las = 2, main = "Points")

#Piscivores dominating


#Check mean-variance relationship

meanvar.plot(categories_inv)


plot(categories_inv ~ as.factor(investig_dome_stats$Area), cex.axis = 0.4, cex = 0.4)

## Let's try with a GLM

mod1 <- manyglm(categories_inv ~ investig_dome_stats$Area, family = "poisson")

plot(mod1)

#Looks like a fan shape so we will use negative binomial instead

mod2 <- manyglm(categories_inv ~ investig_dome_stats$Area, family = "negative_binomial")

plot(mod2) #Looks better!

anova(mod2) #Significant effect of area - p = 0.001

# Now let's check which categories are different

test <- anova(mod2, p.uni = "adjusted") 

## Different between areas: invertivore (p=0.001); piscivore (p=0.001) and

#omnivore (p=0.003)

####### Attack - domes vs squidpops

# # Pivot_wider the data to use it in mvabund

attack_squid_stats <- attack_squid %>% 
  group_by (Trophic.group) %>% 
  mutate(row=row_number()) %>% 
  pivot_wider(names_from = Trophic.group, values_from = attack) %>% 
  dplyr::select(-row) %>% 
  replace(is.na(.),0) %>% 
  dplyr::select(-Invertivore)


# Sub-setting the variables and converting it to an mvabund object format

categories_attack_squid <- mvabund(attack_squid_stats[, 5:7])

# Check the spread of the data

par(mar = c(2, 10, 2, 2)) # adjusts the margins
boxplot(categories_attack_squid, horizontal = TRUE, las = 2, main = "Points")

#Planktivore dominating


#Check mean-variance relationship

meanvar.plot(categories_attack_squid)


plot(categories_attack_squid ~ as.factor(attack_squid_stats$Method), cex.axis = 0.4, cex = 0.4)

## Let's try with a GLM

mod1 <- manyglm(categories_attack_squid ~ attack_squid_stats$Method, family = "poisson")

plot(mod1)

#Looks like a fan shape so we will use negative binomial instead

mod2 <- manyglm(categories_attack_squid ~ attack_squid_stats$Method, family = "negative_binomial")

plot(mod2) #Looks better!

anova(mod2) #No Significant effect of methods - p = 0.05, which doesn't make sense...

# Now let's check which categories are different

test <- anova(mod2, p.uni = "adjusted") 

## Different between methods: planktivore: p=0.03


####### Inspect - domes vs squidpops

# # Pivot_wider the data to use it in mvabund

inspect_squid_stats <- inspecting_squid %>% 
  group_by (Trophic.group) %>% 
  mutate(row=row_number()) %>% 
  pivot_wider(names_from = Trophic.group, values_from = inspecting) %>% 
  dplyr::select(-row) %>% 
  replace(is.na(.),0)


# Sub-setting the variables and converting it to an mvabund object format

categories_inspect_squid <- mvabund(inspect_squid_stats[, 5:8])

# Check the spread of the data

par(mar = c(2, 10, 2, 2)) # adjusts the margins
boxplot(categories_inspect_squid, horizontal = TRUE, las = 2, main = "Points")

#Planktivore dominating


#Check mean-variance relationship

meanvar.plot(categories_inspect_squid)


plot(categories_inspect_squid ~ as.factor(inspect_squid_stats$Method), cex.axis = 0.4, cex = 0.4)

## Let's try with a GLM

mod1 <- manyglm(categories_inspect_squid ~ inspect_squid_stats$Method, family = "poisson")

plot(mod1)

#Looks like a fan shape so we will use negative binomial instead

mod2 <- manyglm(categories_inspect_squid ~ inspect_squid_stats$Method, family = "negative_binomial")

plot(mod2) #Looks better!

anova(mod2) #No significant effect of methods - p = 0.05, which doesn't make sense

# Now let's check which categories are different

test <- anova(mod2, p.uni = "adjusted") 

## Different between methods: planktivore: 0.052 ~








## Let's try nMDS ####

### Attacks - domes ##

# # Pivot_wider the data 

attack_dome_stats <- attack %>% 
  group_by (Trophic.group) %>% 
  mutate(row=row_number()) %>% 
  pivot_wider(names_from = Trophic.group, values_from = attack) %>% 
  dplyr::select(-row) %>% 
  replace(is.na(.),0) %>% 
  dplyr:: select(-Invertivore, -Planktivore)


# Sub-setting the variables

categories <- attack_dome_stats[, 5:7]

library(vegan)

attack_dome_stats.mds <- metaMDS(comm = categories, 
distance = "bray", trace = FALSE, autotransform = FALSE)

## SO many zeros! Not sure I can do this


############## Extra figures

data_domes <- data %>% 
  filter (Treatment != "No_fish") %>% 
  dplyr::select(-Treatment)


attack <- data_domes %>%
  dplyr::group_by(Area,Site, Replicate,Time, Trophic.group)%>% # sum everything in the transect by species
  dplyr::summarise(attack=sum(attack_hour,na.rm=T))%>% 
  ungroup%>% 
  tidyr::complete(Trophic.group, nesting(Area, Site, Replicate,Time),fill= list(attack=0)) %>% 
  replace(is.na(.), 0)%>% # replace the NaN resulting from dividing 0 by 0 for 0
  glimpse()

inspecting <- data_domes %>%
  dplyr::group_by(Area,Site,Replicate,Time,Trophic.group)%>% 
  dplyr::summarise(inspecting=sum(inspecting_hour,na.rm=T))%>% 
  ungroup%>% 
  tidyr::complete(Trophic.group, nesting(Area, Site,Replicate,Time),fill= list(inspecting=0)) %>% 
  replace(is.na(.), 0)%>% # replace the NaN resulting from dividing 0 by 0 for 0
  glimpse()

# Color graphs

dome_colours <- c(Sydney = "#2660A4", "Lizard Island" = "#DD614A",
                  "Malabar" = "#CAD593")

mytheme <- theme(panel.background=element_rect(fill="white"), panel.grid.minor = element_blank(), axis.ticks = element_blank(),
                 panel.grid.major = element_blank(),axis.line = element_line(size = 1, colour = "black"),
                 axis.text = element_text(size = (16), color = "black"), axis.title = element_text(size= (18)),
                 legend.key = element_rect(fill = "white"), legend.text = element_text(size=20),
                 legend.title = element_blank(), plot.title = element_text(hjust = -0.05, size = 20, face = "bold"))

plot_attack <- ggplot(attack, aes(x=Trophic.group, y=attack, fill=Area)) +
  geom_boxplot()+ 
  geom_point(shape = 21, position = position_jitterdodge(jitter.width = 0.1), size = 2) +
  scale_fill_manual(name="Area", values=dome_colours) +
  scale_x_discrete(labels=c("Herbivore"="Herb", "Invertivore"="Inv", "Omnivore"="Omn",
                            "Piscivore"="Pisc", "Planktivore"="Plank"),
                   guide = guide_axis(n.dodge = 2))+
  labs(x="", y="Attack per hour") +
  mytheme + theme(legend.position = "none")+
  ggtitle(("a)"))

plot_attack
plot_attack_log

# ggsave(plot_attack, file=here::here("graphs/Figure_attack.jpeg"),
# height = 16, width = 24, unit = "cm" )

plot_inspecting <- ggplot(inspecting,aes(x=Trophic.group, y=inspecting, fill=Area)) +
  geom_boxplot()+ 
  geom_point(shape = 21, position = position_jitterdodge(jitter.width = 0.1), size = 2) +
  scale_fill_manual(name="Area", values=dome_colours) +
  scale_x_discrete(labels=c("Herbivore"="Herb", "Invertivore"="Inv", "Omnivore"="Omn",
                            "Piscivore"="Pisc", "Planktivore"="Plank"),
                   guide = guide_axis(n.dodge = 2))+
  labs(x="", y="Inspecting per hour") +
  mytheme +theme(legend.position = "none")+
  ggtitle(("b)"))

plot_inspecting
plot_inspecting_log


# ggsave(plot_inspecting, file=here::here("graphs/Figure_inspecting.jpeg"),
#        height = 16, width = 24, unit = "cm" )

#Now all together 

library(ggpubr)

figure_3 <-ggarrange(plot_attack, plot_inspecting,
                     ncol = 2, nrow = 1,
                     common.legend = T)

figure_3


ggsave(figure_3, file=here::here("graphs/Figure_3_new.jpeg"),
       height = 16, width = 24, unit = "cm" )

