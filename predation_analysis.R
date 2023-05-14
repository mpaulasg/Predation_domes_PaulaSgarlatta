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
##  3- Stats (help by Alistair Poore)
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

city_data <- data.frame(city_name=c("Lizard Island", "Sydney"))
city_data$lat <- c(-14.664354450460095, -33.906516)
city_data$long <- c(145.45703254662746,151.245652)

#Plot of latitudinal

Lat_predation <- ggplot(data = ozmap(x="states")) + 
  geom_sf() +
  coord_sf(xlim = c(142,157), ylim = c(-40,-10), expand = FALSE)+
  geom_point(data=sites, aes(x=Longitude, y=Latitude, 
                                 colour = "#d1495b", size=2))+
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "none",
        panel.border = element_rect(colour = "black",fill = NA),
        axis.text = element_text(size = (10), colour="black"), 
        axis.title = element_blank()) + scale_size(guide = "none")+
 guides(color = guide_legend(override.aes = list(size = 3) ) ) 

Lat_predation


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

ggsave(map, file=here::here("graphs", "Figure1.jpeg"),
       height = 20, width = 18, unit = "cm")

####### 2- Fig. 3 - Comparison between tropical and temperate reefs using domes #####

data <- read.csv(here:: here("data/data_predation.csv"))

#Filter first for dome comparison

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

plot_inspection_dome <- ggplot(inspecting,aes(x=Trophic.group, y=inspecting, fill=Area)) +
  geom_boxplot()+ 
  scale_y_continuous(trans="log1p", limits = c(0,20), breaks = seq(from=0, to=20, by=5))+
  geom_point(shape = 21, position = position_jitterdodge(jitter.width = 0.1), size = 2) +
  scale_fill_manual(name="Area", values=dome_colours) +
  labs(x="", y="Inspection per hour") +
  mytheme +theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust=1))+
  ggtitle(("b)"))

plot_inspection_dome

#Now all together 

figure_3 <-ggarrange(plot_attack_dome, plot_inspection_dome, 
                     ncol = 2, nrow = 1)

figure_3


ggsave(figure_3, file=here::here("graphs/Figure_3.jpeg"),
 height = 16, width = 24, unit = "cm" )

######## 3 - Stats ########

# make unique identifier for each replicate

data_domes <- unite(data_domes, rep_ID, c("Site", "Replicate", "Time"), sep = "_", remove = FALSE)

attack_number <- data_domes %>%
  dplyr::group_by(Area,Site, rep_ID,Time, total_video_min)%>% 
  dplyr::summarise(attack=sum(attack_number,na.rm=T))

inspecting_number <- data_domes %>%
  dplyr::group_by(Area,Site,rep_ID,Time, total_video_min)%>% 
  dplyr::summarise(inspecting=sum(inspecting_number,na.rm=T))


## Adding offset for video time

domes_attack <- glmmTMB (attack   ~ Area + (1|Site) + (1|rep_ID) + offset (log(total_video_min)), 
                 data=attack_number, family = poisson()) 

summary(domes_attack)

Anova(domes_attack) # p=0.28

attack_res_domes <- simulateResiduals(domes_attack)
plot(attack_res_domes) #Good


domes_inspect <- glmmTMB (inspecting ~ Area + (1|Site) + (1|rep_ID) + offset (log(total_video_min)), 
                 data=inspecting_number, family = nbinom2 ()) 

summary(domes_inspect)

Anova(domes_inspect) #p = 0.19

inspect_res_domes <- simulateResiduals(domes_inspect)
plot(inspect_res_domes) # Good - some issues in graph residual vs predicted but test n.s

#### Let's try just one trophic group per time ####

#### Attack per trophic group ####

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

#### Inspecting per trophic group ####

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
plot(plank_inv_res_domes) # Good


######################################### end of code #################################################################################