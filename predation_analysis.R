################################################################################
##
## Script for predation analysis
##
## Code by Paula Sgarlatta
##
##  1- Map of study sites
##
##  2- Fig. Comparison between tropics and temperate reefs using domes
##
##  3- Fig. Comparison between squidpops and domes in Malabar
##
##  4- Stats
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
library(mvabund)
library(glmmTMB)
library(stats)
library(car)
library(DHARMa)
library(lme4)
library(emmeans)
library(here)

#### 1 - Map of study sites ######


sites <- read.csv("data/sites_predation_v2.csv")

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
  geom_rect(xmin = 150.5, xmax = 152, ymin = -34.5, ymax = -33.5, 
            fill = NA, colour = "black", size = 0.5)+
   # geom_text(data = city_data, mapping = aes(x=long, y=lat, label=city_name,
   #           color="red"))+ # can't change the colour
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



#Plot of domes/squid in Malabar

sites_malabar <- sites %>% 
  filter(Site != "Blue Lagoon",
         Site != "Malabar for big plot")

# buffer <- 0.5
# 
# geo_bounds <- c(left = min(sites_cross$Longitude), #-buffer,
#                 bottom = min(sites_cross$Latitude), #-buffer,
#                 right = max(sites_cross$Longitude), #+buffer,
#                 top = max(sites_cross$Latitude)) #+buffer)

min_lon <- 151.25
max_lon <- 151.27
min_lat <- -33.98
max_lat <- -33.9625


geo_bounds <- c(left = min_lon, bottom = min_lat, right = max_lon, top = max_lat)


Sites.grid <- expand.grid(lon_bound = c(geo_bounds[1], geo_bounds[3]), 
                          lat_bound = c(geo_bounds[2], geo_bounds[4]))

coordinates(Sites.grid) <- ~ lon_bound + lat_bound

# Aus <- readOGR(dsn = "G:/My Drive/PhD/Chapter 2- Latitudinal-cross-shelf comparison/Data/Map/61395_shp/australia",layer = "cstauscd_r") 
Aus <- readOGR(dsn = "C:/Users/z5179758/Google Drive/PhD/Chapter 3-Predation/R/Data/61395_shp/australia",layer = "cstauscd_r")


Aus_coast <- subset(Aus, FEAT_CODE != "sea")

Aus_crop <- crop(Aus_coast, extent(Sites.grid))

sites_malabar_plot <- ggplot()+ theme_classic() + 
  geom_polygon(data = Aus_crop, aes(x=long, y=lat, group=group), fill="lightgrey", colour="black") +
  coord_equal(ratio = 1)+
  geom_point(data=sites_malabar, aes(x=Longitude, y=Latitude, 
                                   colour = Data_type, size=2)) +
  scale_color_manual(name="Data_type", values=color_type) +
  # annotation_scale(location = "br", plot_unit = "km")+ #width_hint = 0.5) +  #br is the location - bottom, right
  # annotation_north_arrow(location = "br", which_north = "true", 
  # pad_x = unit(2, "in"), pad_y = unit(0.1, "in"),
  # style = north_arrow_fancy_orienteering) +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = c(0.8,0.18), legend.title = element_blank(),
        legend.text = element_text(size=12),
        panel.border = element_rect(colour = "black",fill = NA),
        axis.text = element_text(size = (14), colour="black"), 
        axis.title = element_blank()) + scale_size(guide = "none")+
  guides(color = guide_legend(override.aes = list(size = 3) ) )+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))

sites_malabar_plot #check the detail of the coast

sites_malabar_plot <- ggplot(data = ozmap(x="states")) + 
  geom_sf() +
  coord_sf(xlim = c(151.25,151.26), ylim = c(-33.98,-33.96), expand = FALSE)+
  geom_point(data=sites_malabar, aes(x=Longitude, y=Latitude, 
                             colour = Data_type, size=2))+
  geom_rect(xmin = 150.5, xmax = 152, ymin = -34.5, ymax = -33.5, 
            fill = NA, colour = "black", size = 0.5)+
  # geom_text(data = city_data, mapping = aes(x=long, y=lat, label=city_name,
  #           color="red"))+ # can't change the colour
  scale_color_manual(name="Data_type", values=color_type) +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "none",
        panel.border = element_rect(colour = "black",fill = NA),
        axis.text = element_text(size = (10), colour="black"), 
        axis.title = element_blank()) + scale_size(guide = "none")+
  guides(color = guide_legend(override.aes = list(size = 3) ) ) 

Lat_predation

# ggsave(sites_malabar_plot, file="graphs/sites_malabar.jpeg",
#        height = 10, width = 18, unit = "cm")


#Plot of Australia/rectangle in the area of study

Aus <- ggplot(data = ozmap(x="country")) + 
  geom_sf(color="black", fill= "white") +
  geom_rect(xmin = 142, xmax = 156, ymin = -36, ymax = -14, 
            fill = NA, colour = "black", size = 0.5) +
  scale_fill_viridis_d(option = "plasma") +
  coord_sf(xlim = c(100.00, 160.00), ylim = c(-45.00, -10.00), expand = TRUE) +
  theme_void()

###Place figures together

map <- ggdraw(Lat_predation) +
  draw_plot(Aus, x = 0.5, y = 0.8, width = 0.24, height = 0.24)

map

ggsave(map, file=here::here("graphs", "Figure1.jpeg"),
       height = 20, width = 18, unit = "cm")

####### 2- Comparison between tropical and temperate reefs using domes #####

data <- read.csv(here:: here("data/data_predation_squidpops.csv"))

#Filter first for dome comparison

data_domes <- data %>% 
  filter (Method != "Squidpop",
          Treatment != "No_fish") %>% 
  dplyr::select(-Treatment)
  

attack_species <- data_domes %>%
  dplyr::group_by(Area,Site, Replicate,Time)%>% # sum everything in the transect by species
  dplyr::summarise(attack=sum(attack_hour,na.rm=T))

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

#Calculate mean

attack_toplot<- Rmisc::summarySE(attack, na.rm= T, measurevar=c("attack"), 
                                         groupvars=c("Area", "Trophic.group"))

attack_toplot_species<- Rmisc::summarySE(attack_species, na.rm= T, measurevar=c("attack"), 
                                 groupvars=c("Area"))

inspecting_toplot<- Rmisc::summarySE(inspecting, na.rm= T, measurevar=c("inspecting"), 
                                 groupvars=c("Area", "Trophic.group"))

# Color graphs

trophic_colours <- c(Piscivore = "#2660A4", Herbivore = "#CBA328", Invertivore = "#DD614A",
                      Planktivore = "#F4A698", Omnivore = "#D1CFE2")

mytheme <- theme(panel.background=element_rect(fill="white"), panel.grid.minor = element_blank(), axis.ticks = element_blank(),
                 panel.grid.major = element_blank(),axis.line = element_line(size = 1, colour = "black"),
                 axis.text = element_text(size = (18), color = "black"), axis.title = element_text(size= (18)),
                 legend.key = element_rect(fill = "white"), legend.text = element_text(size=20),
                 legend.title = element_blank())

plot_attack <- ggplot(attack_toplot, aes(x=Area, y=attack, fill=Trophic.group)) +
  geom_col(color="black", size=0.8, position=position_dodge(width=0.6)) +
  geom_errorbar( aes(x=Area, ymin=attack-se, ymax=attack+se), 
  width=0.1, size=0.8, colour="black", position=position_dodge(width=0.6) ) +
  scale_fill_manual(values=trophic_colours) + 
  labs(x="", y="Attack per hour") +
  mytheme +
  ggtitle(("(a)"))

plot_attack

plot_attack_species <- ggplot(attack_toplot_species, aes(x=Area, y=attack)) +
  geom_col(color="black", size=0.8, position=position_dodge(width=0.6)) +
  geom_errorbar( aes(x=Area, ymin=attack-se, ymax=attack+se), 
                 width=0.1, size=0.8, colour="black", position=position_dodge(width=0.6) ) +
  labs(x="", y="Attack per hour") +
  mytheme +
  ggtitle(("(a)"))

plot_attack_species

#ggsave(plot_attack, file=here::here("graphs/Figure_attack.jpeg"),
      # height = 22, width = 25, unit = "cm" )

plot_inspecting <- ggplot(inspecting_toplot,aes(x=Area, y=inspecting, fill=Trophic.group)) +
  geom_col(color="black", size=0.8, position=position_dodge(width=0.6))+
  geom_errorbar( aes(x=Area, ymin=inspecting-se, ymax=inspecting+se), width=0.1, size=0.8, 
                 position=position_dodge(width=0.6), colour="black" ) +
  scale_fill_manual(values=trophic_colours) + 
  labs(x="", y="Inspecting per hour") +
  mytheme +
  ggtitle(("(b)"))

plot_inspecting

# ggsave(plot_inspecting, file=here::here("graphs/Figure_inspecting.jpeg"),
#        height = 22, width = 25, unit = "cm" )

#### Now comparing with squidpops ##########

data_squid <- data %>% 
 dplyr:: filter (Area != "Lizard Island", 
          Site != "Malabar 1 ",
          Treatment != "No_fish") %>% 
 dplyr::select(-Treatment)

attack_squid <- data_squid %>%
  dplyr::group_by(Method,Site,Replicate,Time,Trophic.group)%>% # sum everything in the transect by species
  dplyr::summarise(attack=sum(attack_number,na.rm=T))%>% 
  ungroup%>% 
  tidyr::complete(Trophic.group, nesting(Method, Site,Replicate,Time),fill= list(attack=0)) %>% 
  replace(is.na(.), 0)%>% # replace the NaN resulting from dividing 0 by 0 for 0
  glimpse()

inspecting_squid <- data_squid %>%
  dplyr::group_by(Method,Site,Replicate,Time, Trophic.group)%>% 
  dplyr::summarise(inspecting=sum(inspecting_hour,na.rm=T))%>% 
  ungroup%>% 
  tidyr::complete(Trophic.group, nesting(Method,Site,Replicate,Time),fill= list(inspecting=0)) %>% 
  replace(is.na(.), 0)%>% # replace the NaN resulting from dividing 0 by 0 for 0
  glimpse()


attack_squid_toplot<- Rmisc::summarySE(attack_squid, na.rm= T, measurevar=c("attack"), 
                                 groupvars=c("Method", "Trophic.group"))

inspect_squid_toplot<- Rmisc::summarySE(inspecting_squid, na.rm= T, measurevar=c("inspecting"), 
                                       groupvars=c("Method", "Trophic.group"))

plot_attack_squid <- ggplot(attack_squid_toplot, aes(x=Method, y=attack, fill=Trophic.group)) +
  geom_col(color="black", size=0.8, position=position_dodge(width=0.6)) +
  geom_errorbar( aes(x=Method, ymin=attack-se, ymax=attack+se), 
                 width=0.1, size=0.8, colour="black", position=position_dodge(width=0.6) ) +
  scale_fill_manual(values=trophic_colours) + 
  labs(x="", y="Attack per hour") +
  theme(panel.background=element_rect(fill="white"), panel.grid.minor = element_blank(), axis.ticks = element_blank(),
        panel.grid.major = element_blank(),axis.line = element_line(size = 1, colour = "black"),
        axis.text = element_text(size = (18), color = "black"), axis.title = element_text(size= (18)),
        legend.key = element_rect(fill = "white"), legend.text = element_text(size=20),
        legend.title = element_blank())+
  ggtitle(("(c)"))

plot_attack_squid

# ggsave(plot_attack_squid, file=here::here("graphs/Figure_squid_dome.jpeg"),
#        height = 22, width = 25, unit = "cm" )

plot_inspect_squid <- ggplot(inspect_squid_toplot, aes(x=Method, y=inspecting, fill=Trophic.group)) +
  geom_col(color="black", size=0.8, position=position_dodge(width=0.6)) +
  geom_errorbar( aes(x=Method, ymin=inspecting-se, ymax=inspecting+se), 
                 width=0.1, size=0.8, colour="black", position=position_dodge(width=0.6) ) +
  scale_fill_manual(values=trophic_colours) + 
  labs(x="", y="Inspecting per hour") +
  theme(panel.background=element_rect(fill="white"), panel.grid.minor = element_blank(), axis.ticks = element_blank(),
        panel.grid.major = element_blank(),axis.line = element_line(size = 1, colour = "black"),
        axis.text = element_text(size = (18), color = "black"), axis.title = element_text(size= (18)),
        legend.key = element_rect(fill = "white"), legend.text = element_text(size=20),
        legend.title = element_blank())+
  ggtitle(("(d)"))

plot_inspect_squid

#Now all together 

figure_3 <-ggarrange(plot_attack, plot_inspecting, plot_attack_squid,plot_inspect_squid,
          ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")

figure_3

#ggsave(figure_2, file=here::here("graphs/Figure_2.jpeg"),
       #height = 22, width = 25, unit = "cm" )






######## Stats ########

data <- read.csv(here:: here("data/data_predation_squidpops.csv"))

### Attacks - domes 

#Filter first for dome comparison

data_domes <- data %>% 
  filter (Method != "Squidpop",
          Treatment != "No_fish") %>% 
  dplyr::select(-Treatment)

attack_2 <- data_domes %>%
  dplyr::group_by(Area,Site, Replicate,Time)%>% # sum everything in the replicate by species
  dplyr::summarise(attack=sum(attack_hour,na.rm=T)) %>% 
  ungroup%>% 
  tidyr::complete(Area, fill= list(attack=0)) %>% 
  replace(is.na(.), 0)%>% # replace the NaN resulting from dividing 0 by 0 for 0
  glimpse()

inspecting_2 <- data_domes %>%
  dplyr::group_by(Area,Site, Replicate,Time)%>% # sum everything in the replicate by species
  dplyr::summarise(inspecting=sum(inspecting_hour,na.rm=T)) %>% 
  ungroup%>% 
  tidyr::complete(Area, fill= list(inspecting=0)) %>% 
  replace(is.na(.), 0)%>% # replace the NaN resulting from dividing 0 by 0 for 0
  glimpse()

boxplot(attack ~ Area, data=attack_2) + geom_jitter()

boxplot(inspecting ~ Area, data=inspecting_2) + geom_jitter()

  # #Try with binomial data and log data
  # 
  # attack_binary <- attack_2 %>% 
  # mutate(binary=ifelse(attack >0, 1, 0)) %>% 
  # mutate(log_attack = log10(attack + 1))

  ############# Trying with GLMM - Attacks domes 
  
  
  attack_domes <- glmmTMB (attack  ~ Area + (1|Site) + (1|Replicate), data=attack_2, 
                           family = poisson(link="log")) 
  
  
  Anova(attack_domes) # p=0.19, which is surprising? 
  
  attack_res_domes <- simulateResiduals(attack_domes)
  plot(attack_res_domes) #Good
  testDispersion(attack_res_domes) 

  # Tried with 0,1 data but same result
  
  ############## Inspecting
  

  inspect_domes <- glmmTMB (inspecting ~ Area + (1|Site) + (1|Replicate), data=inspecting_2, 
                             family = poisson(link = "log")) 
  
  Anova(inspect_domes) #Significant - p=0.4 - this is surprising?
  
  inspec_res_domes <- simulateResiduals(inspect_domes)
  plot(inspec_res_domes) # Good! 
  
  ####Let's try just one trophic group per time. 
  
  attack <- data_domes %>%
    dplyr::group_by(Area,Site, Replicate,Time, Trophic.group)%>% # sum everything in the replicate by species
    dplyr::summarise(attack=sum(attack_hour,na.rm=T)) %>% 
    ungroup%>% 
    tidyr::complete(Trophic.group, nesting(Area,Site, Replicate,Time), fill= list(attack=0)) %>% 
    replace(is.na(.), 0)%>% # replace the NaN resulting from dividing 0 by 0 for 0
    glimpse()
  
  inspecting_2 <- data_domes %>%
    dplyr::group_by(Area,Site, Replicate,Time, Trophic.group)%>% # sum everything in the replicate by species
    dplyr::summarise(inspecting=sum(inspecting_hour,na.rm=T)) %>% 
    ungroup%>% 
    tidyr::complete(Trophic.group, nesting(Area,Site, Replicate,Time), fill= list(inspecting=0)) %>% 
    replace(is.na(.), 0)%>% # replace the NaN resulting from dividing 0 by 0 for 0
    glimpse()
  
  
  
  piscivores <- attack %>% 
    filter(Trophic.group == "Piscivore")
  
  pisc <- glmmTMB (attack ~ Area + (1|Site) + (1|Replicate), data=piscivores, 
                   family = poisson(link = "log")) 
  

  Anova(pisc) #p=0.0003991
  
  #This one  works!
  
  pis_res_domes <- simulateResiduals(pisc)
  plot(pis_res_domes) # Good!
  
  Omnivore <- attack %>% 
    filter(Trophic.group == "Omnivore")
  
  omn <- glmmTMB(attack  ~ Area + (1|Site) + (1|Replicate),  data = Omnivore,
                 family = poisson(link = "log"))
  
  Anova(omn) #Cannot run the model, the model doesn't fit. I guess the model doesn't fit because there are not
  #omnivores in Lizard

  Herbivore <- attack %>% 
    filter(Trophic.group == "Herbivore")
  
  herb <- glmmTMB(attack ~ Area + (1|Site) + (1|Replicate), data=Herbivore,
                  family= poisson(link = "log"))
  
  Anova(herb)#Cannot run the model, the model doesn't fit. I guess the model doesn't fit because there are not
  #herbivores in Sydney
  
  
  
  piscivores_inv <- inspecting_2 %>% 
    filter(Trophic.group == "Piscivore")
  
  pisc_inv <- glmmTMB(log(inspecting + 1) ~ Area + (1|Site) + (1|Replicate), data=piscivores_inv,
                      family = tweedie(link = "log"))
  
  summary(pisc_inv)
  Anova(pisc_inv) #Significant - p=0.002
  
  pisc_inv_res_domes <- simulateResiduals(pisc_inv)
  plot(pisc_inv_res_domes) # Perfect
  
  
  Omnivore_inv <- inspecting_2 %>% 
    filter(Trophic.group == "Omnivore")
  
  omn_inv <- glmmTMB(log(inspecting + 1) ~ Area + (1|Site) + (1|Replicate), data=Omnivore_inv,
                     family = poisson(link = "log"))
  
  Anova(omn_inv)#Cannot run the model, the model doesn't fit. I guess the model doesn't fit because there are not
  #omnivores in Lizard
  
  
  Herbivore_inv <- inspecting_2 %>% 
    filter(Trophic.group == "Herbivore")
  
  herb_inv <- glmmTMB(inspecting ~ Area + (1|Site) + (1|Replicate), data=Herbivore_inv,
                      family = poisson(link = "log"))
  
  Anova(herb_inv)#Cannot run the model, the model doesn't fit. I guess the model doesn't fit because there are not
  #herbivores in Sydney
  
  planktivore_inv <- inspecting_2 %>% 
    filter(Trophic.group == "Planktivore")
  
  plank_inv <- glmmTMB(inspecting ~ Area + (1|Site) + (1|Replicate), data=planktivore_inv,
                       family = poisson(link = "log"))
  
  Anova(plank_inv) # p=0.63
  
  plank_inv_res_domes <- simulateResiduals(plank_inv)
  plot(plank_inv_res_domes) # Good!
  
  
  
  
  
############ Comparison domes/squidpops  
  
  data_squid <- data %>% 
    dplyr:: filter (Area != "Lizard Island", 
                    Site != "Malabar 1 ",
                    Treatment != "No_fish") %>% 
    dplyr::select(-Treatment)
  
  attack_2_squid <- data_squid %>%
    dplyr::group_by(Method,Site, Replicate,Time)%>% # sum everything in the replicate by species
    dplyr::summarise(attack=sum(attack_hour,na.rm=T)) %>% 
    ungroup%>% 
    tidyr::complete(Method, fill= list(attack=0)) %>% 
    replace(is.na(.), 0)%>% # replace the NaN resulting from dividing 0 by 0 for 0
    glimpse()
  
  inspecting_2_squid <- data_squid %>%
    dplyr::group_by(Method, Site, Replicate,Time)%>% # sum everything in the replicate by species
    dplyr::summarise(inspecting=sum(inspecting_hour,na.rm=T)) %>% 
    ungroup%>% 
    tidyr::complete(Method, fill= list(inspecting=0)) %>% 
    replace(is.na(.), 0)%>% # replace the NaN resulting from dividing 0 by 0 for 0
    glimpse()
  
  ############# Trying with GLMM - Attacks domes/squidpops 
  
  
  attack_squid <- glmmTMB (attack  ~ Method + (1|Site) + (1|Replicate), data=attack_2_squid, 
                           family = poisson(link="log")) 
  
  
  Anova(attack_squid) # p<0.001 
  
  attack_res_squid <- simulateResiduals(attack_squid)
  plot(attack_res_squid) #This one no working
  testDispersion(attack_res_squid) # this one looks good
  bartlett.test(attack  ~ Method, data=attack_2_squid)#Good kinda
  
  ############## Inspecting
  
  
  inspect_squid <- glmmTMB (inspecting ~ Method + (1|Site) + (1|Replicate), data=inspecting_2_squid, 
                            family = poisson(link = "log")) 
  
  Anova(inspect_squid) # ~ no significant - p=0.048*
  
  inspec_res_squid <- simulateResiduals(inspect_squid)
  plot(inspec_res_squid) # Not enough points I guess 
  testDispersion(inspec_res_squid) # this one looks good
  bartlett.test(inspecting ~ Method, data=inspecting_2_squid)#Good
  
  
  #######Let's try just one trophic group per time. 
  
  attack_squid_2 <- data_squid %>%
    dplyr::group_by(Method,Site, Replicate,Time, Trophic.group)%>% # sum everything in the replicate by species
    dplyr::summarise(attack=sum(attack_hour,na.rm=T)) %>% 
    ungroup%>% 
    tidyr::complete(Trophic.group, nesting(Method,Site, Replicate,Time), fill= list(attack=0)) %>% 
    replace(is.na(.), 0)%>% # replace the NaN resulting from dividing 0 by 0 for 0
    glimpse()
  
  inspecting_squid_2 <- data_squid %>%
    dplyr::group_by(Method,Site, Replicate,Time, Trophic.group)%>% # sum everything in the replicate by species
    dplyr::summarise(inspecting=sum(inspecting_hour,na.rm=T)) %>% 
    ungroup%>% 
    tidyr::complete(Trophic.group, nesting(Method,Site, Replicate,Time), fill= list(inspecting=0)) %>% 
    replace(is.na(.), 0)%>% # replace the NaN resulting from dividing 0 by 0 for 0
    glimpse()
  
  
  
  piscivores_squid <- attack_squid_2 %>% 
    filter(Trophic.group == "Piscivore")
  
  pisc_squid <- glmmTMB (attack ~ Method + (1|Site) + (1|Replicate), data=piscivores_squid, 
                   family = poisson(link = "log")) 
  
  
  Anova(pisc_squid) #Not working because no piscivores with squidpops
  
    Omnivore_squid <- attack_squid_2 %>% 
    filter(Trophic.group == "Omnivore")
  
  omn_squid <- glmmTMB(attack  ~ Method + (1|Site) + (1|Replicate),  data = Omnivore_squid,
                 family = poisson(link = "log"))
  
  Anova(omn_squid) #p<0.008
  omn_attack_res_squid <- simulateResiduals(omn_squid)
  plot(omn_attack_res_squid) # Not so many points 
  testDispersion(omn_attack_res_squid) # this one looks good
  bartlett.test(attack  ~ Method , data=Omnivore_squid)#Good
  
  
  Herbivore_squid <- attack_squid_2 %>% 
    filter(Trophic.group == "Herbivore")
  
  herb <- glmmTMB(attack ~ Method + (1|Site) + (1|Replicate), data=Herbivore_squid,
                  family= poisson(link = "log"))
  
  Anova(herb)#Cannot run the model, the model doesn't fit. I guess the model doesn't fit because there are not
  #herbivores in Sydney
  

  piscivores_squid_inv <- inspecting_squid_2 %>% 
    filter(Trophic.group == "Piscivore")
  
  pisc_squid_inv <- glmmTMB(inspecting  ~ Method + (1|Site) + (1|Replicate), data=piscivores_squid_inv,
                      family = poisson(link="log"))
  
   Anova(pisc_squid_inv) #Cannot run the model, the model doesn't fit. I guess the model doesn't fit because there are not
   #piscivores in Malabar
  
  
  Omnivore_inv <- inspecting_squid_2 %>% 
    filter(Trophic.group == "Omnivore")
  
  omn_squid_inv <- glmmTMB(log(inspecting + 1) ~ Method + (1|Site) + (1|Replicate), data=Omnivore_inv,
                     family = poisson(link = "log"))
  
  Anova(omn_inv)#Cannot run the model, the model doesn't fit. I guess the model doesn't fit because there are not
  #omnivores with squidpops
  
  
  planktivore_inv <- inspecting_squid_2 %>% 
    filter(Trophic.group == "Planktivore")
  
  plank_inv <- glmmTMB(log(inspecting + 1) ~ Method + (1|Site) + (1|Replicate), data=planktivore_inv,
                       family = poisson(link = "log"))
  
  Anova(plank_inv) # p=0.02
  
  plank_inv_res_squid <- simulateResiduals(plank_inv)
  plot(plank_inv_res_squid) # too few points 
  testDispersion(plank_inv_res_squid) # this one looks good
  bartlett.test(log(inspecting + 1)  ~ Method , data=planktivore_inv)#Good

  
  
  
  
  
  
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
