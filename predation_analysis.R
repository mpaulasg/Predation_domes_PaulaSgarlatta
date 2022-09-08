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
  filter (Area != "Lizard Island", 
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

### Attacks - domes 

## Trying with GLMM - Attacks domes 

attack_domes <- glmmTMB (attack ~ Area*Trophic.group + (1|Site), data=attack, 
                         family = poisson(link = "log")) 


## GLMM doesn't work because the interaction is not fitting the model. 

## Let's try a nested ANOVA - first model just with the total community to see if the attacks are different. 

attack_domes <- aov(attack ~ Area/Site, data = attack)

summary(attack_domes) # Not significant 

attack_res_domes <- simulateResiduals(attack_domes)
plot(attack_res_domes) # A little bit of dispersion but KS test good.  

#awesome lets do post hoc tests

pairs(emmeans(attack_domes,spec=~Area|Trophic.group, type="response")) # This looks suspicious to me (all
# same p values). 
pairs(emmeans(attack_domes,spec=~Trophic.group|Area, type="response")) # This is also suspicious, 
## same p values in both areas

### investigating - domes 

insepect_domes <- glmmTMB (inspecting ~ Area + Trophic.group + (1|Site), ziformula = ~1, data=inspecting, 
                         family = poisson(link = "log")) 


summary(insepect_domes)
Anova(insepect_domes) #Significant - p=0.00014

inspec_res_domes <- simulateResiduals(insepect_domes)
plot(inspec_res_domes) # Residuals not good??  

#lets do post hoc tests

pairs(emmeans(insepect_domes,spec=~Area|Trophic.group, type="response")) # This looks suspicious to me (all
# same p values). 
pairs(emmeans(insepect_domes,spec=~Trophic.group|Area, type="response")) # This is also suspicious, 
## same p values in both areas



### Attacks - domes/squidpops 

attack_squid_glmm <- glmmTMB (attack ~ Method + Trophic.group , ziformula = ~1, data=attack_squid, 
                         family = poisson(link = "log")) 


summary(attack_squid_glmm)
Anova(attack_squid_glmm) #Significant - p<0.0005

attack_res_squid <- simulateResiduals(attack_squid_glmm)
plot(attack_res_squid) # Residuals don't look great  

#awesome lets do post hoc tests

pairs(emmeans(attack_squid_glmm,spec=~Method|Trophic.group, type="response")) # This looks suspicious to me (all
# same p values). 
pairs(emmeans(attack_squid_glmm,spec=~Trophic.group|Method, type="response")) # This is also suspicious, 
## same p values in both methods

### investigating - domes/squidpops

inspect_squid_glmm <- glmmTMB (inspecting ~ Method + Trophic.group , ziformula = ~1, data=inspecting_squid, 
                           family = poisson(link = "log")) 


summary(inspect_squid_glmm)
Anova(inspect_squid_glmm) #Significant - p=0.002

inspec_res_squid <- simulateResiduals(inspect_squid_glmm)
plot(inspec_res_squid) # Residuals not good??  

#lets do post hoc tests

pairs(emmeans(inspect_squid_glmm,spec=~Method|Trophic.group, type="response")) # This looks suspicious to me (all
# same p values). 
pairs(emmeans(inspect_squid_glmm,spec=~Trophic.group|Method, type="response")) # This is also suspicious, 
## same p values in both areas

### Attacks - domes 

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





