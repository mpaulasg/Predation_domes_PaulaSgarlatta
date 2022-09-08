

rm(list=ls()) # cleaning memory

# libraries
library(tidyverse)
library(dplyr)
library(glmmTMB)
library(car)
library(DHARMa)
library(emmeans)
library(here)

####### 2- Comparison between tropics and temperate reefs using domes #####

data <- read.csv("data_predation_squidpops.csv")


#Filter first for dome comparison

data_domes <- data %>% 
  filter (Method != "Squidpop",
          Treatment != "No_fish") %>% 
  dplyr::select(-Treatment)  # Deleting treatment as no fish = 0


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


### Now comparing with squidpops

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


######## Stats

### Attacks - domes 

## Trying with GLMM - Attacks domes 


# attack_domes <- glmmTMB (attack ~ Area + Trophic.group + (1|Site), ziformula = ~1, data=attack, 
#                          family = poisson(link = "log")) 

#####################################EVE CHANGES##########################################
#Adding an ID variable (rows with same ID, are same sample- different species)
data = mutate(data , id =  paste(Area, Site,  Replicate, Time, sep = "_"))
attack = mutate(attack , id =  paste(Area, Site,  Replicate, Time, sep = "_"))
inspecting = mutate(inspecting , id =  paste(Area, Site,  Replicate, Time, sep = "_"))
attack_squid = mutate(attack_squid, id =  paste( Site,  Replicate, Time, sep = "_"))

#plot the data
library(ggridges)
ggplot(attack, aes(x = attack, y = interaction(Area, Trophic.group), fill = Area))+ geom_density_ridges()

#fit a model with correlated responses (trophic groups are correlated. Do the trophic groups attack different in different Areas
#fit.rr <- glmmTMB(attack ~ Area * Trophic.group + rr(Trophic.group  + 0|id, d = 2) + (1|Site/ Replicate),
#                  data = attack, family = nbinom2()) 

#did not converge :( I suspect there are not enough non zero observations, 

#mod2_optim <- update(fit.rr ,  control=glmmTMBControl(optimizer=optim,                            optArgs=list(method="BFGS")))

# additive model does converge though- but probably doesn't answer the research question: 
#fit.rr2 <- glmmTMB(attack ~ Area + Trophic.group + rr(Trophic.group  + 0|id, d = 2)+(1|Site/Replicate),
#                  data = attack, family = nbinom2()) 

#plot(simulateResiduals(fit.rr2)) 
#fine


#Cannot estimate the interaction- this is because some of the Area x trophic groups are always zero
#e.g. why is Planktivore in the model when it is never present in either Lizard Island nor Sydney

#It really only makes sense to have Herbivores Omnivores and Piscivores as levels of Trophic group for this response?
attack1 = subset(attack, attack>0)
table(attack1$Trophic.group, attack1$Area)
table(attack$Trophic.group, attack$Area)


Trophic.Groups =   unlist(attack %>% 
  group_by(Trophic.group) %>% 
  summarise(Sum = sum(attack)) %>%
  subset(Sum>0) %>%
  select(Trophic.group))

attack_subset = attack %>% subset ( Trophic.group %in% Trophic.Groups) 

#fit.rr3 <- glmmTMB(attack ~ Area * Trophic.group + rr(Trophic.group + 0|id, d = 2),
#                   data = attack_subset, family = nbinom2()) 
#even cutting down to 3 trophic groups is still not working


#there is only one site with one herbivore which is making things hard for the model. 
attack_subset2 = attack %>% subset ( Trophic.group %in% Trophic.Groups)%>% subset(Trophic.group != "Herbivore")

#This model asks whether Omnivores and Piscivores attack differently in Sydney vs Lizard Island (we don't have enough data on other trophic groups)
fit.rr4 <- glmmTMB(attack ~ Area * Trophic.group + rr(Trophic.group + 0|id, d = 2) + (1|Site) + (1|id),
                   data = attack_subset2, family = poisson()) 

#check assumptions
plot(simulateResiduals(fit.rr4))

#test interactions
fit.rr4_null <- glmmTMB(attack ~ Area + Trophic.group+ rr(Trophic.group + 0|id, d = 2) + (1|Site) + (1|id),
                   data = attack_subset2, family = poisson()) 

anova(fit.rr4_null ,fit.rr4)

#interaction significant. test contrasts

emmeans(fit.rr4, pairwise~Area|Trophic.group, type= "response")

#Piscivores respond differently in attacks (attack 100 times more in Lizard Island); no evidence for omnivores
ggplot(attack_subset2, aes(Area, attack, colour = Site))+geom_jitter()+facet_wrap(~Trophic.group)
#Conclusions for herbivores/ Inventivores and Planktivores are all no evidence- graphically :P 
ggplot(attack, aes(Area, attack, colour = Site))+geom_jitter()+facet_wrap(~Trophic.group)


# summary(attack_domes)
# Anova(attack_domes) #Significant - p=0.005
# 
# attack_res_domes <- simulateResiduals(attack_domes)
# plot(attack_res_domes) # A little bit of dispersion but KS test good.  
# 
# #awesome lets do post hoc tests
# 
# pairs(emmeans(attack_domes,spec=~Area|Trophic.group, type="response")) # This looks suspicious to me (all
# # same p values). 
# pairs(emmeans(attack_domes,spec=~Trophic.group|Area, type="response")) # This is also suspicious, 
# ## same p values in both areas
# 
# ### investigating - domes 

#insepect_domes <- glmmTMB (inspecting ~ Area + Trophic.group + (1|Site), ziformula = ~1, data=inspecting, 
#                           family = poisson(link = "log")) 


insepect_domes <- glmmTMB(inspecting ~ Area * Trophic.group + (1|Site) ,
                   data = inspecting, family = tweedie()) 

emmeans(insepect_domes, pairwise ~Area|Trophic.group)

library(rstanarm)
#stan refuses to work on non count poisson data. So round the response 


insepect_domes_stan2 = stan_glmer(round(inspecting) ~ Area * Trophic.group + (1|Site) ,
                                 data = inspecting, family = neg_binomial_2()) 

#looks like stan could work and use this to fix up the output https://cran.r-project.org/web/packages/tidybayes/vignettes/tidybayes.html

#shows that invertivores and  omnivores and herbs have differences that do not overlap zero. Note that the HPD at 0.9 is kind of like a CI at 0.95. 

#Piscivores on the boundary! The presence of some non zeros has caused this.  
library(tidybayes)
insepect_domes_stan2  %>%
  emmeans(pairwise ~ Area|Trophic.group, level = 0.9)



ggplot(inspecting, aes(Area, inspecting, colour = Site))+geom_jitter()+facet_wrap(~Trophic.group)



### Attacks - domes/squidpops 

#attack_squid_glmm <- glmmTMB (attack ~ Method + Trophic.group , ziformula = ~1, data=attack_squid, 
#                              family = poisson(link = "log")) 

attack_squid_glmm <- glmmTMB(attack ~ Method* Trophic.group +(1|id) ,
                          data = attack_squid, family = poisson()) 

#try with stan_glmer instead

attack_squid_stan = stan_glmer(attack ~ Method* Trophic.group + (1|Site)+(1|id) ,
           data = attack_squid, family = poisson()) 

#plot
ggplot(attack_squid, aes(Method, attack, colour = Site))+geom_jitter()+facet_wrap(~Trophic.group)

attack_squid_stan   %>%
  emmeans(pairwise ~ Method|Trophic.group, level = 0.9)


#Now go back the try the attack vs area model:

attack_squid_stan = stan_glmer(round(attack) ~Area*Trophic.group + (1|Site) ,
                               data = attack, family = poisson()) 


emmeans(attack_squid_stan, pairwise~Area|Trophic.group, level = 0.9) 
#Results qualitatively the same as before when we did glmmTMB on just the two trophic groups without the others, but more satisfactory -  we have omnivores and piscivores different. Others are no evidence of difference. 