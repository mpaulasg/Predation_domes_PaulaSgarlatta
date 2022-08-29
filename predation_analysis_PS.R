

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


attack_domes <- glmmTMB (attack ~ Area + Trophic.group + (1|Site), ziformula = ~1, data=attack, 
                         family = poisson(link = "log")) 

#####################################EVE CHANGES##########################################
#Adding an ID variable (rows with same ID, are same sample- different species)
data = mutate(data , id =  paste(Area, Site,  Replicate, Time, sep = "_"))
data = mutate(data , id =  paste(Area, Site,  Replicate, Time, sep = "_"))


#plot the data
ggplot(attack, aes(x = attack, y = interaction(Area, Trophic.group), fill = Area))+ geom_density_ridges()

#fit a model with correlated responses (trophic groups are correlated. Do the trophic groups attack different in different Areas
fit.rr <- glmmTMB(attack ~ Area * Trophic.group + rr(Trophic.group  + 0|id, d = 2) + (1|Site/ Replicate),
                  data = attack, family = nbinom2()) 

#did not converge :( I suspect there are not enough non zero observations, 

# additive model does converge though- but probably doesn't answer the research question: 
fit.rr2 <- glmmTMB(attack ~ Area + Trophic.group + rr(Trophic.group  + 0|id, d = 2)+(1|Site/Replicate),
                  data = attack, family = nbinom2()) 

plot(simulateResiduals(fit.rr2)) 
#fine


#Cannot estimate the interaction- this is because some of the Area x trophic groups are always zero
#e.g. why is Planktivore in the model when it is never present in either Lizard Island nor Sydney

#It really only makes sense to have Herbivores Omnivores and Piscivores as levels of Trophic group for this response?
attack1 = subset(attack, attack>0)
table(attack1$Trophic.group, attack1$Area)


Trophic.Groups =   unlist(attack %>% 
  group_by(Trophic.group) %>% 
  summarise(Sum = sum(attack)) %>%
  subset(Sum>0) %>%
  select(Trophic.group))
attack_subset = attack %>% subset ( Trophic.group %in% Trophic.Groups)

fit.rr3 <- glmmTMB(attack ~ Area * Trophic.group + rr(Trophic.group + 0|id, d = 2),
                   data = attack_subset, family = nbinom2()) 

#still doesn't converge :(

###############################END OF EVE###########################################

summary(attack_domes)
Anova(attack_domes) #Significant - p=0.005

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


######################################### TRYING MVABUD #######################################################


rm(list=ls()) # cleaning memory

# libraries
library(tidyverse)
library(dplyr)
library(mvabund)
library(car)
library(DHARMa)
library(emmeans)
library(here)


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