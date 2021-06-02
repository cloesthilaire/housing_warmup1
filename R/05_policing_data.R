#Data policing report########################################################

source("R/01_startup.R")
load("output/geometry.Rdata")
load("output/int.Rdata")


#Load wes anderson colors ---------------------------------------------------

library(wesanderson)

pal <- wes_palette("Zissou1", 10, type = c("continuous"))

 

#Import boundaries of PDQs --------------------------------------------------

PDQ_sf <-
  read_sf("data/Limites/Limites_PDQ.shp") %>% 
  st_transform(32618)


#Putting PDQ and crimes together (notice that this dataset has less data)
int_PDQ <- st_join(PDQ_sf,int %>% select(-PDQ))

# Data visualization of interventions ---------------------------------------

#Change the date to separate into year
int_PDQ %>% 
  mutate(DATE = year(DATE)) %>%
  group_by(PDQ, CATEGORIE) %>% 
  summarize(number_intervention = n()) %>% 
  ggplot()+
  geom_sf(aes(fill=number_intervention), color="transparent")+
  theme_void()+
  scale_fill_gradientn(name="Number of interventions",
                       colors=col_palette[c(4, 1, 9)])+
  facet_wrap(~CATEGORIE)

#Make map set (1 by category of intervention)
#Mefait
int_PDQ %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(DATE != 2021) %>% 
  filter(CATEGORIE == "Mefait") %>% 
  group_by(NOM_PDQ, DATE) %>%
  summarize(number_intervention = n()) %>% 
  ggplot()+
  geom_sf(aes(fill=number_intervention), color="transparent")+
  theme_void()+
  scale_fill_gradientn(name="Number of interventions",
                       colors=pal)+
  labs(title="Number of mefaits by PDQ per year")+
  facet_wrap(~DATE)

#Facet_wrap crée un graph par catégorie assignée

#Vol de vehicule a moteur
int_PDQ %>% 
  filter(CATEGORIE == "Vol de vehicule a moteur") %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(DATE != 2021) %>%
  group_by(NOM_PDQ, DATE) %>% 
  summarize(number_intervention = n()) %>% 
  ggplot()+
  geom_sf(aes(fill=number_intervention), color="transparent")+
  theme_void()+
  scale_fill_gradientn(name="Number of interventions",
                       colors=col_palette[c(4, 1, 9)])+
  labs(title="Number of vehicule theft by PDQ per year")+
  facet_wrap(~DATE)

#Vols qualifies
int_PDQ %>% 
  filter(CATEGORIE == "Vols qualifies") %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(DATE != 2021) %>% 
  group_by(NOM_PDQ, DATE) %>% 
  summarize(number_intervention = n()) %>% 
  ggplot()+
  geom_sf(aes(fill=number_intervention), color="transparent")+
  theme_void()+
  scale_fill_gradientn(name="Number of interventions",
                       colors=col_palette[c(4, 1, 9)])+
  labs(title="Number of skilled thefts by PDQ per year")+
  facet_wrap(~DATE)
#Vol dans / sur vehicule a moteur
int_PDQ %>% 
  filter(CATEGORIE == "Vol dans / sur vehicule a moteur") %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(DATE != 2021) %>% 
  group_by(NOM_PDQ, DATE) %>% 
  summarize(number_intervention = n()) %>% 
  ggplot()+
  geom_sf(aes(fill=number_intervention), color="transparent")+
  theme_void()+
  scale_fill_gradientn(name="Number of interventions",
                       colors=col_palette[c(4, 1, 9)])+
  labs(title="Number of thefts of/in motor vehicles by PDQ per year")+
  facet_wrap(~DATE)
#Infractions entrainant la mort (NEED TO FIX BORDERS)
int_PDQ %>% 
  filter(CATEGORIE == "Infractions entrainant la mort") %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(DATE != 2021) %>% 
  group_by(NOM_PDQ, DATE) %>% 
  summarize(number_intervention = n()) %>% 
  ggplot()+
  geom_sf(aes(fill=number_intervention), color="transparent")+
  theme_void()+
  scale_fill_gradientn(name="Number of interventions",
                       colors=col_palette[c(4, 1, 9)])+
  labs(title="Number of offenses causing death by PDQ per year")+
  facet_wrap(~DATE)

#Introduction
int_PDQ %>% 
  filter(CATEGORIE == "Introduction") %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(DATE != 2021) %>% 
  group_by(NOM_PDQ, DATE) %>% 
  summarize(number_intervention = n()) %>% 
  ggplot()+
  geom_sf(aes(fill=number_intervention), color="transparent")+
  theme_void()+
  scale_fill_gradientn(name="Number of interventions",
                       colors=col_palette[c(4, 1, 9)])+
  labs(title="Number of break-ins/thefts of firearms in residences by PDQ per year")+
  facet_wrap(~DATE)

#Make map set 2 by year with the facet_wrap (group by PDQ and year)
  int_PDQ %>% 
    mutate(DATE=year(DATE)) %>%
    filter(DATE != 2021) %>% 
    group_by(NOM_PDQ, DATE) %>% 
    summarize(number_intervention = n()) %>% 
    ggplot()+
    geom_sf(aes(fill=number_intervention))+
  
    scale_fill_gradient2(low="green",mid="blue", high="red")+
    theme_void()+
    labs(title="Sum of police interventions per PDQ per year")+
    labs(fill="Number of interventions")+
    facet_wrap(~DATE)
  
#Geom uses + signs, not pipes 
#Crimes per PDQ per capita

  