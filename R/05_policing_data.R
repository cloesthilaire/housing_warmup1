#Data policing report########################################################
source("R/01_startup.R")
load("output/geometry.Rdata")
load("output/int.Rdata")
#Load Sf library
library(sf)
library(tidyverse)

#Load stringer library
library(stringr)

#Load wes anderson colors
install.packages("wesanderson")
library(wesanderson)
names(wes_palette())

#Import boundaries of PDQs-------------------------------------------------------
PDQ_sf <-
  read_sf("data/Limites/Limites_PDQ.shp")
#Transforming into Montreal spacing (32618)
PDQ_sf <- 
  PDQ_sf %>% 
  st_transform(32618)
#Import SPVM policing
int<-read_csv("data/interventionscitoyendo.csv")

#Change geographical reference
int<-int %>% 
  st_as_sf(coords=c("LONGITUDE", "LATITUDE"),crs=4326) %>% 
  st_transform(32618) %>% 
  select(-X, -Y)

#Change category names to remove accents/fix accents (CLOÉ WILL SEND ME)
int %>% 
  mutate(CATEGORIE = str_replace(CATEGORIE, "\xe9", "e"),
         CATEGORIE = str_replace(CATEGORIE, "\xe0", "a"))

#Putting PDQ and crimes together (notice that this dataset has less data)
int_PDQ<- st_join(PDQ_sf,int)

#Change the date to separate into year
int_PDQ %>% 
  mutate(DATE=year(DATE)) %>%
  mutate(CATEGORIE = str_replace(CATEGORIE, "\xe9", "e"),
         CATEGORIE = str_replace(CATEGORIE, "\xe0", "a")) %>% 
  group_by(PDQ.x,CATEGORIE) %>% 
  summarize(number_intervention=sum(n())) %>% 
  ggplot()+
  geom_sf(aes(fill=number_intervention))+facet_wrap(~CATEGORIE)

#Make map set (1 by category of intervention)
#Mefait
int_PDQ %>% 
  mutate(DATE=year(DATE)) %>% 
  filter(CATEGORIE == "Mefait")%>% 
  group_by(DATE) %>% 
  summarize(number_intervention=n()) %>% 
  ggplot()+
  geom_sf(aes(fill=number_intervention))+
    facet_wrap(~DATE)+
    scale_fill_gradient2(low="green",mid="blue", high="red")+
    theme_void()+
    labs(title="Sum of police interventions per PDQ per category of intervention")+
    labs(fill="Number of interventions")

#Facet_wrap crée un graph par catégorie assignée

#Vol de vehicule a moteur
int_PDQ %>% 
  filter(.preserve = "Vol de vehicule a moteur")%>% 
  group_by("Vol de vehicule a moteur",PDQ.x) %>% 
  summarize(number_intervention=sum(n())) %>% 
  ggplot()+
  geom_sf(aes(fill=number_intervention))+
  facet_wrap(~"Vol de vehicule a moteur")+
  scale_fill_gradient2(low="green",mid="blue", high="red")+
  theme_void()+
  labs(title="Sum of police interventions per PDQ per category of intervention")+
  labs(fill="Number of interventions")

#Vols qualifies
int_PDQ %>% 
  filter(.preserve = "Vols qualifies")%>% 
  group_by("Vols qualifies",PDQ.x) %>% 
  summarize(number_intervention=sum(n())) %>% 
  ggplot()+
  geom_sf(aes(fill=number_intervention))+
  facet_wrap(~"Vols qualifies")+
  scale_fill_gradient2(low="green",mid="blue", high="red")+
  theme_void()+
  labs(title="Sum of police interventions per PDQ per category of intervention")+
  labs(fill="Number of interventions")

#Make map set 2 by year with the facet_wrap (group by PDQ and year)
  int_PDQ %>% 
    mutate(DATE=year(DATE)) %>%
    group_by(PDQ.x,DATE) %>% 
    summarize(number_intervention=sum(n())) %>% 
    ggplot()+
    geom_sf(aes(fill=number_intervention))+
    facet_wrap(~DATE)+
    scale_fill_gradient2(low="green",mid="blue", high="red")+
    theme_void()+
    labs(title="Sum of police interventions per PDQ per year")+
    labs(fill="Number of interventions")
#Geom uses + signs, not pikes 
#Crimes per PDQ per capita

  