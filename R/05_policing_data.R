#Data policing report########################################################
load("output/geometry.Rdata")
load("output/int.Rdata")
load("output/CTALEXIA.Rdata")
source("R/01_startup.R")
install.packages("cancensus")
library(cancensus)
install.packages("dplyr")
library(dplyr)


#To set API key (do not need to run this every time)
set_api_key("<CensusMapper_e6c6d57ebc92d3b6b6d5abeb72951cfd>", install=TRUE)
readRenviron("~/.Renviron")

#Load CT with population data

CT <-
  cancensus::get_census(
    dataset = "CA16", regions = list(CSD = c("2466023")), level = "CT",
    vectors = c("v_CA16_4840", "v_CA16_4841", "v_CA16_4836", "v_CA16_4838",
                "v_CA16_4897", "v_CA16_4899", "v_CA16_4870", "v_CA16_4872",
                "v_CA16_4900", "v_CA16_3957", "v_CA16_3954", "v_CA16_3411", 
                "v_CA16_3405", "v_CA16_6698", "v_CA16_6692", "v_CA16_6725", 
                "v_CA16_6719", "v_CA16_4896", "v_CA16_4861", "v_CA16_4859",
                "v_CA16_2398", "v_CA16_2540", "v_CA16_3852", "v_CA16_3855",
                "v_CA16_5123", "v_CA16_5096"),
    geo_format = "sf") %>% 
  st_transform(32618) %>% 
  select(-c(Type, Households, `Adjusted Population (previous Census)`:CMA_UID, CSD_UID, PR_UID:`Area (sq km)`)) %>% 
  set_names(c("GeoUID", "population", "dwellings", "parent_condo", "condo", "parent_tenure", 
              "renter", "parent_thirty", "p_thirty_renter", "parent_repairs", "major_repairs",
              "median_rent", "average_value_dwellings", "unsuitable_housing", "parent_unsuitable",
              "vm", "parent_vm", "immigrants", "parent_immigrants", "mobility_one_year",
              "parent_mobility_one_year", "mobility_five_years", "parent_mobility_five_years", 
              "median_HH_income_AT", "p_low_income_AT", "parent_aboriginal",
              "aboriginal", "bachelor_above", "parent_education",
              "geometry")) %>% 
  mutate(p_condo = condo / parent_condo,
         p_renter = renter / parent_tenure, 
         p_repairs = major_repairs / parent_repairs,
         p_vm = vm/parent_vm,
         p_immigrants = immigrants/parent_immigrants,
         p_mobility_one_year = mobility_one_year/parent_mobility_one_year,
         p_mobility_five_years = mobility_five_years/parent_mobility_five_years,
         p_unsuitable = unsuitable_housing/parent_unsuitable,
         p_aboriginal = aboriginal/parent_aboriginal,
         p_bachelor_above = bachelor_above / parent_education) %>% 
  select(GeoUID, population, dwellings, renter, median_rent, average_value_dwellings, median_HH_income_AT, p_thirty_renter, p_condo, 
         p_renter, p_repairs, p_mobility_one_year, p_mobility_five_years, p_unsuitable,
         p_vm, p_immigrants, p_aboriginal, p_low_income_AT, p_bachelor_above) %>% 
  as_tibble() %>% 
  st_as_sf(agr = "constant")

#Load wes anderson colors ---------------------------------------------------

library(wesanderson)

pal <- wes_palette("Zissou1", 10, type = c("continuous"))


#Visualization of interventions ---------------------------------------

#Change the date to separate into year
int_PDQ %>% 
  mutate(DATE = year(DATE)) %>%
  filter(DATE != 2021) %>% 
  group_by(PDQ, CATEGORIE) %>% 
  summarize(number_intervention = n()) %>% 
  ggplot()+
  geom_sf(aes(fill=number_intervention), color="transparent")+
  theme_void()+
  scale_fill_gradientn(name="Number of interventions 2015-2020",
                       colors=col_palette[c(4, 1, 9)])+
  facet_wrap(~CATEGORIE)

#Map set by PDQ------------------------------------------------------------
#Import boundaries of PDQs 
PDQ_sf <-
  read_sf("data/Limites/Limites_PDQ.shp") %>% 
  st_transform(32618)


#Putting PDQ and crimes together (notice that this dataset has less data)
int_PDQ <- st_join(PDQ_sf,int %>% select(-PDQ))
#Putting PDQ, crimes and CT data together 
#(!!THIS SEEMS TO BE TOO BIG)
int_CT_PDQ <- st_join(CT, int_PDQ)

#Mefait by PDQ
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
#Mefaits per crimes per PDQ

total_interventions <-
  int_PDQ %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(DATE != 2021) %>% 
  group_by(NOM_PDQ,DATE) %>% 
  summarize(total_number_interventions = n()) 
  
total_mefait <-
  int_PDQ %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(DATE != 2021) %>% 
  filter(CATEGORIE == "Mefait") %>% 
  group_by(NOM_PDQ, DATE) %>%
  summarize(number_mefaits= n()) %>% 
  #HELP ABOVE HERE CLOÉ. All above works but I can't combine the two datasets---------------------------------
  #not working as of here
  cbind(total_interventions,total_mefait, by=c("NOM_PDQ","DATE"))
  st_combine
  Mefaits_per_crime_PDQ_year<-
  merge(total_interventions,total_mefait,
        by=c("NOM_PDQ","DATE"))
  ?st_join(total_interventions,total_mefait, by=c("NOM_PDQ","DATE"))
 %>% 
  ggplot()+
  geom_sf(aes(fill=number_intervention), color="transparent")+
  theme_void()+
  scale_fill_gradientn(name="Number of interventions",
                       colors=pal)+
  labs(title="Number of mefaits by PDQ per year")+
  facet_wrap(~DATE)
  
#Mefait by PDQ by population
#This is not working
int_CT_PDQ %>% 
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
#This is not working either
int_CT_PDQ %>% 
  filter(DATE != 2021) %>% 
  filter(population>50) %>% 
  filter(CATEGORIE == "Mefait") %>% 
  group_by(NOM_PDQ, population,DATE) %>% 
  summarize(n_int = n()) %>% 
  mutate(int_density = n_int/(population/100)) %>% 
  ggplot()+
  geom_sf(aes(fill=int_density),color=NA)+
  scale_fill_gradientn(name="Number of mischiefs by 100 people",
                       colors=col_palette[c(4,1,9)],
                       limits = c(0,2 ), oob = scales::squish)+
  theme_void()+facet_wrap(~DATE)

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
#Map set by CT
  #Merge CT and int data sets
  int_CT <- st_join(CT, int)
  #the first one is the geometry that you are keeping
  #Crimes per capita
    int_CT %>% 
      st_filter(CT) %>% 
    mutate(DATE = year(DATE)) %>% 
    filter(DATE != 2021) %>% 
      filter(population>50) %>% 
      filter(CATEGORIE == "Mefait") %>% 
      group_by(GeoUID, population,DATE) %>% 
      summarize(n_int = n()) %>% 
      mutate(int_density = n_int/(population/100)) %>% 
      ggplot()+
      geom_sf(aes(fill=int_density),color=NA)+
    scale_fill_gradientn(name="Number of mischiefs by 100 people",
                         colors=col_palette[c(4,1,9)],
                         limits = c(0,2 ), oob = scales::squish)+
    theme_void()+facet_wrap(~DATE)
  
  
#Bivariate maps-----------------------------------
  #Load libraries
  library(biscale)
  library(scales)
  library(ggplot2)
  library(cowplot)
  library(grid)
  library(gridGraphics)
  library(patchwork)
  
# Set the bivariate color palette
  
  bivar <- bi_pal_manual(val_1_1 = "#e8e8e8",
                         val_1_2 = "#b8d6be",
                         val_2_1 = "#b5c0da", 
                         val_2_2 = "#90b2b3", 
                         val_3_1 = "#6c83b5",  
                         val_3_2 = "#567994", 
                         val_1_3 = "#73ae80", 
                         val_2_3 = "#5a9178",
                         val_3_3 = "#2a5a5b", preview=FALSE)
  
  show_col(bivar)
  
  # Prepare the dataset to display in a bivariate choropleth map

  #Map 1: Median_HH_income_AT and Mefait
  
  Income_mefait <-
    bi_class(int_CT, x = median_HH_income_AT, y = CATEGORIE=Mefait, style = "quantile", dim = 3, 
             keep_factors = FALSE)
  
  # Plot for the bivariate choropleth map
  
  Bivarite_map_lowincome_aboriginal <- 
    ggplot() +
    geom_sf(data =Percentage_low_income, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
    bi_scale_fill(pal = bivar, dim = 3) +
    bi_theme()+
    theme(legend.position = "bottom")
  
  Bivarite_map_lowincome_aboriginal #to see your plot
  
  # Add bivariate legend
  
  bi_legend <- bi_legend(pal = bivar,
                         dim = 3,
                         xlab = "Percentage of low income",
                         ylab = "Percentage of aboriginal",
                         size = 8)
  plotlegend <- Bivarite_map_lowincome_aboriginal + inset_element(bi_legend, left = 0, bottom = 0.6, right = 0.4, top = 1)
  
  
  # Save in PDF in your output/figures folder to see the true sizes of your plot, ajust accordingly
  
  ggsave("output/figures/Bivarite_map_lowincome_aboriginal.pdf", plot = plotlegend, width = 8, 
         height = 5, units = "in", useDingbats = FALSE)