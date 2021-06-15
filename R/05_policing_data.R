#Data policing report########################################################
#load("output/geometry.Rdata")

load("output/int.Rdata")
#the above data has the wrong CT and DA datasets for me
#I need the ones loaded below
load("output/CTALEXIA.Rdata")
load("output/DA.Rdata")
source("R/01_startup.R")

#These packages are not necessary
install.packages("cancensus")
library(cancensus)
library(dplyr)

#To set API key (do not need to run this every time)
set_api_key("<CensusMapper_e6c6d57ebc92d3b6b6d5abeb72951cfd>", install=TRUE)
readRenviron("~/.Renviron")

# Import boundaries of PDQs ------------------------------------------------

PDQ_sf <-
  read_sf("data/Limites/Limites_PDQ.shp") %>% 
  st_transform(32618)

# Load CT with population data ---------------------------------------------

# CT <-
#   cancensus::get_census(
#     dataset = "CA16", regions = list(CSD = c("2466023")), level = "CT",
#     vectors = c("v_CA16_4840", "v_CA16_4841", "v_CA16_4836", "v_CA16_4838",
#                 "v_CA16_4897", "v_CA16_4899", "v_CA16_4870", "v_CA16_4872",
#                 "v_CA16_4900", "v_CA16_3957", "v_CA16_3954", "v_CA16_3411", 
#                 "v_CA16_3405", "v_CA16_6698", "v_CA16_6692", "v_CA16_6725", 
#                 "v_CA16_6719", "v_CA16_4896", "v_CA16_4861", "v_CA16_4859",
#                 "v_CA16_2398", "v_CA16_2540", "v_CA16_3852", "v_CA16_3855",
#                 "v_CA16_5123", "v_CA16_5096"),
#     geo_format = "sf") %>% 
#   st_transform(32618) %>% 
#   select(-c(Type, Households, `Adjusted Population (previous Census)`:CMA_UID, CSD_UID, PR_UID:`Area (sq km)`)) %>% 
#   set_names(c("GeoUID", "population", "dwellings", "parent_condo", "condo", "parent_tenure", 
#               "renter", "parent_thirty", "p_thirty_renter", "parent_repairs", "major_repairs",
#               "median_rent", "average_value_dwellings", "unsuitable_housing", "parent_unsuitable",
#               "vm", "parent_vm", "immigrants", "parent_immigrants", "mobility_one_year",
#               "parent_mobility_one_year", "mobility_five_years", "parent_mobility_five_years", 
#               "median_HH_income_AT", "p_low_income_AT", "parent_aboriginal",
#               "aboriginal", "bachelor_above", "parent_education",
#               "geometry")) %>% 
#   mutate(p_condo = condo / parent_condo,
#          p_renter = renter / parent_tenure, 
#          p_repairs = major_repairs / parent_repairs,
#          p_vm = vm/parent_vm,
#          p_immigrants = immigrants/parent_immigrants,
#          p_mobility_one_year = mobility_one_year/parent_mobility_one_year,
#          p_mobility_five_years = mobility_five_years/parent_mobility_five_years,
#          p_unsuitable = unsuitable_housing/parent_unsuitable,
#          p_aboriginal = aboriginal/parent_aboriginal,
#          p_bachelor_above = bachelor_above / parent_education) %>% 
#   select(GeoUID, population, dwellings, renter, median_rent, average_value_dwellings, median_HH_income_AT, p_thirty_renter, p_condo, 
#          p_renter, p_repairs, p_mobility_one_year, p_mobility_five_years, p_unsuitable,
#          p_vm, p_immigrants, p_aboriginal, p_low_income_AT, p_bachelor_above) %>% 
#   as_tibble() %>% 
#   st_as_sf(agr = "constant")


#Load wes anderson colors ---------------------------------------------------

library(wesanderson)
pal <- wes_palette("Zissou1", 10, type = c("continuous"))


#Visualization of interventions ---------------------------------------------
#Change the date to separate into year
#Map set by PDQ------------------------------------------------------------
#Number of interventions per category all years combined (2015-2020)
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

#Putting PDQ and crimes together (notice that this dataset has less data)
int_PDQ <- st_join(PDQ_sf,int %>% select(-PDQ))

#Mischief by PDQ
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
  labs(title="Number of mischief crimes by PDQ per year")+
  facet_wrap(~DATE)

#Mischief per crimes per PDQ
total_interventions <-
  int_PDQ %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(DATE != 2021) %>% 
  group_by(NOM_PDQ, DATE) %>% 
  summarize(total_number_interventions = n()) 

total_mefait <-
  int_PDQ %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(DATE != 2021) %>% 
  filter(CATEGORIE == "Mefait") %>% 
  group_by(NOM_PDQ, DATE) %>%
  summarize(number_mefaits= n()) 

total_mefait %>% 
  left_join(., st_drop_geometry(total_interventions), by = c("NOM_PDQ", "DATE")) %>% 
  mutate(mefait_prop = number_mefaits/total_number_interventions) %>% 
  ggplot()+
  geom_sf(aes(fill=mefait_prop), color="transparent")+
  theme_void()+
  scale_fill_gradientn(name="Percentage mischief per total crimes(%)",
                       colors=pal,
                       labels = scales::percent)+
  labs(title="Percentage of mischief out of total crimes by PDQ per year")+
  facet_wrap(~DATE)

#Ratio mischiefs per non-discretionary crimes per PDQ
total_interventions_minus_mefaits <-
  int_PDQ %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(DATE != 2021) %>% 
  filter(CATEGORIE != "Mefait") %>% 
  group_by(NOM_PDQ, DATE) %>% 
  summarize(total_number_interventions_minus_mefaits = n()) 

total_mefait %>% 
  left_join(., st_drop_geometry(total_interventions_minus_mefaits), by = c("NOM_PDQ", "DATE")) %>% 
  mutate(mefait_prop_non_discretionary = number_mefaits/total_number_interventions_minus_mefaits) %>% 
  ggplot()+
  geom_sf(aes(fill=mefait_prop_non_discretionary), color="transparent")+
  theme_void()+
  scale_fill_gradientn(name="Ratio\n(mischief/\nless discretionary crime)",
                       colors=pal)+
  labs(title="Ratio of mischief crimes to other less\ndiscretionary crimes by PDQ per year")+
  facet_wrap(~DATE)

#Mischiefs by PDQ per total mischiefs

total_mefait_island <-
  int_PDQ %>% 
  st_drop_geometry() %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(DATE != 2021) %>% 
  filter(CATEGORIE == "Mefait") %>% 
  group_by(DATE) %>%
  summarize(number_mefaits_island= n())

total_mefait %>% 
  full_join(., total_mefait_island, by = "DATE") %>% 
  mutate(share_mefait_PDQ_total_mefait = number_mefaits/number_mefaits_island) %>% 
  ggplot()+
  geom_sf(aes(fill=share_mefait_PDQ_total_mefait), color="transparent")+
  theme_void()+
  scale_fill_gradientn(name="Percentage mischief crimes by PDQ per total mischief crimes",
                       colors = pal,
                       labels = scales::percent)+
  labs(title="Share of mischief crimes per PDQ by PDQ per year")+
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

#Make map set by year with the facet_wrap (group by PDQ and year)
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

#Map set by CT------------------------------------------------------------------

#Merge CT and int data sets
int_CT <- st_join(CT, int)
#the first one in the join is the geometry that you are keeping

#Mischief crimes per capita CT level
int_CT %>% 
  st_filter(CT) %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(DATE != 2021) %>% 
  filter(population>50) %>% 
  filter(CATEGORIE == "Mefait") %>% 
  group_by(GeoUID, population,DATE) %>% 
  summarize(n_int_CT = n()) %>% 
  mutate(int_density_CT = n_int_CT/(population/100)) %>% 
  ggplot()+
  geom_sf(aes(fill=int_density),color=NA)+
  scale_fill_gradientn(name="Number of mischiefs by 100 people (CT level)",
                       colors=col_palette[c(4,1,9)],
                       limits = c(0,2 ), oob = scales::squish)+
  theme_void()+facet_wrap(~DATE)

#Map set by DA------------------------------------------------------------------

#Merge DA and int data sets
int_DA <- st_join(DA, int)

#Mischief crimes per capita
int_DA %>% 
  st_filter(DA) %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(DATE != 2021) %>% 
  filter(population>50) %>% 
  filter(CATEGORIE == "Mefait") %>% 
  group_by(GeoUID, population,DATE) %>% 
  summarize(n_int_DA = n()) %>% 
  mutate(int_density_DA = n_int_DA/(population/100)) %>% 
  ggplot()+
  geom_sf(aes(fill=int_density_DA),color=NA)+
  scale_fill_gradientn(name="Number of mischiefs by 100 people (DA level)",
                       colors=col_palette[c(4,1,9)],
                       limits = c(0,2 ), oob = scales::squish)+
  theme_void()+facet_wrap(~DATE)

#Bivariate maps------------------------------------------------------
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

#Bivariate maps CT level--------------------------------------------------------
#Map 1: Median_HH_income_AT and Mefait for 2020

CT_income_mefait <-
  int_CT %>% 
  st_filter(CT) %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(DATE == 2020) %>% 
  filter(median_HH_income_AT!="NA") %>% 
  filter(CATEGORIE == "Mefait") %>% 
  filter(population>50) %>% 
  group_by(GeoUID,CATEGORIE,DATE,median_HH_income_AT,population) %>% 
  summarize(n_int_CT_mefait_2 = n()) %>% 
  mutate(int_density_mefait_CT_2 = n_int_CT_mefait_2/(population/100)) %>%
  bi_class(x = int_density_mefait_CT_2, y = median_HH_income_AT, style = "quantile", dim = 3, 
           keep_factors = FALSE)

# Plot for the bivariate choropleth map

CT_bivarite_map_medianincome_mefaits <- 
  ggplot() +
  geom_sf(data =CT_income_mefait, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = bivar, dim = 3) +
  bi_theme()+
  theme(legend.position = "bottom")

# Add bivariate legend

bi_legend_medianincome_mefaits_CT <- bi_legend(pal = bivar,
                       dim = 3,
                       xlab = "Number of mischief crimes per 100 population",
                       ylab = "Median household income",
                       size = 8)
CT_final_bivarite_map_medianincome_mefaits <- CT_bivarite_map_medianincome_mefaits + inset_element(bi_legend_medianincome_mefaits_CT, left = 0, bottom = 0.6, right = 0.4, top = 1)

CT_final_bivarite_map_medianincome_mefaits  #to see your plot

# Save in PDF in your output/figures folder to see the true sizes of your plot, ajust accordingly

ggsave("output/figures/Alexia/CT_final_bivarite_map_medianincome_mefaitsperpopulation.pdf", plot = CT_final_bivarite_map_medianincome_mefaits , width = 8, 
       height = 5, units = "in", useDingbats = FALSE)
  
#Bivariate maps DA level-------------------------------------------------------- 

#Map 1: Median_HH_income_AT and Mefait for 2020

DA_income_mefait <-
  int_DA %>% 
  st_filter(DA) %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(DATE == 2020) %>% 
  filter(median_HH_income_AT!="NA") %>% 
  filter(CATEGORIE == "Mefait") %>% 
  #filter(population>50) %>% 
  group_by(GeoUID,CATEGORIE,DATE,median_HH_income_AT,population) %>% 
  summarize(n_int_DA_mefait_2 = n()) %>% 
  mutate(int_density_mefait_DA_2 = n_int_DA_mefait_2/(population/100)) %>%
  bi_class(x = int_density_mefait_DA_2, y = median_HH_income_AT, style = "quantile", dim = 3, 
           keep_factors = FALSE)

# Plot for the bivariate choropleth map

DA_bivarite_map_medianincome_mefaits <- 
  ggplot() +
  geom_sf(data =DA_income_mefait, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = bivar, dim = 3) +
  bi_theme()+
  theme(legend.position = "bottom")

# Add bivariate legend

bi_legend_medianincome_mefaits_DA <- bi_legend(pal = bivar,
                                               dim = 3,
                                               xlab = "Number of mischief crimes per 100 population",
                                               ylab = "Median household income",
                                               size = 8)
DA_final_bivarite_map_medianincome_mefaits <- DA_bivarite_map_medianincome_mefaits + inset_element(bi_legend_medianincome_mefaits_DA, left = 0, bottom = 0.6, right = 0.4, top = 1)

DA_final_bivarite_map_medianincome_mefaits  #to see your plot

# Save in PDF in your output/figures folder to see the true sizes of your plot, ajust accordingly

ggsave("output/figures/Alexia/DA_final_bivarite_map_medianincome_mefaitsperpopulation.pdf", plot = DA_final_bivarite_map_medianincome_mefaits , width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

#Map 2:Percentage immigrants and mefaits

DA_immigrant_mefait <-
  int_DA %>% 
  st_filter(DA) %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(DATE == 2020) %>% 
  filter(p_immigrants!="NA") %>% 
  filter(CATEGORIE == "Mefait") %>% 
  #filter(population>50) %>% 
  group_by(GeoUID,CATEGORIE,DATE,p_immigrants,population) %>% 
  summarize(n_int_mefait_immigrant_DA = n()) %>% 
  mutate(int_density_mefait_immigrant_DA = n_int_mefait_immigrant_DA/(population/100)) %>%
  bi_class(x = int_density_mefait_immigrant_DA, y = p_immigrants, style = "quantile", dim = 3, 
           keep_factors = FALSE)

# Plot for the bivariate choropleth map

DA_bivarite_map_immigrant_mefaits <- 
  ggplot() +
  geom_sf(data =DA_immigrant_mefait, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = bivar, dim = 3) +
  bi_theme()+
  theme(legend.position = "bottom")

# Add bivariate legend

bi_legend_immigrant_mefaits_DA <- bi_legend(pal = bivar,
                                               dim = 3,
                                               xlab = "Number of mischief crimes per 100 population",
                                               ylab = "Percentage immigrants",
                                               size = 8)
DA_final_bivarite_map_immigrant_mefaits <- DA_bivarite_map_immigrant_mefaits + inset_element(bi_legend_immigrant_mefaits_DA, left = 0, bottom = 0.6, right = 0.4, top = 1)

DA_final_bivarite_map_immigrant_mefaits  #to see your plot

# Save in PDF in your output/figures folder to see the true sizes of your plot, ajust accordingly

ggsave("output/figures/Alexia/DA_final_bivarite_map_immigrant_mefaitsperpopulation.pdf", plot = DA_final_bivarite_map_immigrant_mefaits , width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

#Map 3:Percentage immigrants and mefaits ratio

DA_total_mefait_immigrants_2019 <-
  int_DA %>% 
  st_drop_geometry() %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(DATE == 2019) %>% 
  filter(CATEGORIE == "Mefait") %>% 
  group_by(GeoUID,p_immigrants) %>%
  summarize(number_mefaits_2019= n())

DA_total_interventions_minus_mefaits_immigrants_2019 <-
  int_DA %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(DATE == 2019) %>% 
  filter(CATEGORIE != "Mefait") %>% 
  group_by(GeoUID, p_immigrants) %>% 
  summarize(total_number_interventions_minus_mefaits_2019 = n()) 

DA_immigrant_mefait_ratio_2019 <-
DA_total_mefait_immigrants_2019 %>% 
  full_join(., DA_total_interventions_minus_mefaits_immigrants_2019, by = c("GeoUID", "p_immigrants")) %>% 
 mutate(DA_share_mefait_total_intervention_2019 
         = number_mefaits_2019/total_number_interventions_minus_mefaits_2019) 
  
DA_immigrant_mefait_ratio_2019_2 <-
DA_immigrant_mefait_ratio_2019 %>% 
  filter(DA_share_mefait_total_intervention_2019!="NA") %>% 
filter(p_immigrants!="NA") %>% 
  bi_class(x = DA_share_mefait_total_intervention_2019, y = p_immigrants, style = "quantile", dim = 3, 
           keep_factors = FALSE)

# Plot for the bivariate choropleth map

DA_bivarite_map_immigrant_mefaits_ratio_2019 <- 
  ggplot() +
  geom_sf(data =DA_immigrant_mefait_ratio_2019_2, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = bivar, dim = 3) +
  bi_theme()+
  theme(legend.position = "bottom")+
  aes(geometry = geometry)

# Add bivariate legend

DA_bi_legend_immigrant_mefaits_ratio_2019 <- bi_legend(pal = bivar,
                                            dim = 3,
                                            xlab = "Ratio of mischief crimes to other non-discretionary crimes",
                                            ylab = "Percentage immigrants",
                                            size = 8)
DA_final_bivarite_map_immigrant_mefaits_ratio_2019 <- DA_bivarite_map_immigrant_mefaits_ratio_2019 + inset_element(DA_bi_legend_immigrant_mefaits_ratio_2019, left = 0, bottom = 0.6, right = 0.4, top = 1)

DA_final_bivarite_map_immigrant_mefaits_ratio_2019 #to see your plot

# Save in PDF in your output/figures folder to see the true sizes of your plot, ajust accordingly

ggsave("output/figures/Alexia/DA_final_bivarite_map_immigrant_mefaits_ratio_2019.pdf", plot = DA_final_bivarite_map_immigrant_mefaits_ratio_2019, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

#Map 4:Percentage unsuitable and mefaits ratio
DA_total_mefait_unsuitable_2019 <-
  int_DA %>% 
  st_drop_geometry() %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(DATE == 2019) %>% 
  filter(CATEGORIE == "Mefait") %>% 
  group_by(GeoUID,p_unsuitable) %>%
  #st_buffer(0) %>% 
  summarize(number_mefaits_2019= n())

DA_total_interventions_minus_mefaits_unsuitable_2019 <-
  int_DA %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(CATEGORIE!= "Mefait") %>% 
  filter(DATE == 2019) %>% 
  group_by(GeoUID) %>% 
  summarize(total_number_interventions_minus_mefaits_2019 = n()) 

DA_unsuitable_mefait_ratio_2019 <-
  DA_total_mefait_unsuitable_2019 %>% 
  full_join(., DA_total_interventions_minus_mefaits_unsuitable_2019, by = c("GeoUID", "p_unsuitable")) %>% 
  mutate(DA_share_mefait_total_intervention_2019 
         = number_mefaits_2019/total_number_interventions_minus_mefaits_2019) 

DA_unsuitable_mefait_ratio_2019_2 <-
  DA_unsuitable_mefait_ratio_2019 %>% 
  filter(DA_share_mefait_total_intervention_2019!="NA") %>% 
  filter(p_unsuitable!="NA") %>% 
  bi_class(x = DA_share_mefait_total_intervention_2019, y = p_unsuitable, style = "quantile", dim = 3, 
           keep_factors = FALSE)

# Plot for the bivariate choropleth map

DA_bivarite_map_unsuitable_mefaits_ratio_2019 <- 
  ggplot() +
  geom_sf(data =DA_unsuitable_mefait_ratio_2019_2, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = bivar, dim = 3) +
  bi_theme()+
  theme(legend.position = "bottom")+
  aes(geometry = geometry)

# Add bivariate legend

DA_bi_legend_unsuitable_mefaits_ratio_2019 <- bi_legend(pal = bivar,
                                                       dim = 3,
                                                       xlab = "Ratio of mischief crimes to other non-discretionary crimes",
                                                       ylab = "Percentage unsuitable housing",
                                                       size = 8)
DA_final_bivarite_map_unsuitable_mefaits_ratio_2019 <- DA_bivarite_map_unsuitable_mefaits_ratio_2019 + inset_element(DA_bi_legend_unsuitable_mefaits_ratio_2019, left = 0, bottom = 0.6, right = 0.4, top = 1)

DA_final_bivarite_map_unsuitable_mefaits_ratio_2019 #to see your plot

# Save in PDF in your output/figures folder to see the true sizes of your plot, ajust accordingly

ggsave("output/figures/Alexia/DA_final_bivarite_map_unsuitable_mefaits_ratio_2019.pdf", plot = DA_final_bivarite_map_unsuitable_mefaits_ratio_2019, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

#Scatter plots------------------------------------------------------------------

install.packages("car")
library(car)

#DA level: unsuitable + mischief ratio
#not working hehehe
scatterplot(p_unsuitable ~ DA_share_mefait_total_intervention_2019 |data=DA_unsuitable_mefait_ratio_2019,
            xlab="Percentage unsuitable", 
            ylab="Ratio of mischief crimes to non discretionary crimes",
            main="DA scatter plot I: Mischief crimes and housing unsuitability",
            labels=row.names(mtcars))
#take 2, sort of works
attach(DA_unsuitable_mefait_ratio_2019)
DA_scatterplot_share_mefait_total_intervention_2019 <-
  plot(p_unsuitable, DA_share_mefait_total_intervention_2019, main="Scatterplot Example",
     xlab="Mischief crimes per non-discretionary crimes", ylab="Percentage unsuitable housing", pch=19)

#take 3
DA_unsuitable_mefait_ratio_2019_scatterplot_1 <-
DA_unsuitable_mefait_ratio_2019_2 %>% 
  ggplot()+
geom_point(mapping = aes(x=number_mefaits_2019, y=DA_share_mefait_total_intervention_2019))

DA_unsuitable_mefait_ratio_2019_scatterplot_2 <-
  DA_unsuitable_mefait_ratio_2019_2 %>% 
  filter(DA_share_mefait_total_intervention_2019<=6) %>% 
  filter(p_unsuitable<=0.5) %>% 
  ggplot()+
  geom_point(mapping = aes(x=DA_share_mefait_total_intervention_2019, y=p_unsuitable))

#DA level: p_immigrants and mischief ratio
DA_immigrant_mefait_ratio_2019_scatterplot <-
  DA_immigrant_mefait_ratio_2019_2 %>% 
  filter(DA_share_mefait_total_intervention_2019<=6) %>% 
  filter(p_immigrants<=0.75) %>% 
  ggplot()+
  geom_point(mapping = aes(x=DA_share_mefait_total_intervention_2019, y=p_immigrants))

DA_immigrant_mefait_ratio_2019_scatterplot_2 <-
  DA_immigrant_mefait_ratio_2019_2 %>% 
  filter(DA_share_mefait_total_intervention_2019<=2) %>% 
  filter(p_immigrants<=0.75) %>% 
  ggplot()+
  geom_point(mapping = aes(x=DA_share_mefait_total_intervention_2019, y=p_immigrants))

#CT level
#CT and mischief per 100 ppl
#variables to keep: GeoUID, population, median_HH_income_AT,p_unsuitable
#p_immigrants, p_aboriginal, p_low_income_AT, geometry <MULTIPOLYGON [m]>
#CATEGORIE <chr>,DATE <chr>
#no need to filter for NA in ggplot

int_CT_plots_1 <-
  int_CT %>% 
  st_filter(CT) %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(DATE==2019) %>% 
    group_by(GeoUID) %>% 
  summarize(n_int_CT_categorie=n()) 

int_CT_plots_2 <-
  int_CT %>% 
  st_filter(CT) %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(DATE==2019) %>% 
  filter(CATEGORIE=="Mefait") %>% 
  group_by(GeoUID) %>% 
  summarize(n_int_CT_mefait=n()) 

int_CT_plots_3 <-
  full_join(st_drop_geometry(int_CT_plots_1),st_drop_geometry(int_CT_plots_2), by="GeoUID") %>% 
  mutate(mefait_share=n_int_CT_mefait/n_int_CT_categorie)
  
int_CT_plots_4<-
left_join(int_CT_plots_3, CT, by="GeoUID") %>% 
  filter(population>50) %>% 
  mutate(n_int_CT_mefait_per100ppl=n_int_CT_mefait/(population/100))

#Now you can play with int_CT_plots_4 3  
ggplot(data=int_CT_plots_4)+
  geom_point(mapping = aes(x=mefait_share, y=p_low_income_AT))+
  xlim(c(0,0.5))

#nothing much there
#do it with mefait_share and n_int_CT_mefait_per100ppl
    
#CT and mischief per total interventions ratio