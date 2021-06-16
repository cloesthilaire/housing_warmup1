# Data policing report########################################################
# Background to the code-------------------------------------------------------------------

# This code creates maps and scatter plots based on the crimes data from Donnees 
# Montreal from 2015 until 01.06.2021. There are three spatial levels to the data,
# the poste de quartier (PDQ or precinct), the census track (CT) and the 
# dissemination area (DA).The year 2019 was used for the interventions data, as 2020
# was an unusual year due to the pandemic. 


# French to English translations-------------------------------------------------

# Méfaits -> mischief crimes
# PDQ -> precinct 

# Abbreviations------------------------------------------------------------------

# PDQ = Poste de quartier -> precinct
# Census track = CT
# Dissemination Area = DA
# Interventions = INT

# Loading data and packages------------------------------------------------------

# Loading the crimes data, which we name "interventions"

load("output/int.Rdata")

#the above data has the wrong CT and DA datasets
#Load the CT and DA datasets

load("output/CTALEXIA.Rdata")
load("output/DA.Rdata")
source("R/01_startup.R")

#Load wes anderson colors 

library(wesanderson)
pal <- wes_palette("Zissou1", 10, type = c("continuous"))

# Import boundaries of PDQs 

PDQ_sf <-
  read_sf("data/Limites/Limites_PDQ.shp") %>% 
  st_transform(32618)

#To set API key-----------------------------------------------------------------
#Do not need to run this every time
set_api_key("<CensusMapper_e6c6d57ebc92d3b6b6d5abeb72951cfd>", install=TRUE)
readRenviron("~/.Renviron")

#Visualization of interventions ------------------------------------------------
#Map set by PDQ-----------------------------------------------------------------

#Step 1: Putting PDQ and crimes together (notice that this dataset has less data)
int_PDQ <- st_join(PDQ_sf,int %>% select(-PDQ))

#Map 1: Number of interventions by PDQ per category all years combined (2015-2020)

int_PDQ %>% 
  mutate(DATE = year(DATE)) %>%
  filter(DATE != 2021) %>% 
  group_by(PDQ, CATEGORIE) %>% 
  summarize(PDQ_number_intervention = n()) %>% 
  ggplot()+
  geom_sf(aes(fill=PDQ_total_number_intervention), color="transparent")+
  theme_void()+
  scale_fill_gradientn(name="Number of crimes 2015-2020",
                       colors=col_palette[c(4, 1, 9)])+
  facet_wrap(~CATEGORIE)

#Map 2: Mischief by PDQ per year (2015-2020)

int_PDQ %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(DATE != 2021) %>% 
  filter(CATEGORIE == "Mefait") %>% 
  group_by(NOM_PDQ, DATE) %>%
  summarize(PDQ_number_mischief = n()) %>% 
  ggplot()+
  geom_sf(aes(fill=PDQ_number_mischief), color="transparent")+
  theme_void()+
  scale_fill_gradientn(name="Number of crimes",
                       colors=pal)+
  labs(title="Number of mischief crimes by PDQ per year")+
  facet_wrap(~DATE)

#Map 3: Mischief per crimes per PDQ

PDQ_total_interventions <-
  int_PDQ %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(DATE != 2021) %>% 
  group_by(NOM_PDQ, DATE) %>% 
  summarize(PDQ_total_number_interventions_per_year = n()) 

PDQ_total_mischief <-
  int_PDQ %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(DATE != 2021) %>% 
  filter(CATEGORIE == "Mefait") %>% 
  group_by(NOM_PDQ, DATE) %>%
  summarize(PDQ_number_mischiefs_per_year= n()) 

PDQ_total_mischief %>% 
  left_join(., st_drop_geometry(PDQ_total_interventions), 
            by = c("NOM_PDQ", "DATE")) %>% 
  mutate(mefait_prop = PDQ_number_mischiefs_per_year/
           PDQ_total_number_interventions_per_year) %>% 
  ggplot()+
  geom_sf(aes(fill=mefait_prop), color="transparent")+
  theme_void()+
  scale_fill_gradientn(name="Percentage mischief per total crimes (%)",
                       colors=pal,
                       labels = scales::percent)+
  labs(title="Percentage of mischief out of total crimes by PDQ per year")+
  facet_wrap(~DATE)

#Map 4: Ratio mischiefs per non-discretionary crimes per PDQ

PDQ_total_interventions_minus_mischiefs <-
  int_PDQ %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(DATE != 2021) %>% 
  filter(CATEGORIE != "Mefait") %>% 
  group_by(NOM_PDQ, DATE) %>% 
  summarize(PDQ_total_number_interventions_minus_mischiefs_per_year = n()) 

PDQ_total_mischief %>% 
  left_join(., st_drop_geometry(PDQ_total_interventions_minus_mischief), 
            by = c("NOM_PDQ", "DATE")) %>% 
  mutate(mischief_prop_non_discretionary = PDQ_number_mischiefs_per_year/
           PDQ_total_number_interventions_minus_mischiefs_per_year) %>% 
  ggplot()+
  geom_sf(aes(fill=mischief_prop_non_discretionary), color="transparent")+
  theme_void()+
  scale_fill_gradientn(name="Ratio of mischief to less discretionary crime)",
                       colors=pal)+
  labs(title="Ratio of mischief crimes to other less discretionary crimes 
       by PDQ per year")+
  facet_wrap(~DATE)

#Map 5: Mischiefs by PDQ per total mischiefs

PDQ_total_mischief_island <-
  int_PDQ %>% 
  st_drop_geometry() %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(DATE != 2021) %>% 
  filter(CATEGORIE == "Mefait") %>% 
  group_by(DATE) %>%
  summarize(PDQ_number_mischief_island= n())

total_mefait %>% 
  full_join(., PDQ_total_mefait_island, by = "DATE") %>% 
  mutate(PDQ_share_mischief_total_mischief = PDQ_number_mischiefs_per_year/
           PDQ_number_mischief_island) %>% 
  ggplot()+
  geom_sf(aes(fill=PDQ_share_mischief_total_mischief), color="transparent")+
  theme_void()+
  scale_fill_gradientn(name="Percentage mischief crimes by PDQ per total mischief crimes",
                       colors = pal,
                       labels = scales::percent)+
  labs(title="Share of mischief crimes per PDQ by PDQ per year")+
  facet_wrap(~DATE)

#Map set per category of crime
#Map 6.1: Vol de vehicule a moteur -> Theft of motor vehicle

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
  labs(title="Number of motor vehicule theft by PDQ per year")+
  facet_wrap(~DATE)

#Map 6.2: Vols qualifies -> Skilled thefts

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

#Map 6.3: Vol dans / sur vehicule a moteur -> Thefts of/in motor vehicles

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

#Map 6.4: Infractions entrainant la mort (NEED TO FIX BORDERS)

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

#Map 6.5: Introduction -> Break-ins/thefts of firearms in residences

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

#Map 7: Map set by year and PDQ (facet group by PDQ and year)

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

#Map set by CT------------------------------------------------------------------

#Step 1: Merge CT and int data sets
int_CT <- st_join(CT, int)

#the first one in the join is the geometry that you are keeping

#Map 1: Mischief crimes per capita CT level
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

#Step 1: Merge DA and int data sets
int_DA <- st_join(DA, int)

#Map 1: Mischief crimes per capita
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

#Bivariate maps-----------------------------------------------------------------

#Step 1: Load libraries
library(biscale)
library(scales)
library(ggplot2)
library(cowplot)
library(grid)
library(gridGraphics)
library(patchwork)

#Step 2: Set the bivariate color palette

bivar <- bi_pal_manual(val_1_1 = "#e8e8e8",
                       val_1_2 = "#b8d6be",
                       val_2_1 = "#b5c0da", 
                       val_2_2 = "#90b2b3", 
                       val_3_1 = "#6c83b5",  
                       val_3_2 = "#567994", 
                       val_1_3 = "#73ae80", 
                       val_2_3 = "#5a9178",
                       val_3_3 = "#2a5a5b", preview=FALSE)

show_col(bivar) #to show the bivar color palette

#Step 3: Prepare the dataset to display in a bivariate choropleth map

# Bivariate maps CT level--------------------------------------------------------

# Map 1: Median income (Median_HH_income_AT) and Mischief for 2019

# Make new dataset

CT_income_mischief_2019 <-
  int_CT %>% 
  st_filter(CT) %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(DATE == 2019) %>% 
  filter(median_HH_income_AT!="NA") %>% 
  filter(CATEGORIE == "Mefait") %>% 
  filter(population>50) %>% 
  group_by(GeoUID,median_HH_income_AT,population) %>% 
  summarize(CT_n_mischief_2019 = n()) %>% 
  mutate(CT_density_mischief_2019 = CT_n_mischief_2019/(population/100)) %>%
  bi_class(x = CT_density_mischief_2019, y = median_HH_income_AT, style = "quantile", dim = 3, 
           keep_factors = FALSE)

# Plot for the bivariate choropleth map

CT_bivarite_map_medianincome_mischief_2019 <- 
  ggplot() +
  geom_sf(data =CT_income_mischief_2019, mapping = aes(fill = bi_class), 
          color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = bivar, dim = 3) +
  bi_theme()+
  theme(legend.position = "bottom")

# Add bivariate legend

CT_bi_legend_medianincome_mischiefs_2019 <- bi_legend(pal = bivar,
                       dim = 3,
                       xlab = "Number of mischief crimes per 100 population",
                       ylab = "Median household income",
                       size = 8)

CT_final_bivarite_map_medianincome_mischief_2019 <- 
  CT_bivarite_map_medianincome_mischief_2019 + 
  inset_element(CT_bi_legend_medianincome_mischiefs_2019, left = 0, bottom = 0.6, right = 0.4, top = 1)

CT_final_bivarite_map_medianincome_mischief_2019  # to see your plot

# Save in PDF in your output/figures folder to see the true sizes of your plot, ajust accordingly

ggsave("output/figures/Alexia/CT_bivarite_map_medianincome_mischiefsperpopulation_2019.pdf", 
       plot = CT_final_bivarite_map_medianincome_mischief_2019, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

# Map 2: Percentage low income  and Mischief share for 2019
#IT STARTS HERE CLOÉ!-----------------------------------------------------------
# Make new dataset
# The CT_int_plots_4 dataframe is made in the scatterplots section

CT_lowincome_mischief_share_2019 <-
  CT_int_plots_4 %>%
  bi_class(x = p_low_income_AT, y =CT_mischief_share_2019 , style = "quantile", dim = 3, 
           keep_factors = FALSE)

# Plot for the bivariate choropleth map

CT_bivarite_map_lowincome_mischief_share_2019 <- 
  ggplot() +
  geom_sf(data =CT_lowincome_mischief_share_2019, mapping = aes(fill = bi_class), 
          color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = bivar, dim = 3) +
  bi_theme()+
  theme(legend.position = "bottom")

# Add bivariate legend

CT_bi_legend_lowincome_mischiefs_share_2019 <- bi_legend(pal = bivar,
                                                      dim = 3,
                                                      xlab = "Percentage of mischief crimes out of all crimes",
                                                      ylab = "Percentage of low income households",
                                                      size = 8)

CT_final_bivarite_map_lowincome_mischief_share_2019 <- 
  CT_bivarite_map_lowincome_mischief_share_2019 + 
  inset_element(CT_bi_legend_lowincome_mischiefs_share_2019, left = 0, bottom = 0.6, right = 0.4, top = 1)

CT_final_bivarite_map_lowincome_mischief_share_2019  # to see your plot

# Save in PDF in your output/figures folder to see the true sizes of your plot, ajust accordingly

ggsave("output/figures/Alexia/CT_bivarite_map_medianincome_mischiefsperpopulation_2019.pdf", 
       plot = CT_final_bivarite_map_medianincome_mischief_2019, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)
  
# Bivariate maps DA level-------------------------------------------------------- 

# Map 1: Median income (Median_HH_income_AT) and Mischief for 2019

# Make new dataset

DA_income_mischief_2019 <-
  int_DA %>% 
  st_filter(DA) %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(DATE == 2019) %>% 
  filter(median_HH_income_AT!="NA") %>% 
  filter(CATEGORIE == "Mefait") %>% 
  #filter(population>50) %>% 
  group_by(GeoUID,median_HH_income_AT,population) %>% 
  summarize(DA_n_mischief_2019 = n()) %>% 
  mutate(DA_density_mischief_2019 = DA_n_mischief_2019/(population/100)) %>%
  bi_class(x = DA_density_mischief_2019, y = median_HH_income_AT, style = "quantile", dim = 3, 
           keep_factors = FALSE)

# Plot for the bivariate choropleth map

DA_bivarite_map_medianincome_mischiefs_2019 <- 
  ggplot() +
  geom_sf(data = DA_income_mischief_2019, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = bivar, dim = 3) +
  bi_theme()+
  theme(legend.position = "bottom")

# Add bivariate legend

DA_bi_legend_medianincome_mischiefs_2019 <- bi_legend(pal = bivar,
                                               dim = 3,
                                               xlab = "Number of mischief crimes per 100 population",
                                               ylab = "Median household income",
                                               size = 8)

DA_final_bivarite_map_medianincome_mischiefs_2019 <- 
  DA_bivarite_map_medianincome_mischiefs_2019 + 
  inset_element(DA_bi_legend_medianincome_mischiefs_2019, left = 0, bottom = 0.6, right = 0.4, top = 1)

DA_final_bivarite_map_medianincome_mischiefs_2019 #to see your plot

# Save in PDF in your output/figures folder to see the true sizes of your plot, ajust accordingly

ggsave("output/figures/Alexia/DA_bivarite_map_medianincome_mischiefsperpopulation_2019.pdf", plot = DA_final_bivarite_map_medianincome_mischiefs_2019, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

# Map 2:Percentage immigrants (p_immigrants) and mischief crimes

DA_immigrant_mischief_2019 <-
  int_DA %>% 
  st_filter(DA) %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(DATE == 2019) %>% 
  filter(p_immigrants!="NA") %>% 
  filter(CATEGORIE == "Mefait") %>% 
  #filter(population>50) %>% 
  group_by(GeoUID,p_immigrants,population) %>% 
  summarize(DA_n_mischief_immigrant_2019 = n()) %>% 
  mutate(DA_density_mischief_immigrant_2019 = DA_n_mischief_immigrant_2019/(population/100)) %>%
  bi_class(x = DA_density_mischief_immigrant_2019, y = p_immigrants, style = "quantile", dim = 3, 
           keep_factors = FALSE)

# Plot for the bivariate choropleth map

DA_bivarite_map_immigrant_mischiefs_2019 <- 
  ggplot() +
  geom_sf(data = DA_immigrant_mischief_2019, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = bivar, dim = 3) +
  bi_theme()+
  theme(legend.position = "bottom")

# Add bivariate legend

DA_bi_legend_immigrant_mischiefs_2019 <- bi_legend(pal = bivar,
                                               dim = 3,
                                               xlab = "Number of mischief crimes per 100 population",
                                               ylab = "Percentage immigrants",
                                               size = 8)
DA_final_bivarite_map_immigrant_mischiefsper100ppl <- 
  DA_bivarite_map_immigrant_mischiefs_2019 + 
  inset_element(DA_bi_legend_immigrant_mischiefs_2019, left = 0, bottom = 0.6, right = 0.4, top = 1)

DA_final_bivarite_map_immigrant_mischiefsper100ppl  #to see your plot

# Save in PDF in your output/figures folder to see the true sizes of your plot, ajust accordingly

ggsave("output/figures/Alexia/DA_bivarite_map_immigrant_mischiefs_per100ppl_2019.pdf", plot = DA_final_bivarite_map_immigrant_mischiefsper100ppl, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

# Map 3:Percentage immigrants and mischief ratio

DA_total_mischief_immigrants_2019 <-
  int_DA %>% 
  st_drop_geometry() %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(DATE == 2019) %>% 
  filter(CATEGORIE == "Mefait") %>% 
  group_by(GeoUID) %>%
  summarize(DA_number_mischief_2019= n())

DA_total_interventions_minus_mischiefs_2019 <-
  int_DA %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(DATE == 2019) %>% 
  filter(CATEGORIE != "Mefait") %>% 
  group_by(GeoUID) %>% 
  summarize(DA_total_number_interventions_minus_mischiefs_2019 = n()) 

DA_immigrant_mischiefs_ratio_2019 <-
DA_total_mischiefs_immigrants_2019 %>% 
  full_join(., DA_total_interventions_minus_mischiefs_2019, by = c("GeoUID", "p_immigrants")) %>% 
 mutate(DA_share_mischiefs_total_intervention_2019 
         = DA_number_mischief_2019/total_number_interventions_minus_mischiefs_2019) %>% 
  filter(DA_share_mefait_total_intervention_2019!="NA") %>% 
  filter(p_immigrants!="NA") %>% 
  bi_class(x = DA_share_mefait_total_intervention_2019, y = p_immigrants, style = "quantile", dim = 3, 
           keep_factors = FALSE)
 
# Plot for the bivariate choropleth map

DA_bivarite_map_immigrant_mischiefs_ratio_2019 <- 
  ggplot() +
  geom_sf(data =DA_immigrant_mischiefs_ratio_2019, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = bivar, dim = 3) +
  bi_theme()+
  theme(legend.position = "bottom")+
  aes(geometry = geometry)

# Add bivariate legend

DA_bi_legend_immigrant_mischiefs_ratio_2019 <- bi_legend(pal = bivar,
                                            dim = 3,
                                            xlab = "Ratio of mischief crimes to other non-discretionary crimes",
                                            ylab = "Percentage immigrants",
                                            size = 8)

DA_final_bivarite_map_immigrant_mischiefs_ratio_2019 <- 
  DA_bivarite_map_immigrant_mischiefs_ratio_2019 + 
  inset_element(DA_bi_legend_immigrant_mischiefs_ratio_2019, left = 0, bottom = 0.6, right = 0.4, top = 1)

DA_final_bivarite_map_immigrant_mischiefs_ratio_2019 #to see your plot

# Save in PDF in your output/figures folder to see the true sizes of your plot, ajust accordingly

ggsave("output/figures/Alexia/DA_bivarite_map_immigrant_mischiefs_ratio_2019.pdf", plot = DA_final_bivarite_map_immigrant_mischiefs_ratio_2019, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

# Map 4:Percentage unsuitable (p_unsuitable) and mischiefs ratio for 2019

DA_total_mischiefs_unsuitable_2019 <-
  int_DA %>% 
  st_drop_geometry() %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(DATE == 2019) %>% 
  filter(CATEGORIE == "Mefait") %>% 
  group_by(GeoUID,p_unsuitable) %>%
  #st_buffer(0) %>% 
  summarize(DA_number_mischiefs_2019 = n())

DA_total_interventions_minus_mischiefs_2019 <-
  int_DA %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(DATE == 2019) %>% 
  filter(CATEGORIE != "Mefait") %>% 
  group_by(GeoUID) %>% 
  summarize(DA_total_number_interventions_minus_mischiefs_2019 = n()) 

DA_unsuitable_mischief_ratio_2019 <-
  DA_total_mischiefs_unsuitable_2019 %>% 
  full_join(., DA_total_interventions_minus_mischiefs_2019, by = "GeoUID") %>% 
  mutate(DA_share_mischiefs_total_interventions_2019 
         = DA_number_mischiefs_2019/DA_total_number_interventions_minus_mischiefs_2019) %>% 
  #filter(DA_share_mefait_total_intervention_2019!="NA") %>% 
  #filter(p_unsuitable!="NA") %>% 
  bi_class(x = DA_share_mefait_total_intervention_2019, y = p_unsuitable, style = "quantile", dim = 3, 
           keep_factors = FALSE)

# Plot for the bivariate choropleth map

DA_bivarite_map_unsuitable_mefaits_ratio_2019 <- 
  ggplot() +
  geom_sf(data = DA_unsuitable_mischief_ratio_2019, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = bivar, dim = 3) +
  bi_theme()+
  theme(legend.position = "bottom")+
  aes(geometry = geometry)

# Add bivariate legend

DA_bi_legend_unsuitable_mischief_ratio_2019 <- bi_legend(pal = bivar,
                                                       dim = 3,
                                                       xlab = "Ratio of mischief crimes to other non-discretionary crimes",
                                                       ylab = "Percentage unsuitable housing",
                                                       size = 8)

DA_final_bivarite_map_unsuitable_mischiefs_ratio_2019 <- 
  DA_bivarite_map_unsuitable_mefaits_ratio_2019 + 
  inset_element(DA_bi_legend_unsuitable_mischief_ratio_2019, left = 0, bottom = 0.6, right = 0.4, top = 1)

DA_final_bivarite_map_unsuitable_mischiefs_ratio_2019 #to see your plot

# Save in PDF in your output/figures folder to see the true sizes of your plot, ajust accordingly

ggsave("output/figures/Alexia/DA_bivarite_map_unsuitable_mischiefs_ratio_2019.pdf", plot = DA_final_bivarite_map_unsuitable_mischiefs_ratio_2019, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

#Scatter plots------------------------------------------------------------------

install.packages("car")
library(car)
install.packages("ggpubr")
library(ggpubr)

# CT level
# CT and mischief per 100 ppl
# variables to keep: GeoUID, population, median_HH_income_AT,p_unsuitable
# p_immigrants, p_aboriginal, p_low_income_AT, geometry <MULTIPOLYGON [m]>
# CATEGORIE <chr>,DATE <chr>
# no need to filter for NA in ggplot

CT_int_plots_1 <-
  int_CT %>% 
  st_filter(CT) %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(DATE==2019) %>% 
    group_by(GeoUID) %>% 
  summarize(CT_n_int_total_2019=n()) 

CT_int_plots_2 <-
  int_CT %>% 
  st_filter(CT) %>% 
  mutate(DATE = year(DATE)) %>% 
  filter(DATE==2019) %>% 
  filter(CATEGORIE=="Mefait") %>% 
  group_by(GeoUID) %>% 
  summarize(CT_n_mischiefs_2019=n()) 

CT_int_plots_3 <-
  full_join(st_drop_geometry(CT_int_plots_1),st_drop_geometry(CT_int_plots_2), by="GeoUID") %>% 
  mutate(CT_mischief_share_2019=CT_n_mischiefs_2019/CT_n_int_total_2019)
  
#run this if you want to use CT_int_plots_4 for the scatterplot
CT_int_plots_4 <-
left_join(CT_int_plots_3, CT, by="GeoUID") %>% 
  filter(population>50) %>% 
  mutate(CT_n_mischiefs_per100ppl=CT_n_mischiefs_2019/(population/100)) %>% 
  mutate(p_low_income_AT=p_low_income_AT/100) 

#run this if you want to use CT_int_plots_4 for bivariate maps
CT_int_plots_4 <-
  left_join(CT_int_plots_3, CT, by="GeoUID") %>% 
  filter(population>50) %>% 
  st_as_sf()

# Now you can play with int_CT_plots_4   
p1 <- ggplot(data=CT_int_plots_4, aes (x=p_immigrants, y =CT_n_mischiefs_per100ppl))+
  geom_point()+
  geom_smooth(method=lm)+ #Add "se=FALSE" for no confidence boundaries
  theme_minimal()+
  scale_x_continuous(name = "Percentage of immigrant inhabitants",
                     label = scales::percent)+
  scale_y_continuous("Mischief crimes per 100 people")+
  ylim(c(0,3))
  
  #xlim(c())
  #ylim(c())
  
  # change y between CT_mischief_share_2019 (aka: Percentage of mischief crimes out of all crimes)
# and CT_n_mischiefs_per100ppl (Mischief crimes per 100 people)
  # x can be either p_unsuitable, p_vm, p_immigrants, p_aboriginal, p_low_income_AT
  
# plot with R2 equation
  plot_1 <- p1 +
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 0.5, label.y = 2.5)

  plot_1 #to view the plot

    