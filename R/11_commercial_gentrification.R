# Step 1: Load datasets --------------------------------------------------------

# Load libraries
library(dplyr)
library(datasets)
library(ggplot2)


# Load the CT and DA datasets

load("output/CTALEXIA.Rdata")
load("output/DA.Rdata")
source("R/01_startup.R")
load("output/province.Rdata")
load("output/geometry.Rdata")

# Load the Ãtablissements alimentaires data from DonnÃ©es Montreal
# dataset is from 1988 to 2021.06.28
# the type category is case sensitive 

food_businesses_raw <-
  read_sf("data/businesses/businesses.shp") %>% 
  st_transform(32618) %>% 
  mutate(date = ymd(date_statu))

food_businesses_sf <- 
  food_businesses_raw %>% 
  select(-state, -date_statu, -latitude, -longitude, -x, -y) %>% 
  relocate(geometry, .after = last_col()) %>% 
  set_names(c("id", "name", "address", "city", "type", "statut", "date", "geometry"))

food_businesses <- 
  food_businesses_sf %>% 
  st_drop_geometry()

# save output
save(food_businesses, food_businesses_sf, file = "output/food_businesses.Rdata")

# load output
load("output/food_businesses.Rdata")


# This dataset has classified businesses into the following categories: 
# Ecole/mesure alimentaires, Ãpicerie, Ã©picerie avec prÃ©paration, Ã©vÃ¨nement spÃ©ciaux, 
# aliments naturels, Atelier de conditionnement de produits de la pÃªche, autres, 
# bar laitier, bar laitiers saisonnier, bar salon, taverne, boucherie, boucherie-Ã©picerie, 
# boulangerie, brasserie, cabane Ã  sucre, (cafÃ©, thÃ©, infusion, tisane = toute une catÃ©gorie), 
# cafÃ©tÃ©ria, CafÃ©tÃ©ria institution d'enseignement, Camion de distribution de produits carnÃ©s, 
# Camion de distribution de produits de la pÃªche, Camion de distribution de produits laitiers, 
# Camion-cuisine, Cantine mobile, Casse-croÃ»te, Centre d'accueil, Charcuterie, 
# Charcuterie/fromage, Confiserie/chocolaterie, Cuisine domestique, DÃ©coupe Ã  forfait, 
# Distributeur en gros de fruits et lÃ©gumes frais, Distributeur en gros de produits carnÃ©s, 
# Distributeur en gros de produits de la pÃªche, Distributeur en gros de produits laitiers, 
# Distributeur en gros de produits mixtes, Distributeur en gros de succÃ©danÃ©s de produits laitier, 
# Distributeur en gros d'eau, Distributrice automatique, EntrepÃ´t, EntrepÃ´t de produits laitiers, 
# EntrepÃ´t de produits mixtes, EntrepÃ´t de produits vÃ©gÃ©taux, EntrepÃ´t d'eau, 
# Fabrique de boissons gazeuses, Fruits et lÃ©gumes prÃªts Ã  l'emploi, Garderie, 
# HÃ´pital, Kiosque, Local de prÃ©paration, Magasin Ã  rayons, MarchÃ© public, 
# Noix et arachides, Organisme d'aide alimentaire, PÃ¢tes alimentaires, PÃ¢tisserie, 
# PÃ¢tisserie-dÃ©pÃ´t, Poissonnerie, Produits Ã  base de protÃ©ines vÃ©gÃ©tales, 
# RÃ©sidences de personnes Ã¢gÃ©es, Ramasseur de lait, Restaurant, Restaurant mets pour emporter, 
# Restaurant service rapide, Site d'eau vendue au volume, supermarchÃ©, Traiteur, 
# Usine de produits laitiers, Usine de produits marins, Usine d'emballage de glace, 
# Usine produit autre, VÃ©hicule livraison, Vendeur itinÃ©rant.

# Load wes anderson colors 

library(wesanderson)
pal <- wes_palette("Zissou1", 10, type = c("continuous"))

# Determining how many businesses are inventoried per year

food_businesses%>% 
  mutate(date=year(date)) %>% 
  count(date) %>% 
  View()


# Determining how many businesses per category

food_businesses%>% 
  count(type) %>% 
  View()

# Initial grouping strategy-----------------------------------------------------
# To clean the groups before grouping

types_to_remove <- c("Atelier de conditionnement de produits de la pêche", "Cabane à sucre",
                     "Camion de distribution de produits carnés", "Camion de distribution de produits de la pêche",
                     "Camion de distribution de produits laitiers", "Camp de vacances", "Hôpital", "Site d'eau vendue au volume",
                     "Véhicule livraison", "Vendeur itinérant", "Distributrice automatique")

food_businesses_test <- 
  food_businesses %>% 
  filter(!type %in% types_to_remove) %>% 
  filter(!str_detect(type, "Distributeur en gros")) %>% 
  filter(!str_detect(type, "Entrepôt")) %>% 
  filter(!str_detect(type, "Usine"))

groups <- 
  food_businesses_test %>% 
  select(-city, -statut) %>% 
  arrange(date) %>% 
  group_by(address) %>% 
  mutate(group_id = row_number()) 

# Load grouped data

load("output/grouped_addresses.Rdata")

# Kosta's grouping--------------------------------------------------------------

# types to remove

types_to_remove_kosta <- c("Atelier de conditionnement de produits de la pêche", "Cabane à sucre",
                     "Camion de distribution de produits carnés", "Camion de distribution de produits de la pêche",
                     "Camion de distribution de produits laitiers", "Camp de vacances", "Hôpital", "Site d'eau vendue au volume",
                     "Véhicule livraison", "Vendeur itinérant", "Distributrice automatique", "École/mesures alimentaires", 
                     "Événements spéciaux", "Autres", "Cafétéria institution d'enseignement",
                     "Camion-cuisine", "Cantine mobile", "Centre d'accueil", "Cuisine domestique", 
                     "Découpe à forfait", "Distributeur en gros de fruits et légumes frais",
                     "Distributeur en gros de produits de la pêche", "Distributeur en gros de produits mixtes",
                     "Distributeur en gros de produits carnés","Distributeur en gros de produits laitiers",
                     "Distributeur en gros de succédanés de produits laitiers", "Distributeur en gros d'eau",
                     "Distributrice automatique","Entrepôt", "Entrepôt de produits laitiers", "Entrepôt de produits mixtes", 
                     "Entrepôt de produits végétaux", "Entrepôt d'eau", "Fabrique de boissons gazeuses",
                     "Fruits et légumes prêts à l'emploi", "Garderie","Local de préparation", 
                     "Marché public", "Noix et arachides", "Organisme d'aide alimentaire",
                     "Pâtes alimentaires", "Produits à base de protéines végétales","Résidences de personnes âgées",
                     "Ramasseur de lait", "Site d'eau vendue au volume", "Traiteur", "Usine d'embouteillage d'eau",
                     "Usine de produits laitiers", "Usine de produits marins", "Usine d'emballage de glace",
                     "Usine produit autre", "Kiosque", "Résidence de personnes âgées")

# recreate dataframe and remove types

food_businesses_kosta <-food_businesses_sf %>% 
  filter(!type %in% types_to_remove_kosta )


# restaurant category
#restaurants_kosta <-food_businesses %>% 
 # filter(type=="Restaurant")

# cafe category

keep_cafes_kosta <-c("Bar laitier","Bar laitier saisonnier","Bar salon, taverne",
                    "Brasserie","Café, thé, infusion, tisane","Cafétéria","Confiserie/chocolaterie",
                     "Restaurant mets pour emporter","Restaurant service rapide","Casse-croûte")

food_businesses_kosta <-
food_businesses_kosta %>%  
  mutate(type = ifelse(type %in% keep_cafes_kosta, "Cafe", type)) 

# food stores

keep_foodstores_kosta <-c("Épicerie", "Épicerie avec préparation", "Aliments naturels",
                          "Boucherie","Boucherie-épicerie","Boulangerie","Charcuterie",
                          "Charcuterie/fromage","Pâtisserie","Pâtisserie-dépôt",
                          "Poissonnerie","Supermarché","Magasin à rayons")

food_businesses_kosta <-
  food_businesses_kosta %>%  
  mutate(type = ifelse(type %in% keep_foodstores_kosta, "Food store", type)) 

# verify that only the 3 categories were kept

unique(food_businesses_kosta$type) 

# Bar charts Kosta--------------------------------------------------------------

# Merge the borough and food businesses data together

food_businesses_kosta_boroughs <- st_join(food_businesses_kosta, boroughs) %>% 
  st_drop_geometry()

# If you join by putting the boroughs_raw data first, there will be less data, going
# from 31 717 to 28 373

open_closed_kosta <- c("Ouvert", "Fermé")

#graph 1 : Total number of open and closed businesses per category per year
# Can't seem to be able to rename the legend

food_businesses_kosta_boroughs %>% 
  mutate(date = year(date)) %>% 
  filter(date>=2011) %>% 
  filter(statut %in% open_closed_kosta) %>% 
  group_by(date, statut, type) %>% 
  summarize(n = n()) %>% 
  ggplot()+
  geom_line(aes(x=date, y=n, color = statut))+
  facet_wrap(~type)+
  ggtitle("Number of open and closed businesses per category per year")+
  labs(x = "Year", y ="Number of businesses")+
  labs(fill="Status")


# graph 2: Total number of closed and open businesses per category

food_businesses_kosta_boroughs %>% 
  mutate(date = year(date)) %>% 
  filter(date>=2011) %>% 
  filter(statut %in% open_closed_kosta) %>% 
  group_by(date, statut, type) %>% 
  summarize(n = n()) %>% 
  ggplot()+
  geom_line(aes(x=date, y=n, color = type))+
  facet_wrap(~statut)+
  ggtitle("Number of open and closed businesses per category per year")+
  labs(x = "Year", y ="Number of businesses", fill="Type")

# graph 3 is not working

food_businesses_kosta_boroughs %>% 
  mutate(date = year(date)) %>% 
  filter(date>=2011) %>% 
  filter(borough %in% food_top_11) %>% 
  filter(statut %in% open_closed_kosta) %>% 
  group_by(borough,date, statut, type, dwellings) %>% 
  summarize(statut_per_borough= n()) %>% 
  mutate(statut_per_borough_per_100_dwellings = statut_per_borough/(dwellings/100)) %>%
  ggplot()+
  geom_bar(aes(x = date, fill = statut_per_borough_per_100_dwellings), stat="count")+
    facet_wrap(~borough)+
  ggtitle("Number of open and closed businesses per category per year")+
 labs(x = "Year", y ="Number of businesses per 100 dwellings", fill = "Type")

# graph 4: number of open businesses per category per 1000 dwellings per borough

food_top_11 <- 
  food_businesses_kosta_boroughs %>% 
  filter(!is.na(borough)) %>% 
  mutate(date = year(date)) %>% 
  filter(date>=2011) %>% 
  count(borough) %>% 
  arrange(desc(n)) %>% 
  slice(1:11) %>% 
  pull(borough)

food_businesses_kosta_boroughs %>% 
  mutate(date = year(date)) %>% 
  filter(date>=2011) %>% 
 # filter(date!=2021) %>% #because it messes up the year scale
  filter(borough %in% food_top_11) %>% 
  filter(statut %in% open_closed_kosta) %>% 
  group_by(borough,date, statut, type, dwellings) %>% 
  summarize(statut_per_borough= n()) %>% 
  mutate(statut_per_borough_per_1000_dwellings = statut_per_borough/(dwellings/1000)) %>%
  ggplot()+
  geom_line(aes(x = date, y = statut_per_borough_per_1000_dwellings, color = type))+
  facet_wrap(~borough)+
  ggtitle("Number of open businesses per category per 1000 dwellings per year")+
  labs(x = "Year", y ="Number of businesses per 1000 dwellings", fill = "Type")

# graph 5
# testing with the ggplot2 package
# can't install these packages

food_businesses_kosta_boroughs %>% 
  mutate(date = year(date)) %>% 
  filter(date>=2011) %>% 
  filter(statut %in% open_closed_kosta) %>% 
  group_by(borough,date, statut, type, dwellings) %>% 
  summarize(statut_per_borough= n()) %>% 
  mutate(statut_per_borough_per_100_dwellings = statut_per_borough/(dwellings/100)) %>%
  ggplot2.barplot(xName="date", yName="statut_per_borough_per_100_dwellings",
                   groupName="type", position=position_dodge())+
  #geom_bar(aes(x = date, fill = statut_per_borough_per_100_dwellings), stat="count")+
  facet_wrap(~borough)+
  ggtitle("Number of open and closed businesses per category per year")+
  labs(x = "Year", y ="Number of businesses per 100 dwellings", fill = "Type")


# graph 6
# not working
food_businesses_kosta_boroughs %>% 
  mutate(date = year(date)) %>% 
  filter(date>=2011) %>% 
  filter(borough %in% food_top_11) %>% 
  filter(statut %in% open_closed_kosta) %>% 
  group_by(borough,date, statut, type, dwellings) %>% 
  summarize(statut_per_borough= n()) %>% 
  mutate(statut_per_borough_per_100_dwellings = statut_per_borough/(dwellings/100)) %>%
  group_by(borough,date, statut, statut_per_borough_per_100_dwellings, type) %>% 
  summarize(statut_per_type=n()) %>% 
  ggplot()+
  geom_bar(aes(x = date, fill = statut_per_type, stat="count"))+
  facet_wrap(~borough)+
  ggtitle("Number of open and closed businesses per category per year")+
  labs(x = "Year", y ="Number of businesses per 100 dwellings", fill = "Type")

# graph 7: for Ville-Marie per 1000 dwellings

food_businesses_kosta_boroughs %>% 
  mutate(date = year(date)) %>% 
  filter(date>=2011) %>% 
  # filter(date!=2021) %>% #because it messes up the year scale
  filter(borough=="Ville-Marie") %>% 
  filter(statut %in% open_closed_kosta) %>% 
  group_by(date, statut, type, dwellings) %>% 
  summarize(statut_villemarie= n()) %>% 
  mutate(statut_villemarie_per_1000_dwellings = statut_villemarie/(dwellings/1000)) %>%
  ggplot()+
  geom_line(aes(x = date, y = statut_villemarie_per_1000_dwellings, color = type))+
  facet_wrap(~statut)+
  ggtitle("Ville-Marie: Openings and closings per category per 1000 dwellings by year")+
  labs(x = "Year", y ="Number of businesses per 1000 dwellings", fill = "Type")+
  theme_minimal()+
  theme(legend.position = "bottom")
  

# graph 8: for Ville-Marie absolute count

food_businesses_kosta_boroughs %>% 
  mutate(date = year(date)) %>% 
  filter(date>=2011) %>% 
  # filter(date!=2021) %>% #because it messes up the year scale
  filter(borough=="Ville-Marie") %>% 
  filter(statut %in% open_closed_kosta) %>% 
  group_by(date, statut, type, dwellings) %>% 
  summarize(statut_villemarie= n()) %>% 
  ggplot()+
  geom_line(aes(x = date, y = statut_villemarie, color = type))+
  facet_wrap(~statut)+
  ggtitle("Ville-Marie: Openings and closings per category by year")+
  labs(x = "Year", y ="Number of businesses", fill = "Type")+
  theme_minimal()+
  theme(legend.position = "bottom")

# Shares of openings and closings-----------------------------------------------

# find total number of closings and openings for the whole island per year

total_status_by_borough <- 
  food_businesses_kosta_boroughs %>% 
  mutate(date = year(date)) %>% 
  filter(date>=2011 & date!=2021) %>% 
  filter(statut == "Fermé" | statut == "Ouvert") %>% 
  count(borough, statut, date) %>% 
  rename(total=n)

top_10_boroughs <- 
  boroughs %>% 
  mutate(dwelling_density = dwellings/st_area(geometry)) %>% 
  arrange(desc(dwelling_density)) %>% 
  slice(1:10) %>% 
  pull(borough)

# percentage of closed businesses per total closings by borough per year (2011-2020)

food_businesses_kosta_boroughs %>% 
  mutate(date = year(date)) %>% 
  filter(date>=2011 & date!=2021) %>% 
  filter(statut == "Fermé" | statut == "Ouvert") %>% 
  count(borough, statut, date, type) %>% 
  full_join(., total_status_by_borough, by = c("borough", "statut", "date")) %>% 
  filter(borough %in% top_10_boroughs) %>% 
  mutate(percentage = n/total) %>% 
  filter(statut == "Fermé") %>% 
  ggplot()+
  geom_smooth(aes(x=date, y= percentage, color=type), se=FALSE)+
  facet_wrap(~borough)+
  ggtitle("Percentage of closed businesses per total closings by borough per year (2011-2020)")+
  labs(x = "Year", y ="Percentage", fill = "Type")+
  theme_minimal()+
  theme(legend.position = "bottom")

# percentage of opening businesses per total openings by borough per year (2011-2020)

food_businesses_kosta_boroughs %>% 
  mutate(date = year(date)) %>% 
  filter(date>=2011 & date!=2021) %>% 
  filter(statut == "Fermé" | statut == "Ouvert") %>% 
  count(borough, statut, date, type) %>% 
  full_join(., total_status_by_borough, by = c("borough", "statut", "date")) %>% 
  filter(borough %in% top_10_boroughs) %>% 
  mutate(percentage = n/total) %>% 
  filter(statut == "Ouvert") %>% 
  ggplot()+
  geom_smooth(aes(x=date, y= percentage, color=type), se=FALSE)+
  facet_wrap(~borough)+
  ggtitle("Percentage of opened businesses per total openings by borough per year (2011-2020)")+
  labs(x = "Year", y ="Percentage", fill = "Type")+
  theme_minimal()+
  theme(legend.position = "bottom")

# # % ferme et % ouvert out of all categories
# 
# total_status_by_borough <- 
#   food_businesses_kosta_boroughs %>% 
#   mutate(date = year(date)) %>% 
#   filter(date>=2011 & date!=2021) %>% 
#   filter(statut == "Fermé" | statut == "Ouvert") %>% 
#   count(borough, statut, date) %>% 
#   rename(total=n)
# 
# top_10_boroughs <- 
#   boroughs %>% 
#   mutate(dwelling_density = dwellings/st_area(geometry)) %>% 
#   arrange(desc(dwelling_density)) %>% 
#   slice(1:10) %>% 
#   pull(borough)
# 
# boroughs_of_interest <- c("Côte-des-Neiges-Notre-Dame-de-Grâce", "Rosemont-La Petite-Patrie",
#                           "Mercier-Hochelaga-Maisonneuve", "Villeray-Saint-Michel-Parc-Extension",
#                           "Le Plateau-Mont-Royal", "Ahuntsic-Cartierville", "Le Sud-Ouest",
#                           "Verdun")
# 
# # percentage open per total openings
# food_businesses_kosta_boroughs %>%
#   mutate(date = year(date)) %>%
#   filter(date>=2011 & date!=2021) %>%
#   filter(statut == "Fermé" | statut == "Ouvert") %>%
#   count(borough, statut, date, type) %>%
#   full_join(., total_status_by_borough, by = c("borough", "statut", "date")) %>%
#   filter(borough %in% top_10_boroughs) %>%
#   mutate(percentage = n/total) %>%
#   filter(statut == "Ouvert") %>%
#   ggplot()+
#   geom_smooth(aes(x=date, y= percentage, color=type), se=FALSE)+
#   scale_color_discrete()+
# facet_wrap(~borough)+
#   theme_minimal()
# 
# ## ratio open to closed
# ## add titles to clean up graph
# full_join(total_ferme_by_borough, total_ouvert_by_borough, by = c("borough", "type", "date")) %>%
#   filter(borough %in% boroughs_of_interest) %>%
#   mutate(total_ouvert = ifelse(is.na(total_ouvert), 0, total_ouvert)) %>% 
#   mutate(total_ferme = ifelse(is.na(total_ferme), 0, total_ferme)) %>% 
#   mutate(ratio_open_to_closed = total_ouvert/total_ferme) %>%
#   mutate(positive_negative = total_ouvert - total_ferme) %>% 
#   ggplot()+
#   geom_smooth(aes(x=date, y=ratio_open_to_closed, color=type), se=FALSE)+
#   facet_wrap(~borough)+
#   theme_minimal()

# ratio open-to-close by year by borough by type

total_ferme_by_borough <- 
  food_businesses_kosta_boroughs %>% 
  mutate(date = year(date)) %>% 
  filter(date>=2011 & date!=2021) %>% 
  filter(statut == "Fermé") %>% 
  count(borough, date, type) %>% 
  rename(total_ferme=n)

total_ouvert_by_borough <- 
  food_businesses_kosta_boroughs %>% 
  mutate(date = year(date)) %>% 
  filter(date>=2011 & date!=2021) %>% 
  filter(statut == "Ouvert") %>% 
  count(borough, date, type) %>% 
  rename(total_ouvert=n)

full_join(total_ferme_by_borough, total_ouvert_by_borough, by = c("borough", "type", "date")) %>%
  filter(borough %in% boroughs_of_interest) %>%
  mutate(total_ouvert = ifelse(is.na(total_ouvert), 0, total_ouvert)) %>% 
  mutate(total_ferme = ifelse(is.na(total_ferme), 0, total_ferme)) %>% 
  mutate(ratio_open_to_closed = total_ouvert/total_ferme) %>%
  mutate(positive_negative = total_ouvert - total_ferme) %>% 
  ggplot()+
  geom_smooth(aes(x=date, y=ratio_open_to_closed, color=type), se=FALSE)+
  facet_wrap(~borough)+
  theme_minimal()+
  ggtitle("Ratio of openings to closings (total openings/total closings)(2011-2020)")+
  labs(x = "Year", y ="Ratio", fill = "Type")+
  theme(legend.position = "bottom")

 ## positive negative stores: open stores - closed stores 

full_join(total_ferme_by_borough, total_ouvert_by_borough, by = c("borough", "type", "date")) %>%
  filter(borough %in% boroughs_of_interest) %>%
  mutate(total_ouvert = ifelse(is.na(total_ouvert), 0, total_ouvert)) %>% 
  mutate(total_ferme = ifelse(is.na(total_ferme), 0, total_ferme)) %>% 
  mutate(ratio_open_to_closed = total_ouvert/total_ferme) %>%
  mutate(positive_negative = total_ouvert - total_ferme) %>% 
  ggplot()+
  geom_smooth(aes(x=date, y=positive_negative, color=type), se=FALSE)+
  facet_wrap(~borough)+
  theme_minimal()+
  ggtitle("Total openings- total closings(2011-2020)")+
  labs(x = "Year", y ="Ratio", fill = "Type")+
  theme(legend.position = "bottom")

### This code is currently not working

# total_closings_kosta_per_year <-
# food_businesses_kosta_boroughs %>% 
#   mutate(date = year(date)) %>% 
#   filter(date>=2011 & date!=2021) %>% 
#   filter(statut == "Fermé") %>% 
#   group_by(date) %>%
#   summarize(total_number_closings_kosta= n())
# 
# # total number openings per year (2011-2020)
# 
# total_openings_kosta_per_year <-
#   food_businesses_kosta_boroughs %>% 
#   mutate(date = year(date)) %>% 
#   filter(date>=2011 & date!=2021) %>% 
#   filter(statut == "Ouvert") %>% 
#   group_by(date) %>%
#   summarize(total_number_openings_kosta= n())
# 
# # join the closings and openings
# 
# total_openings_closings_island_kosta <-
# total_closings_kosta_per_year %>% 
# full_join(., total_openings_kosta_per_year, by = "date")
# 
# 
# # Graph 1: Ahuntsic-Cartierville
# 
# share_open_ahuntsiccatierville <-
# food_businesses_kosta_boroughs %>% 
#   mutate(date = year(date)) %>% 
#   filter(date>=2011& date!=2021) %>% 
#   filter(borough=="Ahuntsic-Cartierville") %>% 
#   filter(statut=="Ouvert") %>%  
#   group_by(date, type, statut) %>% 
#   summarize(open_ahuntsiccatierville= n()) %>% 
#   full_join(., total_openings_kosta_per_year, by = "date") %>% 
#   mutate(share_openings_ahuntsiccatierville = open_ahuntsiccatierville/
#            total_number_openings_kosta)
# 
# share_closed_ahuntsiccatierville <-
#   food_businesses_kosta_boroughs %>% 
#   mutate(date = year(date)) %>% 
#   filter(date>=2011& date!=2021) %>% 
#   filter(borough=="Ahuntsic-Cartierville") %>% 
#   filter(statut=="Fermé") %>%  
#   group_by(date, type, statut) %>% 
#   summarize(closed_ahuntsiccatierville= n()) %>% 
#   full_join(., total_closings_kosta_per_year, by = "date") %>% 
#   mutate(share_closings_ahuntsiccatierville = closed_ahuntsiccatierville/
#            total_number_closings_kosta)
# 
# combined_openings_closings_share_ahuntsiccatierville <-      
# share_closed_ahuntsiccatierville %>% 
#   full_join(.,share_open_ahuntsiccatierville)
# 
# combined_openings_closings_share_ahuntsiccatierville %>% 
#   ggplot()+
#   geom_line(aes(x = date, y = statut_villemarie_per_1000_dwellings, color = type))+
#   ### Help here!!! I don't know which y to use, since I sort of have 2 (share_closings_ahuntsiccatierville and share_openings_ahuntsiccatierville)
#   
#   facet_wrap(~statut)+
#   ggtitle("Ville-Marie: Openings and closings per category per 1000 dwellings by year")+
#   labs(x = "Year", y ="Number of businesses per 1000 dwellings", fill = "Type")+
#   theme_minimal()+
#   theme(legend.position = "bottom")

# Statistical Analysis ---------------------------------------------------------

# Bar charts

food_businesses %>% 
  mutate(date = year(date)) %>% 
  ggplot()+
  geom_histogram(aes(x = date, fill = statut), stat="count")

# Proportional status per year bar graph

food_businesses %>% 
  mutate(date = year(date)) %>%
  filter(date >= 2010) %>% 
  count(date, statut) %>% 
  ggplot(aes(x = date, y = n, fill = statut)) + 
  geom_bar(position = "fill",stat = "identity")


# Alexia + Cloé groupings------------------------------------------------------

# recreate dataframe and remove types

food_businesses_alexia <-food_businesses_sf 

# cafe category

keep_cafes_alexia <-c("Bar laitier","Bar laitier saisonnier",
                     "Café, thé, infusion, tisane",
                     "Restaurant mets pour emporter","Restaurant service rapide","Casse-croûte")

food_businesses_alexia <-
  food_businesses_alexia %>%  
  mutate(type = ifelse(type %in% keep_cafes_alexia, "Café", type)) 

#industriel 

keep_industrial_alexia <-c("Usine de produits laitiers","Usine de produits marins","Usine produit autre",
                           "Fabrique de boissons gazeuses","Distributeur en gros de fruits et légumes frais",
                           "Distributeur en gros de produits de la pêche","Distributeur en gros de produits mixtes",
                           "Distributeur en gros de produits carnés","Distributeur en gros de produits laitiers",
                           "Distributeur en gros de succédanés de produits laitiers","Entrepôt","Entrepôt de produits laitiers",
                           "Entrepôt de produits mixtes","Entrepôt de produits végétaux","Entrepôt d'eau",
                           "Distributeur en gros d'eau","Atelier de conditionnement de produits de la pêche",
                           "Camion de distribution de produits carnés","Camion de distribution de produits de la pêche",
                           "Camion de distribution de produits laitiers","Découpe à forfait","Ramasseur de lait",
                           "Site d'eau vendue au volume","Usine d'emballage de glace","Véhicule livraison",
                           "Local de préparation","Fruits et légumes prêts à l'emploi","Pâtes alimentaires",
                           "Produits à base de protéines végétales", "Usine d'embouteillage d'eau")

food_businesses_alexia <-
  food_businesses_alexia %>%  
  mutate(type = ifelse(type %in% keep_industrial_alexia, "Industrial", type)) 

# restaurant = restaurant 

# bars

keep_bars_alexia <-c("Bar salon, taverne", "Brasserie")

food_businesses_alexia <-
  food_businesses_alexia %>%  
  mutate(type = ifelse(type %in% keep_bars_alexia, "Bar", type)) 

#grocery stores

keep_grocerystores_alexia <-c("Épicerie", "Épicerie avec préparation", "Supermarché", 
                              "Magasin à rayons")

food_businesses_alexia <-
  food_businesses_alexia %>%  
  mutate(type = ifelse(type %in% keep_grocerystores_alexia, "Grocery store", type)) 


#specialized stores

keep_specializedstores_alexia <-c("Aliments naturels", "Boucherie", "Boucherie-épicerie", 
                              "Boulangerie", "Charcuterie", "Charcuterie/fromage", "Pâtisserie",
                              "Pâtisserie-dépôt", "Poissonnerie", "Confiserie/chocolaterie",
                              "Marché public", "Noix et arachides")

food_businesses_alexia <-
  food_businesses_alexia %>%  
  mutate(type = ifelse(type %in% keep_specializedstores_alexia, "Specialized stores", type)) 


# Ephemeral

keep_ephemeral_alexia <-c("Événements spéciaux", "Camion-cuisine", "Cantine mobile", 
                                  "Vendeur itinérant", "Traiteur",  "Kiosque")

food_businesses_alexia <-
  food_businesses_alexia %>%  
  mutate(type = ifelse(type %in% keep_ephemeral_alexia, "Ephemeral stores", type)) 


#Institutional/Community

keep_institutional_community_alexia <- c("Cafétéria institution d'enseignement", 
                                        "École/mesures alimentaires", "Garderie", 
                          "Hôpital", "Organisme d'aide alimentaire", 
                          "Centre d'accueil","Résidence de personnes âgées",
                          "Cafétéria")

food_businesses_alexia <-
  food_businesses_alexia %>%  
  mutate(type = ifelse(type %in% keep_institutional_community_alexia, "Institutional/Community", type))


#Others

keep_others_alexia <- c("Autres", "Cuisine domestique", "Distributrice automatique", 
                                         "Cabane à sucre", "Camp de vacances")
                                
food_businesses_alexia <-
  food_businesses_alexia %>%  
  mutate(type = ifelse(type %in% keep_others_alexia, "Other", type))


# verify that only the created categories were kept

unique(food_businesses_alexia$type) 

#only keep the categories of interest and merge with boroughs

food_businesses_boroughs_alexia <- 
  food_businesses_alexia %>% 
  filter(type != "Ephemeral stores") %>% 
  filter(type != "Institutional/Community") %>% 
  filter(type != "Industrial") %>% 
  filter(type != "Other") %>% 
  st_join(boroughs, .) %>% 
  st_drop_geometry()

# Charts with Alexia groupings--------------------------------------------------

# total openings and closings per borough

total_ferme_by_borough_alexia <- 
  food_businesses_boroughs_alexia %>% 
  mutate(date = year(date)) %>% 
  filter(date>=2011 & date!=2021) %>% 
  filter(statut == "Fermé") %>% 
  count(borough, date, type) %>% 
  rename(total_ferme=n)

total_ouvert_by_borough_alexia <- 
  food_businesses_boroughs_alexia %>% 
  mutate(date = year(date)) %>% 
  filter(date>=2011 & date!=2021) %>% 
  filter(statut == "Ouvert") %>% 
  count(borough, date, type) %>% 
  rename(total_ouvert=n)


# ratio of openings to closings 

full_join(total_ferme_by_borough_alexia, total_ouvert_by_borough_alexia, by = c("borough", "type", "date")) %>%
  filter(borough %in% boroughs_of_interest) %>%
  mutate(total_ouvert = ifelse(is.na(total_ouvert), 0, total_ouvert)) %>% 
  mutate(total_ferme = ifelse(is.na(total_ferme), 0, total_ferme)) %>% 
  mutate(ratio_open_to_closed = total_ouvert/total_ferme) %>%
  mutate(positive_negative = total_ouvert - total_ferme) %>% 
  ggplot()+
  geom_smooth(aes(x=date, y=ratio_open_to_closed, color=type), se=FALSE)+
  facet_wrap(~borough)+
  theme_minimal()+
  ggtitle("Ratio of openings to closings (total openings/total closings)(2011-2020)")+
  labs(x = "Year", y ="Ratio", fill = "Type")+
  theme(legend.position = "bottom")+
  ylim(c())

## positive negative stores: open stores - closed stores 

full_join(total_ferme_by_borough_alexia, total_ouvert_by_borough_alexia, by = c("borough", "type", "date")) %>%
  filter(borough %in% boroughs_of_interest) %>%
  mutate(total_ouvert = ifelse(is.na(total_ouvert), 0, total_ouvert)) %>% 
  mutate(total_ferme = ifelse(is.na(total_ferme), 0, total_ferme)) %>% 
  mutate(ratio_open_to_closed = total_ouvert/total_ferme) %>%
  mutate(positive_negative = total_ouvert - total_ferme) %>% 
  ggplot()+
  geom_smooth(aes(x=date, y=positive_negative, color=type), se=FALSE)+
  facet_wrap(~borough)+
  theme_minimal()+
  ggtitle("Total openings- total closings(2011-2020)")+
  labs(x = "Year", y ="Ratio", fill = "Type")+
  theme(legend.position = "bottom")+
  ylim(c(-20, 25))

# Alexia tags: Graph per boroughs------------------------------------------------------------

#Boroughs of interest: Ville-Marie, Le Plateau-Mont-Royal, Villeray-Saint-Michel-Parc-Extension, Le Sud-Ouest and 	
# Mercier-Hochelaga-Maisonneuve

food_businesses_boroughs_alexia %>% 
  count(borough) %>% 
  View()

# Graph 1: % closing by type of food business

food_businesses_boroughs_alexia %>% 
  mutate(date = year(date)) %>% 
  filter(date>=2011 & date!=2021) %>% 
  filter(statut == "Fermé" | statut == "Ouvert") %>% 
  count(borough, statut, date, type) %>% 
  full_join(., total_status_by_borough, by = c("borough", "statut", "date")) %>% 
  filter(borough=="Mercier-Hochelaga-Maisonneuve") %>% 
  mutate(percentage = (n/total)*100) %>% 
  filter(statut == "Fermé") %>% 
  ggplot()+
  geom_smooth(aes(x=date, y= percentage, color=type), se=FALSE)+
  ggtitle("Mercier-Hochelaga-Maisonneuve:Percentage of closed businesses per total closings by year (2011-2020)")+
  labs(x = "Year", y ="Percentage", fill = "Type")+
  theme_minimal()+
  theme(legend.position = "bottom")+
  scale_color_manual(name="Type of food\nbusiness",
                     values=col_palette[c(1:5)])

# Graph 2: % openings by type of food business

food_businesses_boroughs_alexia %>% 
  mutate(date = year(date)) %>% 
  filter(date>=2011 & date!=2021) %>% 
  filter(statut == "Fermé" | statut == "Ouvert") %>% 
  count(borough, statut, date, type) %>% 
  full_join(., total_status_by_borough, by = c("borough", "statut", "date")) %>% 
  filter(borough=="Mercier-Hochelaga-Maisonneuve") %>% 
  mutate(percentage = (n/total)*100) %>% 
  filter(statut == "Ouvert") %>% 
  ggplot()+
  geom_smooth(aes(x=date, y= percentage, color=type), se=FALSE)+
  ggtitle("Mercier-Hochelaga-Maisonneuve:Percentage of opened businesses per total openings by year (2011-2020)")+
  labs(x = "Year", y ="Percentage", fill = "Type")+
  theme_minimal()+
  theme(legend.position = "bottom")+
  scale_color_manual(name="Type of food\nbusiness",
                     values=col_palette[c(1:5)])


#graph 3: Ratio open-to-close

full_join(total_ferme_by_borough_alexia, total_ouvert_by_borough_alexia, by = c("borough", "type", "date")) %>%
  filter(borough=="Mercier-Hochelaga-Maisonneuve") %>%
  mutate(total_ouvert = ifelse(is.na(total_ouvert), 0, total_ouvert)) %>% 
  mutate(total_ferme = ifelse(is.na(total_ferme), 0, total_ferme)) %>% 
  mutate(ratio_open_to_closed = total_ouvert/total_ferme) %>%
  mutate(positive_negative = total_ouvert - total_ferme) %>% 
  ggplot()+
  geom_smooth(aes(x=date, y=ratio_open_to_closed, color=type), se=FALSE)+
  theme_minimal()+
  ggtitle("Mercier-Hochelaga-Maisonneuve: Ratio of openings to closings (total openings/total closings)(2011-2020)")+
  labs(x = "Year", y ="Ratio", fill = "Type")+
  theme(legend.position = "bottom")+
  scale_color_manual(name="Type of food\nbusiness",
                     values=col_palette[c(1:5)])

# graph 4:Openings-closings

full_join(total_ferme_by_borough_alexia, total_ouvert_by_borough_alexia, by = c("borough", "type", "date")) %>%
  filter(borough=="Mercier-Hochelaga-Maisonneuve") %>%
  mutate(total_ouvert = ifelse(is.na(total_ouvert), 0, total_ouvert)) %>% 
  mutate(total_ferme = ifelse(is.na(total_ferme), 0, total_ferme)) %>% 
  mutate(ratio_open_to_closed = total_ouvert/total_ferme) %>%
  mutate(positive_negative = total_ouvert - total_ferme) %>% 
  ggplot()+
  geom_smooth(aes(x=date, y=positive_negative, color=type), se=FALSE)+
  facet_wrap(~borough)+
  theme_minimal()+
  ggtitle("Mercier-Hochelaga-Maisonneuve: Total openings - total closings (2011-2020)")+
  labs(x = "Year", y ="Count", fill = "Type")+
  theme(legend.position = "bottom")+
  scale_color_manual(name="Type of food\nbusiness",
                     values=col_palette[c(1:5)])

# Cloé's grouped addresses----------------------------------------------------------------------------------------

load("output/food_businesses.Rdata")
load("output/province.Rdata")
load("output/geometry.Rdata")

load("output/food_businesses.Rdata")

# Alexia's categories ----------------------------------------------------------------------

cafes <-c("Bar laitier", "Casse-croûte",
          "Café, thé, infusion, tisane")

restaurants <- c("Restaurant", "Restaurant mets pour emporter", "Restaurant service rapide")

industrial <- c("Usine de produits laitiers","Usine de produits marins","Usine produit autre",
                "Fabrique de boissons gazeuses","Distributeur en gros de fruits et légumes frais",
                "Distributeur en gros de produits de la pêche","Distributeur en gros de produits mixtes",
                "Distributeur en gros de produits carnés","Distributeur en gros de produits laitiers",
                "Distributeur en gros de succédanés de produits laitiers","Entrepôt","Entrepôt de produits laitiers",
                "Entrepôt de produits mixtes","Entrepôt de produits végétaux","Entrepôt d'eau",
                "Distributeur en gros d'eau","Atelier de conditionnement de produits de la pêche",
                "Camion de distribution de produits carnés","Camion de distribution de produits de la pêche",
                "Camion de distribution de produits laitiers","Découpe à forfait","Ramasseur de lait",
                "Site d'eau vendue au volume","Usine d'emballage de glace","Véhicule livraison",
                "Local de préparation","Fruits et légumes prêts à l'emploi","Pâtes alimentaires",
                "Produits à base de protéines végétales", "Usine d'embouteillage d'eau")

bars <-c("Bar salon, taverne", "Brasserie")

grocery <-c("Épicerie", "Épicerie avec préparation", "Supermarché", 
            "Magasin à rayons")

specialized <-c("Aliments naturels", "Boucherie", "Boucherie-épicerie", 
                "Boulangerie", "Charcuterie", "Charcuterie/fromage", "Pâtisserie",
                "Pâtisserie-dépôt", "Poissonnerie", "Confiserie/chocolaterie",
                "Marché public", "Noix et arachides")

ephemeral <-c("Événements spéciaux", "Camion-cuisine", "Cantine mobile", 
              "Vendeur itinérant", "Traiteur",  "Kiosque", "Bar laitier saisonnier")

institutional <- c("Cafétéria institution d'enseignement", 
                   "École/mesures alimentaires", "Garderie", 
                   "Hôpital", "Organisme d'aide alimentaire", 
                   "Centre d'accueil","Résidence de personnes âgées",
                   "Cafétéria")

others <- c("Autres", "Cuisine domestique", "Distributrice automatique", 
            "Cabane à sucre", "Camp de vacances")

# Clean the sf dataset with new categories ----------------------------------------

food_businesses_sf <- 
  food_businesses_sf %>%  
  mutate(type = ifelse(type %in% industrial, "Industrial", type),
         type = ifelse(type %in% grocery, "General grocery", type),
         type = ifelse(type %in% bars, "Bars", type),
         type = ifelse(type %in% ephemeral, "Ephemeral", type),
         type = ifelse(type %in% institutional, "Institutional", type),
         type = ifelse(type %in% specialized, "Specialized", type),
         type = ifelse(type %in% others, "Other", type),
         type = ifelse(type %in% cafes, "Cafes", type),
         type = ifelse(type %in% restaurants, "Restaurants", type))

food_businesses_sf <- 
  food_businesses_sf %>% 
  filter(type != "Ephemeral") %>% 
  #filter(type != "Institutional") %>% 
  #filter(type != "Industrial") %>% 
  filter(type != "Other") 

# Join with borough --------------------------------------------------

food_businesses_borough <- 
  food_businesses_sf %>% 
  st_join(boroughs, .) %>% 
  st_drop_geometry()

# Clean but with no geometry --------------------------------------------------

food_businesses <- 
  food_businesses_sf %>% 
  st_drop_geometry()

# Verify its clean ------------------------------------------------------------

unique(food_businesses$type)

# % ferme et % ouvert out of all categories -----------------------------------

total_status_by_borough <- 
  food_businesses_borough %>% 
  mutate(date = year(date)) %>% 
  filter(date>=2011 & date!=2021) %>% 
  filter(statut == "Fermé" | statut == "Ouvert") %>% 
  count(borough, statut, date) %>% 
  rename(total=n)

# Filter to not have 30830982082 plots

top_10_boroughs <- 
  boroughs %>% 
  mutate(dwelling_density = dwellings/st_area(geometry)) %>% 
  arrange(desc(dwelling_density)) %>% 
  slice(1:10) %>% 
  pull(borough)

# Gentrifying boroughs, without Ville-Marie 

boroughs_of_interest <- c("Côte-des-Neiges-Notre-Dame-de-Grâce", "Rosemont-La Petite-Patrie",
                          "Mercier-Hochelaga-Maisonneuve", "Villeray-Saint-Michel-Parc-Extension",
                          "Le Plateau-Mont-Royal", "Ahuntsic-Cartierville", "Le Sud-Ouest",
                          "Verdun")

food_businesses_borough %>%
  mutate(date = year(date)) %>%
  filter(date>=2011 & date!=2021) %>%
  filter(statut == "Fermé" | statut == "Ouvert") %>%
  count(borough, statut, date, type) %>%
  full_join(., total_status_by_borough, by = c("borough", "statut", "date")) %>%
  filter(borough %in% top_10_boroughs) %>%
  mutate(percentage = n/total) %>%
  filter(statut == "Ouvert") %>% 
  ggplot()+
  #geom_bar(aes(x=date, y=percentage, fill=type),position = "fill",stat = "identity")+ #Looks ugly
  geom_smooth(aes(x=date, y= percentage, color=type), se=FALSE)+
  facet_wrap(~borough)+
  scale_color_manual(name="Type of food\nbusiness",
                     values=col_palette[c(1:5)])+
  scale_y_continuous(name = "Percentage share by type",
                     labels=scales::percent)+
  theme_minimal()

# ratio open-to-close by year by borough by type ------------------------------

total_ferme_by_borough <- 
  food_businesses_borough %>% 
  mutate(date = year(date)) %>% 
  filter(date>=2011 & date!=2021) %>% 
  filter(statut == "Fermé") %>% 
  count(borough, date, type) %>% 
  rename(total_ferme=n)

total_ouvert_by_borough <- 
  food_businesses_borough %>% 
  mutate(date = year(date)) %>% 
  filter(date>=2011 & date!=2021) %>% 
  filter(statut == "Ouvert") %>% 
  count(borough, date, type) %>% 
  rename(total_ouvert=n)

full_join(total_ferme_by_borough, total_ouvert_by_borough, by = c("borough", "type", "date")) %>%
  filter(borough %in% top_10_boroughs) %>%
  mutate(total_ouvert = ifelse(is.na(total_ouvert), 0, total_ouvert)) %>% 
  mutate(total_ferme = ifelse(is.na(total_ferme), 0, total_ferme)) %>% 
  mutate(ratio_open_to_closed = total_ouvert/total_ferme) %>%
  mutate(positive_negative = total_ouvert - total_ferme) %>% 
  ggplot()+
  geom_smooth(aes(x=date, y=positive_negative, color=type), se=FALSE)+
  facet_wrap(~borough)+
  theme_minimal()

# Food businesses open in 2020 --------------------------------------------------

years <- 
  food_businesses %>% 
  filter(statut == "Ouvert") %>% 
  distinct(name, address, .keep_all = TRUE) %>% 
  mutate(year = year(date)) %>% 
  filter(year != 2021) %>% 
  mutate(duplicates = 2020 - year + 1) %>%
  mutate(name_address = paste(name, address)) %>% 
  group_by(name_address) %>% 
  expand(duplicates = seq(1:duplicates))

food_businesses_open <- 
  food_businesses %>% 
  filter(statut == "Ouvert") %>% 
  distinct(name, address, .keep_all = TRUE) %>% 
  mutate(year = year(date)) %>% 
  filter(year != 2021) %>% 
  mutate(name_address = paste(name, address))

# food_businesses %>% 
#   filter(statut != "Ouvert") %>% 
#   count(name, address) %>% View()
# distinct(name, address, .keep_all = TRUE)

food_businesses_2019 <- 
  full_join(food_businesses_open, years, by="name_address") %>% 
  mutate(year = year + (duplicates - 1)) %>%
  filter(year == 2019) %>% 
  left_join(., food_businesses_sf %>% select(id), by = "id") %>% 
  st_as_sf()


# Changes in type by address ------------------------------------------------------------

groups <- 
  food_businesses %>% 
  select(-city, -statut) %>% 
  distinct(address, date, .keep_all=TRUE) %>% 
  arrange(date) %>% 
  group_by(address) %>% 
  mutate(group_id = row_number()) 

group1 <- 
  groups %>% 
  filter(group_id == 1) %>% 
  select(-group_id)

group2 <- 
  groups %>% 
  filter(group_id == 2) %>% 
  select(-group_id)

group3 <- 
  groups %>% 
  filter(group_id == 3)  %>% 
  select(-group_id)

group4 <- 
  groups %>% 
  filter(group_id == 4)  %>% 
  select(-group_id)

group5 <- 
  groups %>% 
  filter(group_id == 5)  %>% 
  select(-group_id)

group6 <- 
  groups %>% 
  filter(group_id == 6)  %>% 
  select(-group_id)

group7 <- 
  groups %>% 
  filter(group_id == 7)  %>% 
  select(-group_id)

group8 <- 
  groups %>% 
  filter(group_id == 8)  %>% 
  select(-group_id)

group9 <- 
  groups %>% 
  filter(group_id == 9)  %>% 
  select(-group_id)

group10 <- 
  groups %>% 
  filter(group_id == 10)  %>% 
  select(-group_id)

group11 <- 
  groups %>% 
  filter(group_id == 11)  %>% 
  select(-group_id)

group12 <- 
  groups %>% 
  filter(group_id == 12)  %>% 
  select(-group_id)

group13 <- 
  groups %>% 
  filter(group_id == 13)  %>% 
  select(-group_id)

group14 <- 
  groups %>% 
  filter(group_id == 14)  %>% 
  select(-group_id)

group15 <- 
  groups %>% 
  filter(group_id == 15)  %>% 
  select(-group_id)

group1_2 <- right_join(group1, group2, by="address")
group2_3 <- right_join(group2, group3, by="address")
group3_4 <- right_join(group3, group4, by="address")
group4_5 <- right_join(group4, group5, by="address")
group5_6 <- right_join(group5, group6, by="address")
group6_7 <- right_join(group6, group7, by="address")
group7_8 <- right_join(group7, group8, by="address")
group8_9 <- right_join(group8, group9, by="address")
group9_10 <- right_join(group9, group10, by="address")
group10_11 <- right_join(group10, group11, by="address")
group11_12 <- right_join(group11, group12, by="address")
group12_13 <- right_join(group12, group13, by="address")
group13_14 <- right_join(group13, group14, by="address")
group14_15 <- right_join(group14, group15, by="address")

switches <- 
  rbind(group1_2, group2_3, group3_4, group4_5, group5_6,
        group6_7, group7_8, group8_9, group9_10, group10_11,
        group11_12, group12_13, group13_14, group14_15)

# grouped_addresses <- 
#   group1 %>% 
#   left_join(., group2, by = "address") %>% 
#   set_names(c("id1", "name1", "address", "type1", "date1", 
#               "id2", "name2", "type2", "date2")) %>% 
#   left_join(., group3, by = "address") %>% 
#   set_names(c("id1", "name1", "address", "type1", "date1", 
#               "id2", "name2", "type2", "date2", 
#               "id3", "name3", "type3", "date3")) %>% 
#   left_join(., group4, by = "address") %>% 
#   set_names(c("id1", "name1", "address", "type1", "date1", 
#               "id2", "name2", "type2", "date2", 
#               "id3", "name3", "type3", "date3", 
#               "id4", "name4", "type4", "date4")) %>% 
#   left_join(., group5, by = "address") %>% 
#   set_names(c("id1", "name1", "address", "type1", "date1", 
#               "id2", "name2", "type2", "date2", 
#               "id3", "name3", "type3", "date3", 
#               "id4", "name4", "type4", "date4",
#               "id5", "name5", "type5", "date5")) %>% 
#   left_join(., group6, by = "address") %>% 
#   set_names(c("id1", "name1", "address", "type1", "date1", 
#               "id2", "name2", "type2", "date2", 
#               "id3", "name3", "type3", "date3", 
#               "id4", "name4", "type4", "date4",
#               "id5", "name5", "type5", "date5",
#               "id6", "name6", "type6", "date6")) %>% 
#   left_join(., group7, by = "address") %>% 
#   set_names(c("id1", "name1", "address", "type1", "date1", 
#               "id2", "name2", "type2", "date2", 
#               "id3", "name3", "type3", "date3", 
#               "id4", "name4", "type4", "date4",
#               "id5", "name5", "type5", "date5",
#               "id6", "name6", "type6", "date6",
#               "id7", "name7", "type7", "date7")) %>% 
#   left_join(., group8, by = "address") %>% 
#   set_names(c("id1", "name1", "address", "type1", "date1", 
#               "id2", "name2", "type2", "date2", 
#               "id3", "name3", "type3", "date3", 
#               "id4", "name4", "type4", "date4",
#               "id5", "name5", "type5", "date5",
#               "id6", "name6", "type6", "date6",
#               "id7", "name7", "type7", "date7",
#               "id8", "name8", "type8", "date8")) %>% 
#   left_join(., group9, by = "address") %>% 
#   set_names(c("id1", "name1", "address", "type1", "date1", 
#               "id2", "name2", "type2", "date2", 
#               "id3", "name3", "type3", "date3", 
#               "id4", "name4", "type4", "date4",
#               "id5", "name5", "type5", "date5",
#               "id6", "name6", "type6", "date6",
#               "id7", "name7", "type7", "date7",
#               "id8", "name8", "type8", "date8",
#               "id9", "name9", "type9", "date9")) %>% 
#   left_join(., group10, by = "address") %>% 
#   set_names(c("id1", "name1", "address", "type1", "date1", 
#               "id2", "name2", "type2", "date2", 
#               "id3", "name3", "type3", "date3", 
#               "id4", "name4", "type4", "date4",
#               "id5", "name5", "type5", "date5",
#               "id6", "name6", "type6", "date6",
#               "id7", "name7", "type7", "date7",
#               "id8", "name8", "type8", "date8",
#               "id9", "name9", "type9", "date9",
#               "id10", "name10", "type10", "date10")) %>% 
#   left_join(., group11, by = "address") %>% 
#   set_names(c("id1", "name1", "address", "type1", "date1", 
#               "id2", "name2", "type2", "date2", 
#               "id3", "name3", "type3", "date3", 
#               "id4", "name4", "type4", "date4",
#               "id5", "name5", "type5", "date5",
#               "id6", "name6", "type6", "date6",
#               "id7", "name7", "type7", "date7",
#               "id8", "name8", "type8", "date8",
#               "id9", "name9", "type9", "date9",
#               "id10", "name10", "type10", "date10",
#               "id11", "name11", "type11", "date11")) %>% 
#   left_join(., group12, by = "address") %>% 
#   set_names(c("id1", "name1", "address", "type1", "date1", 
#               "id2", "name2", "type2", "date2", 
#               "id3", "name3", "type3", "date3", 
#               "id4", "name4", "type4", "date4",
#               "id5", "name5", "type5", "date5",
#               "id6", "name6", "type6", "date6",
#               "id7", "name7", "type7", "date7",
#               "id8", "name8", "type8", "date8",
#               "id9", "name9", "type9", "date9",
#               "id10", "name10", "type10", "date10",
#               "id11", "name11", "type11", "date11",
#               "id12", "name12", "type12", "date12")) %>% 
#   left_join(., group13, by = "address") %>% 
#   set_names(c("id1", "name1", "address", "type1", "date1", 
#               "id2", "name2", "type2", "date2", 
#               "id3", "name3", "type3", "date3", 
#               "id4", "name4", "type4", "date4",
#               "id5", "name5", "type5", "date5",
#               "id6", "name6", "type6", "date6",
#               "id7", "name7", "type7", "date7",
#               "id8", "name8", "type8", "date8",
#               "id9", "name9", "type9", "date9",
#               "id10", "name10", "type10", "date10",
#               "id11", "name11", "type11", "date11",
#               "id12", "name12", "type12", "date12",
#               "id13", "name13", "type13", "date13")) %>% 
#   left_join(., group13, by = "address") %>% 
#   set_names(c("id1", "name1", "address", "type1", "date1", 
#               "id2", "name2", "type2", "date2", 
#               "id3", "name3", "type3", "date3", 
#               "id4", "name4", "type4", "date4",
#               "id5", "name5", "type5", "date5",
#               "id6", "name6", "type6", "date6",
#               "id7", "name7", "type7", "date7",
#               "id8", "name8", "type8", "date8",
#               "id9", "name9", "type9", "date9",
#               "id10", "name10", "type10", "date10",
#               "id11", "name11", "type11", "date11",
#               "id12", "name12", "type12", "date12",
#               "id13", "name13", "type13", "date13",
#               "id14", "name14", "type14", "date14")) %>% 
#   left_join(., group13, by = "address") %>% 
#   set_names(c("id1", "name1", "address", "type1", "date1", 
#               "id2", "name2", "type2", "date2", 
#               "id3", "name3", "type3", "date3", 
#               "id4", "name4", "type4", "date4",
#               "id5", "name5", "type5", "date5",
#               "id6", "name6", "type6", "date6",
#               "id7", "name7", "type7", "date7",
#               "id8", "name8", "type8", "date8",
#               "id9", "name9", "type9", "date9",
#               "id10", "name10", "type10", "date10",
#               "id11", "name11", "type11", "date11",
#               "id12", "name12", "type12", "date12",
#               "id13", "name13", "type13", "date13",
#               "id14", "name14", "type14", "date14",
#               "id15", "name15", "type15", "date15"))
# 
# grouped_addresses %>% 
#   select(type1,type2,type3,type4,type5,type6,type7,type8,type9,type10) %>% View()

unique(food_businesses$type)

non_gentrifying <- c("General grocery", "Institutional", "Industrial")

gentrifying <- c("Restaurants", "Bars", "Specialized", "Cafes")

switches_sf <- 
  switches %>% 
  mutate(transition = ifelse(type.x %in% non_gentrifying & type.y %in% gentrifying, TRUE, FALSE)) %>% 
  filter(transition == TRUE) %>% 
  mutate(date.x = year(date.x),
         date.y = year(date.y),
         number_years = date.y-date.x) %>% 
  filter(number_years <5) %>% 
  left_join(., food_businesses_sf %>% select(address), by="address") %>%
  st_as_sf() %>% 
  distinct(id.x, .keep_all=TRUE) %>% 
  filter(date.y >=2011) 

#%>% 
#st_join(DA %>% select(GeoUID), .) %>% 
#filter(!is.na(id.x)) %>% 
#group_by(GeoUID, date.y) %>% 
#summarize(n=n()) 

#Number of switches from non-gentrifying to gentrified per year all one map
switches_sf %>% 
  filter(date.y < 2021) %>% 
  ggplot()+
  geom_sf(data=province, fill="grey90", color=NA) +
  geom_sf(data=boroughs, fill=NA, color="black")+
  geom_sf(aes(color=date.y), size=1, alpha=0.5)+
  scale_color_viridis_c(name="Date")+
  #facet_wrap(~date.y)+
  #upgo::gg_bbox(boroughs)+
  coord_sf(xlim = c(582274, 618631), ylim = c(5029928, 5062237),
           expand = FALSE)+
  theme_void()+
  ggtitle("Number of switches from non-gentrifying to gentrified per year")

#Number of switches per 1000 households
switches_sf %>% 
  st_join(boroughs, .) %>% 
  filter(date.y < 2021) %>% 
  mutate(dwellings_1000=dwellings/1000) %>% 
  group_by(borough, date.y, dwellings_1000) %>% 
  summarize(n=n(), n_density=n()/(sum(dwellings_1000)/n())) %>% 
  ggplot()+
  geom_sf(data=province, fill="grey90", color=NA) +
  geom_sf(data=boroughs, fill=NA, color="black")+
  geom_sf(aes(fill=n_density), color=NA)+
  scale_fill_gradientn(name="Number of switches\nper 1000 households",
                       colors=col_palette[c(4,1,9)])+
  facet_wrap(~date.y)+
 # upgo::gg_bbox(boroughs)+
  coord_sf(xlim = c(582274, 618631), ylim = c(5029928, 5062237),
           expand = FALSE)+
  theme_void()


#Number of switches per year
switches_sf %>% 
  st_join(boroughs, .) %>% 
  filter(date.y < 2021) %>% 
  mutate(dwellings_1000=dwellings/1000) %>% 
  group_by(borough, date.y, dwellings_1000) %>% 
  summarize(n=n()) %>% 
  ggplot()+
  geom_sf(data=province, fill="grey90", color=NA) +
  geom_sf(data=boroughs, fill=NA, color="black")+
  geom_sf(aes(fill=n), color=NA)+
  scale_fill_gradientn(name="Number of switches",
                       colors=col_palette[c(4,1,9)])+
  facet_wrap(~date.y)+
  #upgo::gg_bbox(boroughs)+
  coord_sf(xlim = c(582274, 618631), ylim = c(5029928, 5062237),
           expand = FALSE)+
  theme_void()

