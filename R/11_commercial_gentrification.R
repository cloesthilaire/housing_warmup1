# Step 1: Load datasets --------------------------------------------------------

# Load libraries
library(dplyr)

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
  

# 1988=10, 1989=28, 1990=35, 1991=16
# 1992=12, 1993=1460, 1994=93, 1995=105, 1996=84, 1997=72, 1998 = 93, 1999= 86,
# 2000= 121, 2001=171, 2002=234, 2003=182, 2004=201, 2005=147, 2006=353, 2007=817,
# 2008=1841, 2009=2162, 2010=2547, 2011=2017, 2012=2731, 2013=2705, 2014=2835,
# 2015=3030, 2016=2931, 2017=2882, 2018=3628, 2019=4161, 2020=3334, 2021=1807


# Determining how many businesses per category
food_businesses%>% 
  count(type) %>% 
  View()

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

food_businesses_kosta_boroughs %>% 
  mutate(date = year(date)) %>% 
  #need to do something else with the date
  filter(statut %in% open_closed_kosta) %>% 
  group_by(borough,date, statut, type) %>% 
  summarize(statut_per_borough= n()) %>% 
  mutate(statut_per_borough_per_100_dwellings = statut_per_borough/(dwellings/100)) %>%
  ggplot()+
  geom_histogram(aes(x = date, fill = statut), stat="count")+
    facet_wrap(borough)




  
# Statistical Analysis ---------------------------------------------------------

# Bar charts


# This graph works

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



