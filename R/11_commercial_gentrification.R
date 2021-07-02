# Step 1: Load datasets --------------------------------------------------------
# Load the CT and DA datasets

load("output/CTALEXIA.Rdata")
load("output/DA.Rdata")
source("R/01_startup.R")
load("output/province.Rdata")

# Load the Ãtablissements alimentaires data from DonnÃ©es Montreal
# dataset is from 1988 to 2021.06.28

food_businesses_sf <-
  read_sf("data/businesses/businesses.shp") %>% 
  st_transform(32618) %>% 
  mutate(DATE = ymd(date_statu)) %>% 
  mutate_at(vars(DATE), funs(year, month, day))

food_businesses <-
  food_businesses_sf %>% 
  st_drop_geometry()

#Save output -------------------------------------------------------------

save(food_business, file = "output/food_business")

# This dataset has classified businesses into the following categories: 
# Ãcole/mesure alimentaires, Ãpicerie, Ã©picerie avec prÃ©paration, Ã©vÃ¨nement spÃ©ciaux, 
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

food_businesses_sf %>% 
  filter(year == 2005) %>% 
  count(statut)
  

# repeat this process for every year to find: 1988=10, 1989=28, 1990=35, 1991=16
# 1992=12, 1993=1460, 1994=93, 1995=105, 1996=84, 1997=72, 1998 = 93, 1999= 86,
# 2000= 121, 2001=171, 2002=234, 2003=182, 2004=201, 2005=147, 2006=353, 2007=817,
# 2008=1841, 2009=2162, 2010=2547, 2011=2017, 2012=2731, 2013=2705, 2014=2835,
# 2015=3030, 2016=2931, 2017=2882, 2018=3628, 2019=4161, 2020=3334, 2021=1807

# Map 1: 1988-1993
# 1561 datapoints

# 1996 to do gentrification start
# temporal coverage 2011 a aujourd'hui

food_businesses_sf %>% 
  filter(year <=1993) %>% 
  #group_by(latitude, longitude) %>% 
  #summarize(n_businesses_1988_1993=n()) %>% 
  ggplot()+
  geom_sf(data=province, fill="grey90", color=NA)+
  geom_sf(aes(),color=NA)+
  #geom_sf(aes(n_businesses_1988_1993),color=NA)+
  #scale_fill_gradientn(name="Crimes per 100 people",
  #colors=col_palette[c(4,1,9)],
  #limits = c(0,2 ), 
  #oob = scales::squish, 
  #na.value = "grey90")+
  #coord_sf(xlim=c(582280,618631), ylim=c(5029848, 5062237), expand=FALSE)+
  #ggtitle("Mischief crimes per 100 people (Census Tract level)")+
  theme_void()+facet_wrap(~type, ~statut)

