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
  filter(type != "Institutional") %>% 
  filter(type != "Industrial") %>% 
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

grouped_addresses <- 
  group1 %>% 
  left_join(., group2, by = "address") %>% 
  set_names(c("id1", "name1", "address", "type1", "date1", 
              "id2", "name2", "type2", "date2")) %>% 
  left_join(., group3, by = "address") %>% 
  set_names(c("id1", "name1", "address", "type1", "date1", 
              "id2", "name2", "type2", "date2", 
              "id3", "name3", "type3", "date3")) %>% 
  left_join(., group4, by = "address") %>% 
  set_names(c("id1", "name1", "address", "type1", "date1", 
              "id2", "name2", "type2", "date2", 
              "id3", "name3", "type3", "date3", 
              "id4", "name4", "type4", "date4")) %>% 
  left_join(., group5, by = "address") %>% 
  set_names(c("id1", "name1", "address", "type1", "date1", 
              "id2", "name2", "type2", "date2", 
              "id3", "name3", "type3", "date3", 
              "id4", "name4", "type4", "date4",
              "id5", "name5", "type5", "date5")) %>% 
  left_join(., group6, by = "address") %>% 
  set_names(c("id1", "name1", "address", "type1", "date1", 
              "id2", "name2", "type2", "date2", 
              "id3", "name3", "type3", "date3", 
              "id4", "name4", "type4", "date4",
              "id5", "name5", "type5", "date5",
              "id6", "name6", "type6", "date6")) %>% 
  left_join(., group7, by = "address") %>% 
  set_names(c("id1", "name1", "address", "type1", "date1", 
              "id2", "name2", "type2", "date2", 
              "id3", "name3", "type3", "date3", 
              "id4", "name4", "type4", "date4",
              "id5", "name5", "type5", "date5",
              "id6", "name6", "type6", "date6",
              "id7", "name7", "type7", "date7")) %>% 
  left_join(., group8, by = "address") %>% 
  set_names(c("id1", "name1", "address", "type1", "date1", 
              "id2", "name2", "type2", "date2", 
              "id3", "name3", "type3", "date3", 
              "id4", "name4", "type4", "date4",
              "id5", "name5", "type5", "date5",
              "id6", "name6", "type6", "date6",
              "id7", "name7", "type7", "date7",
              "id8", "name8", "type8", "date8")) %>% 
  left_join(., group9, by = "address") %>% 
  set_names(c("id1", "name1", "address", "type1", "date1", 
              "id2", "name2", "type2", "date2", 
              "id3", "name3", "type3", "date3", 
              "id4", "name4", "type4", "date4",
              "id5", "name5", "type5", "date5",
              "id6", "name6", "type6", "date6",
              "id7", "name7", "type7", "date7",
              "id8", "name8", "type8", "date8",
              "id9", "name9", "type9", "date9")) %>% 
  left_join(., group10, by = "address") %>% 
  set_names(c("id1", "name1", "address", "type1", "date1", 
              "id2", "name2", "type2", "date2", 
              "id3", "name3", "type3", "date3", 
              "id4", "name4", "type4", "date4",
              "id5", "name5", "type5", "date5",
              "id6", "name6", "type6", "date6",
              "id7", "name7", "type7", "date7",
              "id8", "name8", "type8", "date8",
              "id9", "name9", "type9", "date9",
              "id10", "name10", "type10", "date10")) %>% 
  left_join(., group11, by = "address") %>% 
  set_names(c("id1", "name1", "address", "type1", "date1", 
              "id2", "name2", "type2", "date2", 
              "id3", "name3", "type3", "date3", 
              "id4", "name4", "type4", "date4",
              "id5", "name5", "type5", "date5",
              "id6", "name6", "type6", "date6",
              "id7", "name7", "type7", "date7",
              "id8", "name8", "type8", "date8",
              "id9", "name9", "type9", "date9",
              "id10", "name10", "type10", "date10",
              "id11", "name11", "type11", "date11")) %>% 
  left_join(., group12, by = "address") %>% 
  set_names(c("id1", "name1", "address", "type1", "date1", 
              "id2", "name2", "type2", "date2", 
              "id3", "name3", "type3", "date3", 
              "id4", "name4", "type4", "date4",
              "id5", "name5", "type5", "date5",
              "id6", "name6", "type6", "date6",
              "id7", "name7", "type7", "date7",
              "id8", "name8", "type8", "date8",
              "id9", "name9", "type9", "date9",
              "id10", "name10", "type10", "date10",
              "id11", "name11", "type11", "date11",
              "id12", "name12", "type12", "date12")) %>% 
  left_join(., group13, by = "address") %>% 
  set_names(c("id1", "name1", "address", "type1", "date1", 
              "id2", "name2", "type2", "date2", 
              "id3", "name3", "type3", "date3", 
              "id4", "name4", "type4", "date4",
              "id5", "name5", "type5", "date5",
              "id6", "name6", "type6", "date6",
              "id7", "name7", "type7", "date7",
              "id8", "name8", "type8", "date8",
              "id9", "name9", "type9", "date9",
              "id10", "name10", "type10", "date10",
              "id11", "name11", "type11", "date11",
              "id12", "name12", "type12", "date12",
              "id13", "name13", "type13", "date13")) %>% 
  left_join(., group13, by = "address") %>% 
  set_names(c("id1", "name1", "address", "type1", "date1", 
              "id2", "name2", "type2", "date2", 
              "id3", "name3", "type3", "date3", 
              "id4", "name4", "type4", "date4",
              "id5", "name5", "type5", "date5",
              "id6", "name6", "type6", "date6",
              "id7", "name7", "type7", "date7",
              "id8", "name8", "type8", "date8",
              "id9", "name9", "type9", "date9",
              "id10", "name10", "type10", "date10",
              "id11", "name11", "type11", "date11",
              "id12", "name12", "type12", "date12",
              "id13", "name13", "type13", "date13",
              "id14", "name14", "type14", "date14")) %>% 
  left_join(., group13, by = "address") %>% 
  set_names(c("id1", "name1", "address", "type1", "date1", 
              "id2", "name2", "type2", "date2", 
              "id3", "name3", "type3", "date3", 
              "id4", "name4", "type4", "date4",
              "id5", "name5", "type5", "date5",
              "id6", "name6", "type6", "date6",
              "id7", "name7", "type7", "date7",
              "id8", "name8", "type8", "date8",
              "id9", "name9", "type9", "date9",
              "id10", "name10", "type10", "date10",
              "id11", "name11", "type11", "date11",
              "id12", "name12", "type12", "date12",
              "id13", "name13", "type13", "date13",
              "id14", "name14", "type14", "date14",
              "id15", "name15", "type15", "date15"))

grouped_addresses %>% 
  select(type1,type2,type3,type4,type5,type6,type7,type8,type9,type10) %>% View()
