#### 03 PERMITS IMPORT AND CLEANING ########################################################

source("R/01_startup.R")
library(stringr)

# Download permit dataset -------------------------------------------------------------

permits_raw <-
  read_sf("data/permis-construction/permis-construction.shp") %>%
  st_transform(32618) %>%
  as_tibble() %>%
  st_as_sf()

permits <- 
  permits_raw %>% 
  select(-c(longitude:loc_y)) %>% 
  set_names(c("no_demande", "id", "start_date", "issued_date",
              "address", "borough", "type", "description", "category1",
              "category2", "text", "nb_dwellings", "geometry"))

permits <- 
  permits %>% 
  mutate(description = ifelse(str_detect(description, "Construc|construc|CONSTRUC|Nouveau bâtiment|Mur de soutènement"), "Construction", description)) %>% 
  mutate(description = ifelse(str_detect(description, "Transfo|transfo|Transf|MO PRAM Decarie Modif Int/ Ext|A1:Taux implantation modifié|Mise aux normes|A2:Taux implantation inchangé|MO Modif Int & Ext|AI - Aménagement intérieur|Permis d'améliorations|RP- réparation|MI PRAM Decarie Modif Int"), "Transformation", description)) %>% 
  mutate(description = ifelse(str_detect(description, "DÉMO|Démo|démo|DEMO|Z - Déménag Bat Princ HP"), "Demolition", description)) %>% 
  mutate(description = ifelse(str_detect(description, "AGRANDISSEMENT|Agrandissement|agrandissement"), "Agrandissement", description)) %>% 
  mutate(description = ifelse(str_detect(description, "Abattage|abattage"), "Abattage", description)) %>% 
  mutate(description = ifelse(str_detect(description, "Dégarnissage intérieur|Travaux intérieurs|MI Modifications Intérieures|Réaménagement de local|Rénovation"), "Renovation interieures", description)) %>% 
  mutate(description = ifelse(str_detect(description, "ML Modif LOGEMENT ADD SS|Lotissement|LS - Logement sous-sol|Nouveau logement"), "Ajout/Combination logement", description)) %>% 
  mutate(description = ifelse(str_detect(description, "Piscine|piscine|PISCINE|spa|Spa"), "Piscine et spa", description)) %>% 
  mutate(description = ifelse(str_detect(description, "Affichage|Enseigne|enseigne|affichage|ENSEIGNE|ET Etalage Extérieur|Z - Panneau Réclame HP"), "Affichage", description)) %>% 
  mutate(description = ifelse(str_detect(description, "Garage|garage|abri|Abri|Cabanon|CABANON|cabanon|Solarium|Batiment Accessoire|Z - Déménag Dépendance HP|EM- Embarcadère|ACCESSOIRE|accessoire|Bâtiment temporaire|Temporaire|remorque|Remise|remise|BÂTIMENTS ACCESSOIRES|EA Equipement Accessoire"), "Garage/Access/Bat. Temporaire", description)) %>%
  mutate(description = ifelse(str_detect(description, "Vente|Foire|vente|Foire|promo|Sollicitation|Place de marché"), "Permis vente", description)) %>% 
  mutate(description = ifelse(str_detect(description, "Chat|chat|Chien|chien|CHATS"), "Permis animaux", description)) %>% 
  mutate(description = ifelse(str_detect(description, "Clôture|Cloture|cloture|clôture|CLÔTURE|muret|pieux|Occupation permanente"), "Cloture/Muret", description)) %>% 
  mutate(description = ifelse(str_detect(description, "Stationnement|stationnement|Fond compensation stat."), "Stationnement", description)) %>% 
  mutate(description = ifelse(str_detect(description, "Antenne|antenne|Satellite|satellite|ANTENNE"), "Antenne/Satellite", description)) %>% 
  mutate(description = ifelse(str_detect(description, "Terrasse|terrasse"), "Terrasse", description)) %>% 
  mutate(description = ifelse(str_detect(description, "TR- transport de bâtiments|DB Déplacement Batiment"), "Transport batiment", description)) %>% 
  mutate(description = ifelse(str_detect(description, "Décontaminer|Décontamination|Decontamination|DÉCONTAMINATION"), "Decontamination", description)) %>% 
  mutate(description = ifelse(str_detect(description, "Renouvellement|Certificat d'autorisation|Autres Certificats|Certificat Autre|Droit acquis"), "Certificats et renouvellement", description)) %>% 
  mutate(description = ifelse(str_detect(description, "Domaine Public|domaine public|dom. pub|Règl. Art. 89 Charte|T6: ATT. EN MODIFICATION|Recherche de plans|Etude préliminaire"), "Domaine public", description)) %>% 
  mutate(description = ifelse(str_detect(description, "Drain|Égout|Aqueduc|RACCORD|Raccord|raccord|Plomberie|Sanitaire|sanitaire"), "Plomberie/Raccord/Sanitaire", description)) %>%  
  mutate(description = ifelse(str_detect(description, "Boites de Dons|Boite de Dons|CC - Conteneur de cueillettes|Boites de dons|Conteneur de dons|Boîte récupération tissus|Boites de dons|Boîte de dons"), "Boites de dons", description)) %>% 
  mutate(description = ifelse(str_detect(description, "adresse civique|Numérotage|Adresse civ|Numéro Civique|numéro civique|Numéro civique"), "Adresse civique, demande", description)) %>% 
  mutate(description = ifelse(str_detect(description, "RÉSERVOIRS|Réservoir|réservoir|PU- puits"), "Reservoir", description)) %>% 
  mutate(description = ifelse(str_detect(description, "CA CERTIFICATS|Z-Autre - Const Accessoire HP"), "Thermopompe/Gaz", description)) %>% 
  mutate(description = ifelse(str_detect(description, "Art mural"), "Art mural", description)) %>% 
  mutate(description = ifelse(str_detect(description, "Entrée charretière|Entrée Charretière|Entrée de service|Aire de chargement"), "Entrees", description)) %>% 
  mutate(description = ifelse(str_detect(description, "Excavation|remblai|Remblais|Remblai"), "Excavation/Remblais", description)) %>% 
  mutate(description = ifelse(str_detect(description, "Renaturalisation|renaturalisation|Stabilisation de rive|bande riveraine"), "Renaturalisation", description)) %>% 
  mutate(description = ifelse(str_detect(description, "pesticide"), "Pesticide", description)) %>% 
  mutate(description = ifelse(str_detect(description, "Occupation temporaire|Occupation périodique|ce type de permis n'est plus v"), "Occupation temporaire", description)) %>% 
  mutate(description = ifelse(str_detect(description, "PPCMOI|Changement d'usage|Dérogation copropriété divise|Dérogation mineure|Usage conditionnel"), "Derogations", description)) %>% 
  mutate(description = ifelse(str_detect(description, "sinistre|incendie"), "Reparations post sinistre", description)) %>% 
  mutate(description = ifelse(str_detect(description, "AP Aménagement Paysager|aménagement paysager|Modification terrain|Aménagement du terrain|Travaux extérieurs|ME Modifications Extérieures|ME PRAM Decarie Modif Ext|MA Modif Ext - Acc: Resid|MA Modif Ext - Acc: Comm Ind|Aménagement de terrain|ME PRAM Poirier Modif Ext|Z-Autre - Amélioration HP"), "Travaux exterieurs", description)) %>% 
  mutate(description = ifelse(str_detect(description, "Appareil mécanique|EQ Equip Elect/Méc: Resid|Équipement mécanique|APPAREILS MÉCANIQUE|TG - Travaux de génie|EQ Equip Elect/Méc: Comm Ind"), "Mecanique/Electrique", description))

permits <- 
  permits %>% 
  mutate(category1 = ifelse(str_detect(category1, "Commercial|Commerce|commercial|COMMERCE"), "Commercial", category1)) %>%
  mutate(category1 = ifelse(str_detect(category1, "Résidentiel - Mixte|Résidentiel-Mixte|Usage Multiple"), "Mixte", category1)) %>%
  mutate(category1 = ifelse(str_detect(category1, "Industriel|INDUSTRIE|Industrie|industriel"), "Industriel", category1)) %>% 
  mutate(category1 = ifelse(str_detect(category1, "PUBLIC|Public|public|publique"), "Public", category1)) %>% 
  mutate(category1 = ifelse(str_detect(category1, "Résidentiel|Habitation|habitation|HABITATION|bâtiment accessoire"), "Residentiel", category1)) %>% 
  mutate(category1 = ifelse(str_detect(category1, "Institutionnel|collectif"), "Institutionnel", category1)) %>% 
  mutate(category1 = ifelse(str_detect(category1, "Autre|agricole|Non"), "Autre", category1)) %>% 
  mutate(category1 = ifelse(str_detect(category1, "Parc|PARC"), "Parcs et espaces verts", category1)) %>% 
  mutate(category1 = ifelse(str_detect(category1, "VACANT|Vacant"), "Terrain vacant", category1)) 

permits1 <- 
  permits %>% 
  mutate(category2 = ifelse(str_detect(category2, "4 à 8 logements|4 Logements|Multifamilial|(4logs)|(5à11logs)|multifamilial|quadruplex|Multi|5 à 11 Logements|12 Logements +|8 à 12 Logements|Condominium|Condos|36 logements et plus|12 à 36 logements"), "Multifamilial", category2)) %>% 
  mutate(category2 = ifelse(str_detect(category2, "Commercial|Commer léger|commercial|Commerce|commerce|Bureaux|Bureau|bureau|commerciaux|C1 Léger"), "Commercial", category2)) %>% 
  mutate(category2 = ifelse(str_detect(category2, "1 Logement|Unifamilial|unifamilial|Bungalow|Cottage"), "Unifamilial", category2)) %>% 
  mutate(category2 = ifelse(str_detect(category2, "Garage|accessoire|garage|Remise|Abri"), "Garage/Bat. Access.", category2)) %>% 
  mutate(category2 = ifelse(str_detect(category2, "Bifamilial|Trifamilial|Trif|Bif|2 Logements|Jumelée|3 Logements|trifamilial|(3logs)|Duplex|Triplex"), "Bi- et trifamilial", category2)) %>% 
  mutate(category2 = ifelse(str_detect(category2, "Culte|culte"), "Culte", category2)) %>% 
  mutate(category2 = ifelse(str_detect(category2, "pétroliers|industrie|Industrie|lourd|Manufactu|exploitation"), "Industriel", category2)) %>% 
  mutate(category2 = ifelse(str_detect(category2, "communautaire|Communautaire|civil"), "Communautaire", category2)) %>% 
  mutate(category2 = ifelse(str_detect(category2, "École|Université|Enseignement|Collegial|Collégial|enseignement|Garderie"), "Scolaire et garderie", category2)) %>% 
  mutate(category2 = ifelse(str_detect(category2, "gouvernemental|gouv|municipal|Gouv"), "Gouvernemental", category2)) %>% 
  mutate(category2 = ifelse(str_detect(category2, "Institu|institu|Administration"), "Institutionnel", category2)) %>% 
  mutate(category2 = ifelse(str_detect(category2, "Personnes agées|retraite|résidence|personnes âgées"), "Retraite", category2)) %>% 
  mutate(category2 = ifelse(str_detect(category2, "Mixte|mixte|multiple|Résid. & Comm."), "Mixte", category2)) %>%
  mutate(category2 = ifelse(str_detect(category2, "Parc|parc|Berge|Golf|conservation (p4)"), "Parcs et espaces verts", category2)) %>%
  mutate(category2 = ifelse(str_detect(category2, "Vacant|vacant|VACANT"), "Terrains et lots vacants", category2)) %>%
  mutate(category2 = ifelse(str_detect(category2, "publics|public|loisir|récréa|Récréation|sportif|récréation|PUBLIC"), "Public et recreatif", category2)) %>% 
  mutate(category2 = ifelse(str_detect(category2, "alcool|Restauration|restauration"), "Restaurants et bars", category2)) %>% 
  mutate(category2 = ifelse(str_detect(category2, "Agricole|agriculture"), "Agricole", category2)) %>% 
  mutate(category2 = ifelse(str_detect(category2, "hébergement|chambre|d'héb"), "Centres hebergement", category2)) %>% 
  mutate(category2 = ifelse(str_detect(category2, "Batiments en hauteur >6 étages|Résidentiel|Habitation collective|chalet|Residentiel"), "Residentiel, general", category2)) %>% 
  mutate(category2 = ifelse(str_detect(category2, "Santé|hospitalier"), "Sante et hopitaux", category2)) %>% 
  mutate(category2 = ifelse(str_detect(category2, "Piscine|paysager"), "Amenagements exterieurs", category2)) %>% 
  mutate(category2 = ifelse(str_detect(category2, "détail|Vente|services|Services"), "Services et commerce au detail", category2)) %>% 
  mutate(category2 = ifelse(str_detect(category2, "Carburant|essence|Pétrolier|Transport|véhicule|Hotel|Véhicule|station-service"), "Transport et auxiliaires", category2))


unique(permits1$category2)