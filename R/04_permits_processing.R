#### 04 PERMITS PROCESSING ########################################################

source("R/01_startup.R")
load("output/geometry.Rdata")
load("output/permits.Rdata")


# Condo conversions ------------------------------------------------------------

condo_conversions <- 
  permits %>% 
  mutate(conversion = str_detect(text, "transfo") & str_detect(text, "copropri")) %>% 
  mutate(conversion = ifelse(str_detect(text, "conver") & str_detect(text, "copropri"), TRUE, conversion)) %>%
  mutate(conversion = ifelse(str_detect(text, "transfo") & str_detect(text, "condo"), TRUE, conversion)) %>%
  mutate(conversion = ifelse(str_detect(text, "ream") & str_detect(text, "condo"), TRUE, conversion)) %>%
  filter(conversion==TRUE) %>%
  mutate(issued_date = year(issued_date))


# Combining of two units into one ------------------------------------------------------------

combined_dwellings <- 
  permits %>% 
  filter(nb_dwellings < 0) %>% 
  filter(category1 == "Residentiel" | category1 == "Mixte") %>% 
  mutate(combining = str_detect(text, "transfo")) %>% 
  mutate(combining = ifelse(str_detect(text, "reuni") & str_detect(text, "redu"), TRUE, combining)) %>%
  mutate(combining = ifelse(str_detect(text, "ream") & str_detect(text, "redu"), TRUE, combining)) %>%
  mutate(combining = ifelse(str_detect(text, "redu") & str_detect(text, "log"), TRUE, combining)) %>%
  mutate(combining = ifelse(str_detect(text, "ream") & str_detect(text, "redu"), TRUE, combining)) %>%
  mutate(combining = ifelse(str_detect(text, "loge") & str_detect(text, "conver"), TRUE, combining)) %>%
  mutate(combining = ifelse(str_detect(text, "modif") & str_detect(text, "suppression") & str_detect(text, "civique"), TRUE, combining)) %>%
  mutate(combining = ifelse(str_detect(text, "loge") & str_detect(text, "diminuer"), TRUE, combining)) %>%
  mutate(combining = ifelse(str_detect(text, "ream") & str_detect(text, "typologie"), TRUE, combining)) %>%
  mutate(combining = ifelse(str_detect(text, "ream") & str_detect(text, "unifamilial"), TRUE, combining)) %>%
  mutate(combining = ifelse(str_detect(text, "amenager") & str_detect(text, "plex") & str_detect(text, "unifamilial"), TRUE, combining)) %>%
  mutate(combining = ifelse(str_detect(text, "amenager") & str_detect(text, "unifamilial"), TRUE, combining)) %>%
  mutate(combining = ifelse(str_detect(text, "enlever") & str_detect(text, "loge"), TRUE, combining)) %>%
  mutate(combining = ifelse(str_detect(text, "jumeler"), TRUE, combining)) %>%
  mutate(combining = ifelse(str_detect(text, "fusion"), TRUE, combining)) %>%
  mutate(combining = ifelse(str_detect(text, "transf") & str_detect(text, "log"), TRUE, combining)) %>%
  mutate(combining = ifelse(str_detect(text, "transf") & str_detect(text, "commerce"), FALSE, combining)) %>%
  mutate(combining = ifelse(str_detect(text, "transf") & str_detect(text, "garderie"), FALSE, combining)) %>%
  mutate(combining = ifelse(str_detect(text, "transf") & str_detect(text, "hotel"), FALSE, combining)) %>%
  mutate(combining = ifelse(str_detect(text, "transf") & str_detect(text, "bureau"), FALSE, combining)) %>%
  filter(combining==TRUE) %>%
  mutate(issued_date = year(issued_date))


# New construction, permits data ------------------------------------------------------------

# new_construction <- 
#   permits %>% 
#   filter(type == "CO") %>% 
#   filter(category1 == "Residentiel"| category1 == "Mixte") %>% 
#   filter(nb_dwellings >= 10) %>% 
#   mutate(new_constru = ifelse(str_detect(text, "const") & str_detect(text, "batiment"), TRUE, FALSE)) %>%
#   mutate(new_constru = ifelse(str_detect(text, "const") & str_detect(text, "immeuble"), TRUE, new_constru)) %>% 
#   mutate(new_constru = ifelse(str_detect(text, "const") & str_detect(text, "residence"), TRUE, new_constru)) %>% 
#   mutate(new_constru = ifelse(str_detect(text, "multi") & str_detect(text, "log"), TRUE, new_constru)) %>% 
#   mutate(new_constru = ifelse(str_detect(text, "bat") & str_detect(text, "log"), TRUE, new_constru)) %>% 
#   mutate(new_constru = ifelse(str_detect(text, "batiment") & str_detect(text, "habitation"), TRUE, new_constru)) %>% 
#   mutate(new_constru = ifelse(str_detect(text, "nouve") & str_detect(text, "const"), TRUE, new_constru)) %>% 
#   mutate(new_constru = ifelse(str_detect(text, "condo"), TRUE, new_constru)) %>% 
#   mutate(new_constru = ifelse(str_detect(text, "coop"), TRUE, new_constru)) %>% 
#   filter(new_constru == TRUE) %>%
#   mutate(issued_date = year(issued_date))


# Demolition permits ------------------------------------------------------------

demolitions <- 
  permits %>% 
  filter(type == "DE") %>% 
  #filter(category1 == "Residentiel"| category1 == "Mixte") %>% 
  filter(!str_detect(text, "hangar")) %>% 
  filter(!str_detect(text, "piscine") & !str_detect(text, "demolir")) %>% 
  filter(!str_detect(text, "garage") & !str_detect(text, "demo")) %>% 
  filter(category2 != "Garage/Bat. Access.") %>% 
  mutate(issued_date = year(issued_date))
    

# Renovation permits ------------------------------------------------------------

renovations <- 
  permits %>% 
  filter(type == "TR") %>% 
  filter(category1 == "Residentiel"| category1 == "Mixte") %>% 
  filter(description != "Abattage") %>% 
  filter(!str_detect(text, "galerie")) %>% 
  filter(!str_detect(text, "patio")) %>% 
  filter(!str_detect(text, "perron")) %>% 
  filter(!str_detect(text, "toiture")) %>% 
  filter(!str_detect(text, "thermopompe")) %>% 
  filter(!str_detect(text, "fissure") & !str_detect(text, "fondation")) %>% 
  filter(!str_detect(text, "remplac") & !str_detect(text, "drain")) %>% 
  filter(!str_detect(text, "refection") & !str_detect(text, "membrane")) %>% 
  filter(!str_detect(text, "fissure") & !str_detect(text, "fondation")) %>% 
  filter(!str_detect(text, "refection") & !str_detect(text, "toit")) %>% 
  filter(!str_detect(text, "pieux") & !str_detect(text, "structure")) %>% 
  filter(!str_detect(text, "remplacer") & !str_detect(text, "revetement") & !str_detect(text, "toit")) %>% 
  filter(!str_detect(text, "rempla") & !str_detect(text, "plat") & !str_detect(text, "toit")) %>%
  mutate(issued_date = year(issued_date))
  

# New construction, uef data --------------------------------------------------------

new_construction <- 
  uef %>% 
  filter(!is.na(NOMBRE_LOG)) %>% 
  filter(NOMBRE_LOG > 0) %>% 
  filter(ANNEE_CONS >= 1990) %>% 
  filter(ANNEE_CONS <= 2020) %>%  
  group_by(CIVIQUE_DE, CIVIQUE_FI, NOM_RUE, ANNEE_CONS) %>% 
  summarise(number_dwellings = sum(NOMBRE_LOG, na.rm = TRUE)) 


# Save output -----------------------------------------------------------------------

save(condo_conversions, combined_dwellings, demolitions, new_construction, renovations,
     file = "output/permits_processed.Rdata")



