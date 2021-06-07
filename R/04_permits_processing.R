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


# Combining of two units into one ------------------------------------------------------------

new_construction <- 
  permits %>% 
  filter(type == "CO") %>% 
  filter(category1 == "Residentiel"| category1 == "Mixte") %>% 
  filter(nb_dwellings >= 10) %>% 
  mutate(new_constru = ifelse(str_detect(text, "const") & str_detect(text, "batiment"), TRUE, FALSE)) %>%
  mutate(new_constru = ifelse(str_detect(text, "const") & str_detect(text, "immeuble"), TRUE, new_constru)) %>% 
  mutate(new_constru = ifelse(str_detect(text, "const") & str_detect(text, "residence"), TRUE, new_constru)) %>% 
  mutate(new_constru = ifelse(str_detect(text, "multi") & str_detect(text, "log"), TRUE, new_constru)) %>% 
  mutate(new_constru = ifelse(str_detect(text, "bat") & str_detect(text, "log"), TRUE, new_constru)) %>% 
  mutate(new_constru = ifelse(str_detect(text, "batiment") & str_detect(text, "habitation"), TRUE, new_constru)) %>% 
  mutate(new_constru = ifelse(str_detect(text, "nouve") & str_detect(text, "const"), TRUE, new_constru)) %>% 
  mutate(new_constru = ifelse(str_detect(text, "condo"), TRUE, new_constru)) %>% 
  mutate(new_constru = ifelse(str_detect(text, "coop"), TRUE, new_constru)) %>% 
  filter(new_constru == TRUE) %>%
  mutate(issued_date = year(issued_date))
    




