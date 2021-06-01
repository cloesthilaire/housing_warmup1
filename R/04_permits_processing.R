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

condo_conversions %>% 
  select(-borough) %>% 
  st_join(boroughs, .) %>% 
  group_by(borough, issued_date) %>% 
  summarize(number_conversion=sum(n())) %>% 
  ggplot()+
  geom_sf(data=boroughs)+
  geom_sf(aes(fill=number_conversion))+
  scale_fill_stepsn(name="Number of conversions",
                    n.breaks = 5,
                    colours = col_palette[c(5,3,4,1,2,9)],
                    guide = "coloursteps",
                    na.value = "grey50")+
  theme_void()+
  facet_wrap(~issued_date)

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

combined_dwellings %>% 
  select(-borough) %>% 
  st_join(boroughs, .) %>% 
  group_by(borough, issued_date) %>% 
  summarize(number_combining=sum(nb_dwellings)) %>% 
  ggplot()+
  geom_sf(data=boroughs)+
  geom_sf(aes(fill=number_combining))+
  scale_fill_stepsn(name="Number of combinations",
                    n.breaks = 5,
                    colours = col_palette[c(5,3,4,1,2,9)],
                    guide = "coloursteps",
                    na.value = "grey50")+
  theme_void()+
  facet_wrap(~issued_date)


# Combining of two units into one ------------------------------------------------------------

permits %>% 
  filter(type == "CO") %>% 
  filter(category1 == "Residentiel"| category1 == "Mixte") %>% 
  filter(nb_dwellings >= 10) %>% 
  View()
# mutate(issued_date = floor_date(issued_date, "year")) %>% 
# ggplot()+
# geom_sf(data=boroughs_raw, fill=NA)+
# geom_sf(aes(size=nb_dwellings))+
# theme_void()+
# facet_wrap(~issued_date)




