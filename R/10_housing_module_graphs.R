######### 10 GRAPHICS TO INCLUDE IN THE HOUSING MODULE ########################################

library(gganimate)
library(transformr)

load("output/geometry.Rdata")
load("output/cmhc.Rdata")
load("output/LL.Rdata")

# Graph 1: Permits emitted for combinations ----------------------------------------------------------

combination_map <- 
  combined_dwellings %>% 
  filter(issued_date <= 2020) %>% 
  mutate(date_range = ifelse(issued_date >= 1990 & issued_date < 1995, "[1990-1995[", issued_date)) %>% 
  mutate(date_range = ifelse(issued_date >= 1995 & issued_date < 2000, "[1995-2000[", date_range)) %>%
  mutate(date_range = ifelse(issued_date >= 2000 & issued_date < 2005, "[2000-2005[", date_range)) %>% 
  mutate(date_range = ifelse(issued_date >= 2005 & issued_date < 2010, "[2005-2010[", date_range)) %>% 
  mutate(date_range = ifelse(issued_date >= 2010 & issued_date < 2015, "[2010-2015[", date_range)) %>% 
  mutate(date_range = ifelse(issued_date >= 2015 & issued_date <= 2020, "[2015-2020]", date_range)) %>% 
  select(-borough) %>% 
  st_join(boroughs, .) %>% 
  group_by(date_range, borough) %>% 
  summarize(number_units_lost = sum(nb_dwellings, na.rm=TRUE)) %>% 
  filter(!is.na(date_range)) %>% 
  ggplot()+
  geom_sf(data = province, fill="grey90", color=NA)+
  geom_sf(data = boroughs, fill="grey80", color="grey75")+
  geom_sf(aes(fill=number_units_lost), color=NA)+
  scale_fill_gradientn(name="Change in units",
                       colors=col_palette[c(9, 1, 4)])+
  gg_bbox(boroughs)+
  ggtitle("Loss of units resulting from combinations")+
  facet_wrap(~date_range)+
  theme_void()

ggsave("output/figures/combination_map .pdf", plot = combination_map, width = 8, 
       height = 4.2, units = "in", useDingbats = FALSE)

# Graph 2: Permits emitted for condo conversions ----------------------------------------------------------

conversion_map <- 
  condo_conversions %>% 
  filter(issued_date <= 2020) %>% 
  mutate(date_range = ifelse(issued_date >= 1990 & issued_date < 1995, "[1990-1995[", issued_date)) %>% 
  mutate(date_range = ifelse(issued_date >= 1995 & issued_date < 2000, "[1995-2000[", date_range)) %>%
  mutate(date_range = ifelse(issued_date >= 2000 & issued_date < 2005, "[2000-2005[", date_range)) %>% 
  mutate(date_range = ifelse(issued_date >= 2005 & issued_date < 2010, "[2005-2010[", date_range)) %>% 
  mutate(date_range = ifelse(issued_date >= 2010 & issued_date < 2015, "[2010-2015[", date_range)) %>% 
  mutate(date_range = ifelse(issued_date >= 2015 & issued_date <= 2020, "[2015-2020]", date_range)) %>% 
  select(-borough) %>% 
  st_join(boroughs, .) %>% 
  group_by(date_range, borough) %>% 
  summarize(number_conversions = n()) %>% 
  filter(!is.na(date_range)) %>% 
  ggplot()+
  geom_sf(data = province, fill="grey90", color=NA)+
  geom_sf(data = boroughs, fill="grey80", color="grey75")+
  geom_sf(aes(fill=number_conversions), color=NA)+
  scale_fill_gradientn(name="Number of condo\nconversions",
                       colors=col_palette[c(4, 1, 9)])+
  gg_bbox(boroughs)+
  ggtitle("Number of condominium conversions")+
  facet_wrap(~date_range)+
  theme_void()

ggsave("output/figures/conversion_map .pdf", plot = conversion_map, width = 8, 
       height = 4.2, units = "in", useDingbats = FALSE)


# Graph 3: Number of new units built ----------------------------------------------------------

construction_map <- 
  LL_sf_centroid %>% 
  filter(!is.na(nombre_logements)) %>% 
  filter(nombre_logements > 0) %>% 
  filter(annee_construction >= 1990) %>% 
  filter(annee_construction <= 2020) %>% 
  mutate(date_range = ifelse(annee_construction >= 1990 & annee_construction < 1995, "[1990-1995[", annee_construction)) %>% 
  mutate(date_range = ifelse(annee_construction >= 1995 & annee_construction < 2000, "[1995-2000[", date_range)) %>%
  mutate(date_range = ifelse(annee_construction >= 2000 & annee_construction < 2005, "[2000-2005[", date_range)) %>% 
  mutate(date_range = ifelse(annee_construction >= 2005 & annee_construction < 2010, "[2005-2010[", date_range)) %>% 
  mutate(date_range = ifelse(annee_construction >= 2010 & annee_construction < 2015, "[2010-2015[", date_range)) %>% 
  mutate(date_range = ifelse(annee_construction >= 2015 & annee_construction <= 2020, "[2015-2020]", date_range)) %>% 
  select(numero_matricule, date_range, nombre_logements) %>% 
  st_join(boroughs, .) %>% 
  group_by(date_range, borough) %>% 
  summarize(new_units = sum(nombre_logements, na.rm=TRUE)) %>% 
  filter(!is.na(date_range)) %>% 
  ggplot()+
  geom_sf(data = province, fill="grey90", color=NA)+
  geom_sf(data = boroughs, fill="grey80", color="grey75")+
  geom_sf(aes(fill=new_units), color=NA)+
  scale_fill_stepsn(name="Number of units",
                    breaks = 5,
                    colors=col_palette[c(4, 1, 9)])+
  gg_bbox(boroughs)+
  ggtitle("Number of new units built by date range")+
  facet_wrap(~date_range)+
  theme_void()

ggsave("output/figures/construction_map .pdf", plot = construction_map, width = 8, 
       height = 4.2, units = "in", useDingbats = FALSE)
  

# Graph 2: Average rents by year ----------------------------------------------------------

annual_avg_rent %>% 
  filter(occupied_status == "Occupied Units",
         bedroom == "Total") %>% 
  select(-zone) %>% 
  full_join(., cmhc, by="zone_name") %>% 
  st_as_sf() %>% 
  ggplot()+
  geom_sf(aes(fill=avg_rent), color=NA)+
  scale_fill_gradientn(name = "Average rent",
                       colors = col_palette[c(4, 1, 9)], 
                       na.value = "grey80")+
  ggtitle("Annual average rent by CMHC zone", subtitle = "Year: {frame_time}")+
  transition_time(date)+
  theme_void()

# Graph 3: Decades of rent ----------------------------------------------------------

rents_decade %>%
  mutate(year = as.numeric(year)) %>% 
  inner_join(., CT %>% select(GeoUID)) %>%
  st_as_sf() %>% 
  ggplot()+
  geom_sf(aes(fill=total), color=NA)+
  scale_fill_gradientn(name = "Average rent",
                       colors = col_palette[c(4, 1, 9)], 
                       na.value = "grey80")+
  ggtitle("Annual average rent by CMHC zone", subtitle = "Year: {frame_time}")+
  transition_time(year)+
  theme_void()


