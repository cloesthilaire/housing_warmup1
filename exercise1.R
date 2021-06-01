
# load these package ------------------------------------------------------
library(scales)
library(mapview)

# exercise1--bike commuters -----------------------------------------------


DAbike <- 
  get_census(
  dataset = "CA16", regions = list( CSD = c("2466023")), level = "DA",
  vectors = c("v_CA16_5792", "v_CA16_5807"),
  geo_format = "sf") %>% 
  st_transform(32618) %>% 
  select(-c(`Shape Area`:Households, CSD_UID:`Area (sq km)`)) %>% 
  set_names(c("dwellings", "GeoUID", "parent_commutetowork", "commute_bike", "geometry")) %>% 
  mutate(p_commute_bike = commute_bike / parent_commutetowork) %>% 
  as_tibble()%>% 
  st_as_sf(agr = "constant")

DAbike %>% glimpse()

commuters <- DAbike %>% 
  ggplot() +
  geom_sf(mapping = aes(fill=p_commute_bike), color = NA) +
  scale_fill_gradient(name="Percentage of bike commuters",
                       low = "#074387",
                       high = "#FFD500",
                       na.value = "grey80",
                       labels = scales::percent) +
  theme_void()

#can also try

#commuters+ theme(legend.position = "left",legend.title = element_text(face = "bold"),legend.background = element_rect(fill = "blue"))

#commuters+ guides(fill = guide_colourbar(title = "Percentage\nof\nbike commuters", title.vjust = 1)) 

# \n break into lines

# save figure and adjust size
ggsave("output/figures/commuters.pdf", plot = commuters, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)


# exercise 2-- intersection -----------------------------------------------

read_csv("data/lieuxculturels.csv")

culturallocation_sf <- 
  lieuxculturels %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%  #first need to define the original crs
  st_transform(32618)

plot(culturallocation_sf) #quick check how it look like
  
cul <- 
  culturallocation_sf %>% #layers come in order
  ggplot()+
  geom_sf(data = DAbike, aes())+
  geom_sf(mapping = aes(color = "red"))

mapview(culturallocation_sf) +
  mapview(DAbike)

# st_intersection(DAbike, culturallocation_sf)
# intersect points in polygon

DA_intersect <- st_intersection(culturallocation_sf, DAbike)
DA_intersect2 <- 
  DA_intersect %>% 
  count(GeoUID)

DA_intersect2 %>%
  group_by(GeoUID)%>% 
  summarise(n)

# layer up
DA_intersect2 %>%
  ggplot()+
  geom_sf(data = DAbike, aes())+
  geom_sf(mapping = aes(fill = n))

plot(DA_intersect) # quick check what you have in the dataset

#try st_join
view(st_join(DAbike, culturallocation_sf))

cul <- st_join(DAbike, culturallocation_sf) %>%
  select(-c(`dwellings`,`parent_commutetowork`,`commute_bike`,`p_commute_bike` )) %>%
  filter(Ville != "NA") %>% 
  group_by(GeoUID) %>% 
  count()

cul %>% 
  ggplot() +
  geom_sf(data = DAbike, aes(), colour = NA) +
  geom_sf(mapping = aes(fill = n), colour = NA)+
  scale_fill_gradient(name = "Numbers of \ncultural institutions")+
  theme_void()

# discuss with cloe
cul2 <- st_join(DAbike, culturallocation_sf) %>%
  select(-c(`dwellings`,`parent_commutetowork`,`commute_bike`,`p_commute_bike` )) %>%
  filter(Ville != "NA") %>%
  group_by(GeoUID) %>% 
  summarize(number_ins = n())  # can I put as.character here

glimpse(cul2)

cul2 %>% 
  ggplot() +
  geom_sf(data = DAbike, fill="grey80", color=NA)+  
  geom_sf(aes(fill = as.character(number_ins)), color=NA)+
  scale_fill_manual(name = "Numbers of \nCultural Institutions",
                    values = col_palette[c(4,1,9)],
                    na.value = "grey60") +
  theme_void()

# geom_label()+
# bbox

