######### 10 GRAPHICS TO INCLUDE IN THE HOUSING MODULE ########################################

library(gganimate)
library(transformr)

load("output/geometry.Rdata")
load("output/cmhc.Rdata")
load("output/permits.Rdata")

# Graph 1: Permits emitted ----------------------------------------------------------

combined_dwellings %>% 
  ggplot()+
  geom_sf(data = province, fill="grey90", color=NA)+
  geom_sf(data = boroughs, fill="grey80", color="grey70")+
  geom_sf(color=col_palette[1])+
  gg_bbox(boroughs)+
  ggtitle("Number of combining permits", subtitle = "Year: {frame_time}")+
  transition_time(issued_date)+
  theme_void()

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


