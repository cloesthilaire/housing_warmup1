######### 10 GRAPHICS TO INCLUDE IN THE HOUSING MODULE ########################################

source("R/01_startup.R")

library(gganimate)
library(transformr)

load("output/geometry.Rdata")
#load("output/cmhc.Rdata")
load("output/prmits_processed.Rdata")

# Graph 1: Permits emitted for combinations ----------------------------------------------------------

combined_dwellings_data <- 
  combined_dwellings %>% 
  filter(issued_date <= 2020) %>%
  mutate(date_range = ifelse(issued_date >= 1990 & issued_date < 1995, "[1990-1995[", issued_date)) %>%
  mutate(date_range = ifelse(issued_date >= 1995 & issued_date < 2000, "[1995-2000[", date_range)) %>%
  mutate(date_range = ifelse(issued_date >= 2000 & issued_date < 2005, "[2000-2005[", date_range)) %>%
  mutate(date_range = ifelse(issued_date >= 2005 & issued_date < 2010, "[2005-2010[", date_range)) %>%
  mutate(date_range = ifelse(issued_date >= 2010 & issued_date < 2015, "[2010-2015[", date_range)) %>%
  mutate(date_range = ifelse(issued_date >= 2015 & issued_date <= 2020, "[2015-2020]", date_range)) %>%
  st_join(DA, .) %>% 
  filter(!is.na(date_range)) %>%
  group_by(GeoUID, date_range) %>% 
  summarize(n_date_range = sum(n(), na.rm = TRUE))

combination_map <- 
  combined_dwellings_data %>% 
  ggplot()+
  geom_sf(data = province, fill="grey90", color=NA)+
  geom_sf(aes(fill=n_date_range), color=NA)+
  scale_fill_gradientn(name="Number of permits",
                    # n.breaks = 5,
                    # breaks = c(1, 2, 3, 4, 5),
                    colours=col_palette[c(4, 1, 9)], #, 4,1,2,9,10
                    limits = c(1, 5),
                    oob = scales::squish)+
  coord_sf(xlim = c(597000, 619000), ylim = c(5030500, 5055000),
           expand = FALSE) +
  ggtitle("Number of permits emitted for dwelling combinations by date range")+
  theme_void()+
  facet_wrap(~date_range)

ggsave("output/figures/combination_map.pdf", plot = combination_map, width = 8, 
       height = 4.2, units = "in", useDingbats = FALSE)


# Graph 2: Units lost to combinations ----------------------------------------------------------

combination_loss_data <- 
  combined_dwellings %>% 
  filter(issued_date <= 2020) %>%
  mutate(date_range = ifelse(issued_date >= 1990 & issued_date < 1995, "[1990-1995[", issued_date)) %>%
  mutate(date_range = ifelse(issued_date >= 1995 & issued_date < 2000, "[1995-2000[", date_range)) %>%
  mutate(date_range = ifelse(issued_date >= 2000 & issued_date < 2005, "[2000-2005[", date_range)) %>%
  mutate(date_range = ifelse(issued_date >= 2005 & issued_date < 2010, "[2005-2010[", date_range)) %>%
  mutate(date_range = ifelse(issued_date >= 2010 & issued_date < 2015, "[2010-2015[", date_range)) %>%
  mutate(date_range = ifelse(issued_date >= 2015 & issued_date <= 2020, "[2015-2020]", date_range)) %>% 
  st_join(DA, .) %>% 
  filter(!is.na(date_range)) %>%
  group_by(GeoUID, date_range) %>% 
  summarize(n_date_range = sum(nb_dwellings, na.rm = TRUE))

combination_loss_map <- 
  combination_loss_data %>%  
  ggplot()+
  geom_sf(data = province, fill="grey90", color=NA)+
  geom_sf(aes(fill=n_date_range), color=NA)+
  scale_fill_gradientn(name="Number of units lost",
                    # n.breaks = 5,
                    # breaks = c(-40, -30, -20, -10, 1),
                    colours=col_palette[c(9, 2, 1, 4)],
                    limits = c(-10, 0),
                    oob = scales::squish)+
  coord_sf(xlim = c(597000, 619000), ylim = c(5030500, 5055000),
           expand = FALSE) +
  ggtitle("Number of housing units loss to combinations by date range")+
  theme_void()+
  facet_wrap(~date_range)

ggsave("output/figures/combination_loss_map.pdf", plot = combination_loss_map, width = 8, 
       height = 4.2, units = "in", useDingbats = FALSE)


# Graph 3: Permits emitted for condo conversions ----------------------------------------------------------

conversion_map_data <- 
  condo_conversions %>% 
  filter(issued_date <= 2020) %>% 
  mutate(date_range = ifelse(issued_date >= 1990 & issued_date < 1995, "[1990-1995[", issued_date)) %>% 
  mutate(date_range = ifelse(issued_date >= 1995 & issued_date < 2000, "[1995-2000[", date_range)) %>%
  mutate(date_range = ifelse(issued_date >= 2000 & issued_date < 2005, "[2000-2005[", date_range)) %>% 
  mutate(date_range = ifelse(issued_date >= 2005 & issued_date < 2010, "[2005-2010[", date_range)) %>% 
  mutate(date_range = ifelse(issued_date >= 2010 & issued_date < 2015, "[2010-2015[", date_range)) %>% 
  mutate(date_range = ifelse(issued_date >= 2015 & issued_date <= 2020, "[2015-2020]", date_range)) %>% 
  select(-borough) %>% 
  st_join(DA, .) %>% 
  group_by(GeoUID, date_range) %>% 
  summarize(number_conversions = n()) %>% 
  filter(!is.na(date_range))

conversion_map <- 
  conversion_map_data %>%   
  ggplot()+
  geom_sf(data = province, fill="grey90", color=NA)+
  geom_sf(aes(fill=number_conversions), color=NA)+
  scale_fill_stepsn(name="Number of permits",
                    n.breaks = 2,
                    breaks = c(2, 3),
                    colours=col_palette[c(4, 1, 10)],
                    limits = c(1, 4),
                    oob = scales::squish)+
  coord_sf(xlim = c(597000, 619000), ylim = c(5030500, 5055000),
           expand = FALSE) +
  ggtitle("Number of condominium conversions")+
  facet_wrap(~date_range)+
  theme_void()

ggsave("output/figures/conversion_map.pdf", plot = conversion_map, width = 8, 
       height = 4.2, units = "in", useDingbats = FALSE)


# Graph 4: Number of new units built ----------------------------------------------------------

new_construction_data <- 
  new_construction %>% 
  set_names(c("civic_start", "civic_end", "street_name", "annee_construction", "number_dwellings", "geometry")) %>% 
  mutate(date_range = ifelse(annee_construction >= 1990 & annee_construction < 1995, "[1990-1995[", "TBD")) %>% 
  mutate(date_range = ifelse(annee_construction >= 1995 & annee_construction < 2000, "[1995-2000[", date_range)) %>%
  mutate(date_range = ifelse(annee_construction >= 2000 & annee_construction < 2005, "[2000-2005[", date_range)) %>% 
  mutate(date_range = ifelse(annee_construction >= 2005 & annee_construction < 2010, "[2005-2010[", date_range)) %>% 
  mutate(date_range = ifelse(annee_construction >= 2010 & annee_construction < 2015, "[2010-2015[", date_range)) %>% 
  mutate(date_range = ifelse(annee_construction >= 2015 & annee_construction <= 2020, "[2015-2020]", date_range)) %>% 
  st_as_sf() %>% 
  st_join(DA, .) %>% 
  filter(!is.na(date_range)) %>%
  group_by(GeoUID, date_range) %>% 
  summarize(n_date_range = n(),
            dwellings_date_range = sum(number_dwellings, na.rm = TRUE)) 

#construction_map <- 
  new_construction_data %>% 
  ggplot()+
  geom_sf(data = province, fill="grey90", color=NA)+
  geom_sf(aes(fill=n_date_range), color=NA)+
  scale_fill_gradientn(name="Number of buildings",
                       colours=col_palette[c(4, 1, 2, 9)],
                       limits = c(10, 200),
                       oob = scales::squish)+
  coord_sf(xlim = c(597000, 619000), ylim = c(5030500, 5055000),
           expand = FALSE) +
  ggtitle("Number of new residential buildings built by date range")+
  theme_void()+
  facet_wrap(~date_range)

ggsave("output/figures/construction_map.pdf", plot = construction_map, width = 8, 
       height = 4.2, units = "in", useDingbats = FALSE)

construction_map <- 
  new_construction_data %>% 
  ggplot()+
  geom_sf(data = province, fill="grey90", color=NA)+
  geom_sf(aes(fill=dwellings_date_range), color=NA)+
  scale_fill_gradientn(name="Number of units",
                       colours=col_palette[c(4, 1, 2, 9)],
                       limits = c(10, 1500),
                       oob = scales::squish)+
  coord_sf(xlim = c(597000, 619000), ylim = c(5030500, 5055000),
           expand = FALSE) +
  ggtitle("Number of new residential units built by date range")+
  theme_void()+
  facet_wrap(~date_range)

ggsave("output/figures/construction_map.pdf", plot = construction_map, width = 8, 
       height = 4.2, units = "in", useDingbats = FALSE)


# Graph 5: Permits emitted for demolition ----------------------------------------------------------

demolition_data_map <- 
  demolitions %>% 
  filter(issued_date <= 2020) %>%
  mutate(date_range = ifelse(issued_date >= 1990 & issued_date < 1995, "[1990-1995[", issued_date)) %>%
  mutate(date_range = ifelse(issued_date >= 1995 & issued_date < 2000, "[1995-2000[", date_range)) %>%
  mutate(date_range = ifelse(issued_date >= 2000 & issued_date < 2005, "[2000-2005[", date_range)) %>%
  mutate(date_range = ifelse(issued_date >= 2005 & issued_date < 2010, "[2005-2010[", date_range)) %>%
  mutate(date_range = ifelse(issued_date >= 2010 & issued_date < 2015, "[2010-2015[", date_range)) %>%
  mutate(date_range = ifelse(issued_date >= 2015 & issued_date <= 2020, "[2015-2020]", date_range)) %>%
  st_join(DA, .) %>% 
  filter(!is.na(date_range)) %>%
  group_by(GeoUID, date_range) %>% 
  summarize(n_date_range = sum(n(), na.rm = TRUE)) 

demolition_map <- 
  demolition_data_map %>% 
  ggplot()+
  geom_sf(data = province, fill="grey90", color=NA)+
  geom_sf(aes(fill=n_date_range), color=NA)+
  scale_fill_gradientn(name="Number of permits",
                       colours=col_palette[c(4, 1, 2)],
                       limits = c(0, 5),
                       oob = scales::squish
                       )+
  coord_sf(xlim = c(597000, 619000), ylim = c(5030500, 5055000),
           expand = FALSE) +
  ggtitle("Number of demolition permits issued by date range")+
  theme_void()+
  facet_wrap(~date_range)

ggsave("output/figures/demolition_map.pdf", plot = demolition_map, width = 8, 
       height = 4.2, units = "in", useDingbats = FALSE)


# Graph 6: Permits emitted for non-structural renovations ----------------------------------------------------------

renovation_data <- 
  renovations %>% 
  filter(issued_date <= 2020) %>%
  mutate(date_range = ifelse(issued_date >= 1990 & issued_date < 1995, "[1990-1995[", issued_date)) %>%
  mutate(date_range = ifelse(issued_date >= 1995 & issued_date < 2000, "[1995-2000[", date_range)) %>%
  mutate(date_range = ifelse(issued_date >= 2000 & issued_date < 2005, "[2000-2005[", date_range)) %>%
  mutate(date_range = ifelse(issued_date >= 2005 & issued_date < 2010, "[2005-2010[", date_range)) %>%
  mutate(date_range = ifelse(issued_date >= 2010 & issued_date < 2015, "[2010-2015[", date_range)) %>%
  mutate(date_range = ifelse(issued_date >= 2015 & issued_date <= 2020, "[2015-2020]", date_range)) %>%
  st_join(DA, .) %>% 
  filter(!is.na(date_range)) %>%
  group_by(GeoUID, date_range) %>% 
  summarize(n_date_range = sum(n(), na.rm = TRUE)) 
  
renovation_map <- 
  renovation_data %>% 
  ggplot()+
  geom_sf(data = province, fill="grey90", color=NA)+
  geom_sf(aes(fill=n_date_range), color=NA)+
  scale_fill_gradientn(name="Number of permits",
                       colours=col_palette[c(4, 1, 9)],
                       limits = c(0, 20),
                       oob = scales::squish
  )+
    coord_sf(xlim = c(597000, 619000), ylim = c(5030500, 5055000),
             expand = FALSE) +
  ggtitle("Number of renovation permits issued by date range")+
  theme_void()+
  facet_wrap(~date_range)

ggsave("output/figures/renovation_map.pdf", plot = renovation_map, width = 8, 
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


