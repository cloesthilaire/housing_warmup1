#### 06 CMHC 2020 IMPORT DATA  ########################################################

source("R/01_startup.R")
load("output/geometry.Rdata")
load("output/cmhc_2019.Rdata")

# Annual units ---------------------------------------------------------------

units2020 <- readxl::read_xlsx("data/mtl_units.xlsx")

annual_units <- 
  units2020 %>% 
  pivot_longer(cols = Bachelor:Total) %>% 
  mutate(date = 2020,
         dwelling_type = "Total") %>% 
  rename(units = value,
         bedroom = name) %>% 
  left_join(., st_drop_geometry(cmhc)) %>% 
  select(date, zone, zone_name, dwelling_type, bedroom, units) %>% 
  rbind(., annual_units) %>% 
  arrange(date)

city_units <- 
  annual_units %>% 
  filter(zone_name == "Montréal") %>% 
  select(date, bedroom, units) %>% 
  rbind(., city_units) %>% 
  arrange(date)

annual_units <- 
  annual_units %>% 
  filter(zone_name != "Montréal")

# Average rents -----------------------------------------------------------------

rents2020 <- readxl::read_xlsx("data/mtl_average_rents.xlsx")

part1 <- rents2020 %>% 
  select(zone_name, Bachelor, "1 Bedroom", "2 Bedroom", "3 Bedroom +", "Total") %>% 
  pivot_longer(cols = Bachelor:Total)

part2 <- rents2020 %>% 
  select(zone_name, quality0, quality1, quality2, quality3, qualityt) %>% 
  pivot_longer(quality0:qualityt) %>%
  pull(value)

annual_avg_rent <- 
  part1 %>% 
  mutate(quality = part2) %>% 
  rename(bedroom = name,
         avg_rent = value) %>% 
  mutate(date = 2020,
         occupied_status = "Occuppied Units",
         occ_rent_higher = NA) %>% 
  left_join(., st_drop_geometry(cmhc)) %>% 
  select(date, zone, zone_name, bedroom, occupied_status, avg_rent, quality, occ_rent_higher) %>% 
  rbind(., annual_avg_rent) %>% 
  arrange(date) 

city_avg_rent <- 
  annual_avg_rent %>% 
  filter(zone_name == "Montréal") %>% 
  select(date, bedroom, avg_rent, quality) %>% 
  rbind(., city_avg_rent) %>% 
  arrange(date)

annual_avg_rent <- 
  annual_avg_rent %>% 
  filter(zone_name != "Montréal")

# Annual vacancy ------------------------------------------------

vacancy2020 <- readxl::read_xlsx("data/mtl_vacancy_rates.xlsx")

part1 <- vacancy2020 %>% 
  select(zone_name, Bachelor, "1 Bedroom", "2 Bedroom", "3 Bedroom +", "Total") %>% 
  pivot_longer(cols = Bachelor:Total)

part2 <- rents2020 %>% 
  select(zone_name, quality0, quality1, quality2, quality3, qualityt) %>% 
  pivot_longer(quality0:qualityt) %>%
  pull(value)

annual_vacancy <- 
  part1 %>% 
  mutate(quality = part2) %>% 
  rename(bedroom = name,
         vacancy = value) %>% 
  mutate(date = 2020,
         dwelling_type = "Total") %>% 
  left_join(., st_drop_geometry(cmhc)) %>% 
  select(date, zone, zone_name, dwelling_type, bedroom, vacancy, quality) %>% 
  rbind(., annual_vacancy) %>% 
  arrange(date) 
  
city_vacancy <- 
  annual_vacancy %>% 
  filter(zone_name == "Montréal") %>% 
  select(date, bedroom, vacancy, quality) %>% 
  rbind(., city_vacancy) %>% 
  arrange(date)

annual_vacancy <- 
  annual_vacancy %>% 
  filter(zone_name != "Montréal")


# Save output -------------------------------------------------------

save(annual_avg_rent, annual_units, annual_vacancy, 
     city_avg_rent, city_units, city_vacancy, cmhc, file = "output/cmhc.Rdata")
