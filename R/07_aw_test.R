View(cancensus::list_census_vectors("CA06"))

DA_06_test <-
  get_census(
    dataset = "CA06", regions = list(CSD = c("2466023")), level = "DA",
    vectors = c("v_CA06_101", "v_CA06_103", "v_CA06_2049", "v_CA06_2051", "v_CA06_2053",
                "v_CA06_105", "v_CA06_108", "v_CA06_1303", "v_CA06_1302",
                "v_CA06_478", "v_CA06_474", "v_CA06_453", "v_CA06_451", "v_CA06_2018",
                "v_CA06_462", "v_CA06_460", "v_CA06_2030", "v_CA06_1981", "v_CA06_1979",
                "v_CA06_1292", "v_CA06_1293", "v_CA06_1257", "v_CA06_1258", "v_CA06_1248",
                "v_CA06_2050", "v_CA06_2054"),
    geo_format = "sf") %>% 
  st_transform(32618) %>% 
  select(-c(`Shape Area`:`Quality Flags`, CSD_UID, CT_UID, CD_UID:`Area (sq km)`)) %>% 
  set_names(c("GeoUID", "population", "dwellings", "tenure_total", "renter", 
              "repairs_total", "major_repairs", "renter_total", "HH_income_total",  
              "HH_income_AT_median", "gross_rent_avg",
              "thirty_renter", "value_dwellings_total", "value_dwellings_avg", "vm", "vm_total", "immigrants", "immigrants_total",
              "mobility_one_year", "mobility_one_year_total", "mobility_five_years", "mobility_five_years_total", 
              "low_income_AT_prop", "low_income_total", "aboriginal_total", "aboriginal",
              "bachelor", "above_bachelor", "education_total",
              "geometry")) %>% 
  mutate(bachelor_above = bachelor + above_bachelor,
         low_income = low_income_AT_prop * low_income_total
         # p_renter = renter / parent_tenure, 
         # p_repairs = major_repairs / parent_repairs,
         # p_thirty_renter = thirty_renter / parent_thirty,
         # p_vm = vm/parent_vm,
         # p_immigrants = immigrants/parent_immigrants,
         # p_mobility_one_year = mobility_one_year/parent_mobility_one_year,
         # p_mobility_five_years = mobility_five_years/parent_mobility_five_years,
         # p_aboriginal = aboriginal/parent_aboriginal,
         # p_bachelor_above = bachelor_above / parent_education
  ) %>% 
  select(-c(bachelor, above_bachelor, low_income_AT_prop)) %>% 
  as_tibble() %>% 
  st_as_sf(agr = "constant")

DA_06_test <- 
  DA_06_test %>% 
  rename(ID = GeoUID)


# Process 2006 in 2016 boundaries ------------------------------------------------------------

DA_06_test <-
  DA_06_test %>% 
  mutate(area = st_area(geometry)) %>% 
  st_set_agr("constant")

# Identify variables to be averaged
avg_list <- str_subset(names(DA_06_test), "avg|median") %>% 
  str_subset("total", negate = TRUE)

# Identify variables to be aggregated
agg_list <-
  setdiff(names(DA_06_test), c("ID", "name", "CTUID", "CSDUID", "geometry", 
                               "area")) %>% 
  setdiff(avg_list)

DA_06_to_16 <-
  DA %>% 
  rename(ID = GeoUID) %>% 
  select(ID) %>% 
  #st_transform(32618) %>% 
  st_set_agr("constant") %>% 
  st_intersection(DA_06_test) %>% 
  mutate(area_prop = st_area(geometry) / area) %>% 
  mutate(across(all_of(agg_list), ~{.x * units::drop_units(area_prop)})) %>% 
  select(-ID.1, -area, -area_prop) %>% 
  st_drop_geometry() %>% 
  group_by(ID) %>% 
  summarize(
    value_dwellings_avg = 
      weighted.mean(value_dwellings_avg, value_dwellings_total, na.rm = TRUE),
    gross_rent_avg = 
      weighted.mean(gross_rent_avg, renter_total, na.rm = TRUE),
    HH_income_AT_median = 
      weighted.mean(HH_income_AT_median, HH_income_total, na.rm = TRUE),
    across(all_of(agg_list), sum, na.rm = TRUE)) %>% 
  mutate(across(where(is.numeric), ~replace(., is.nan(.), 0)))


DA_06_interpolated <- 
  DA_06_to_16 %>% 
  rename(dwellings_06 = dwellings,
         population_06 = population,
         renter_06 = renter,
         value_dwellings_avg_06 = value_dwellings_avg,
         gross_rent_avg_06 = gross_rent_avg,
         HH_income_AT_median_06 = HH_income_AT_median) %>% 
  mutate(p_low_income_AT_06 = low_income * low_income_total,
         p_renter_06 = renter_06 / tenure_total,
         p_repairs_06 = major_repairs / repairs_total,
         p_thirty_renter_06 = thirty_renter / renter_total,
         p_vm_06 = vm / vm_total,
         p_immigrants_06 = immigrants / immigrants_total,
         p_mobility_one_year_06 = mobility_one_year / mobility_one_year_total,
         p_mobility_five_years_06 = mobility_five_years / mobility_five_years_total,
         p_aboriginal_06 = aboriginal / aboriginal_total,
         p_bachelor_above_06 = bachelor_above / education_total
  ) %>% 
  select(ID, dwellings_06, population_06, renter_06, gross_rent_avg_06, value_dwellings_avg_06, 
         HH_income_AT_median_06, p_thirty_renter_06, p_renter_06, p_repairs_06, 
         p_mobility_one_year_06, p_mobility_five_years_06, p_vm_06, 
         p_immigrants_06, p_aboriginal_06, p_low_income_AT_06, p_bachelor_above_06) %>% 
  as_tibble()

DA_06_16 <- left_join(DA %>% rename(ID = GeoUID), DA_06_interpolated, by = "ID") %>% 
  relocate(geometry, .after = last_col())

var_DA_06_16 <- 
  DA_06_16 %>% 
  mutate(var_dwellings = (dwellings - dwellings_06) / dwellings_06, 
         var_population = (population - population_06) / population_06,
         var_renter = (renter - renter_06) / renter_06,
         var_prop_renter = (p_renter - p_renter_06) / p_renter_06,
         var_avg_rent = (average_rent - gross_rent_avg_06) / gross_rent_avg_06,
         var_avg_value_dwelling = (average_value_dwellings - value_dwellings_avg_06) / value_dwellings_avg_06,
         var_median_HH_income_AT = (median_HH_income_AT - HH_income_AT_median_06) / HH_income_AT_median_06,
         var_prop_LIMAT = (p_low_income_AT - p_low_income_AT_06) / p_low_income_AT_06,
         var_prop_thirty_renter = (p_thirty_renter - p_thirty_renter_06) / p_thirty_renter_06,
         var_prop_repairs = (p_repairs - p_repairs_06) / p_repairs_06,
         var_prop_mobility_one_year = (p_mobility_one_year - p_mobility_one_year_06) / p_mobility_one_year_06,
         var_prop_mobility_five_years = (p_mobility_five_years - p_mobility_five_years_06) / p_mobility_five_years_06,
         var_prop_vm = (p_vm - p_vm_06) / p_vm_06,
         var_prop_immigrants = (p_immigrants - p_immigrants_06) / p_immigrants_06,
         var_prop_aboriginal = (p_aboriginal - p_aboriginal_06) / p_aboriginal_06) #%>% 
#select(ID, var_dwellings:var_prop_aboriginal)


save(var_DA_06_16,   
     file = "output/var_06_16.Rdata")

var_DA_06_16 %>% 
  ggplot() +
  geom_sf(aes(fill = var_dwellings), color = NA)+
  scale_fill_gradient2(low = col_palette[3],
                       mid = "white",
                       high = col_palette[1],
                       midpoint = 0,
                       label = scales::percent,
                       oob = scales::squish,
                       limits = c(-0.5, 0.5))+
  theme_void()

var_DA_06_16 %>% 
  ggplot() +
  geom_sf(aes(fill = var_avg_rent), color = NA)+
  scale_fill_gradient2(low = col_palette[3],
                       mid = "white",
                       high = col_palette[1],
                       midpoint = 0,
                       label = scales::percent,
                       oob = scales::squish,
                       limits = c(-0.5, 0.5))+
  theme_void()

var_DA_06_16 %>% 
  ggplot() +
  geom_sf(aes(fill = var_prop_immigrants), color = NA)+
  scale_fill_gradient2(low = col_palette[3],
                       mid = "white",
                       high = col_palette[1],
                       midpoint = 0,
                       label = scales::percent,
                       oob = scales::squish,
                       limits = c(-0.5, 0.5))+
  theme_void()

var_DA_06_16 %>% 
  ggplot() +
  geom_sf(aes(fill = var_avg_value_dwelling), color = NA)+
  scale_fill_gradient2(low = col_palette[3],
                       mid = "white",
                       high = col_palette[1],
                       midpoint = 0,
                       label = scales::percent,
                       oob = scales::squish,
                       limits = c(-0.5, 1))+
  theme_void()

var_DA_06_16 %>% 
  ggplot() +
  geom_sf(aes(fill = var_median_HH_income_AT), color = NA)+
  scale_fill_gradient2(low = col_palette[3],
                       mid = "white",
                       high = col_palette[1],
                       midpoint = 0,
                       label = scales::percent,
                       oob = scales::squish,
                       limits = c(-1, 1))+
  theme_void()





