#### 02 GEOMETRY IMPORT ########################################################

source("R/01_startup.R")
library(cancensus)
library(osmdata)


# Montreal DAs ------------------------------------------------------------

View(cancensus::list_census_vectors("CA16"))

DA <-
  get_census(
    dataset = "CA16", regions = list(CSD = c("2466023")), level = "DA",
    vectors = c("v_CA16_4840", "v_CA16_4841", "v_CA16_4836", "v_CA16_4838",
                "v_CA16_4897", "v_CA16_4899", "v_CA16_4870", "v_CA16_4872", "v_CA16_4901",
                "v_CA16_4900", "v_CA16_3957", "v_CA16_3954", "v_CA16_3411", 
                "v_CA16_3405", "v_CA16_6698", "v_CA16_6692", "v_CA16_6725", 
                "v_CA16_6719", "v_CA16_4896", "v_CA16_4861", "v_CA16_4859",
                "v_CA16_2398", "v_CA16_2540", "v_CA16_3852", "v_CA16_3855",
                "v_CA16_5123", "v_CA16_5096"),
    geo_format = "sf") %>% 
  st_transform(32618) %>% 
  select(-c(`Shape Area`:Households, CSD_UID, CT_UID:`Area (sq km)`)) %>% 
  set_names(c("dwellings", "GeoUID", "population", "parent_condo", "condo", "parent_tenure", 
              "renter", "parent_thirty", "p_thirty_renter", "parent_repairs", "major_repairs",
              "median_rent", "average_value_dwellings", "unsuitable_housing", "parent_unsuitable", "average_rent",
              "vm", "parent_vm", "immigrants", "parent_immigrants", "mobility_one_year",
              "parent_mobility_one_year", "mobility_five_years", "parent_mobility_five_years", 
              "median_HH_income_AT", "p_low_income_AT",
              "parent_aboriginal", "aboriginal", "bachelor_above", "parent_education",
              "geometry")) %>% 
  mutate(p_condo = condo / parent_condo,
         p_renter = renter / parent_tenure, 
         p_repairs = major_repairs / parent_repairs,
         p_vm = vm/parent_vm,
         p_thirty_renter = p_thirty_renter/100,
         p_immigrants = immigrants/parent_immigrants,
         p_mobility_one_year = mobility_one_year/parent_mobility_one_year,
         p_mobility_five_years = mobility_five_years/parent_mobility_five_years,
         p_unsuitable = unsuitable_housing/parent_unsuitable,
         p_aboriginal = aboriginal/parent_aboriginal,
         p_bachelor_above = bachelor_above / parent_education) %>% 
  select(GeoUID, dwellings, population, renter, median_rent, average_rent, average_value_dwellings, median_HH_income_AT, p_thirty_renter, p_condo, 
         p_renter, p_repairs, p_mobility_one_year, p_mobility_five_years, p_unsuitable,
         p_vm, p_immigrants, p_aboriginal, p_low_income_AT, p_bachelor_above) %>% 
  as_tibble() %>% 
  st_as_sf(agr = "constant")


# Montreal CTs ------------------------------------------------------------

View(cancensus::list_census_vectors("CA16"))

CT <-
  cancensus::get_census(
    dataset = "CA16", regions = list(CSD = c("2466023")), level = "CT",
    vectors = c("v_CA16_4840", "v_CA16_4841", "v_CA16_4836", "v_CA16_4838",
                "v_CA16_4897", "v_CA16_4899", "v_CA16_4870", "v_CA16_4872",
                "v_CA16_4900", "v_CA16_3957", "v_CA16_3954", "v_CA16_3411", 
                "v_CA16_3405", "v_CA16_6698", "v_CA16_6692", "v_CA16_6725", 
                "v_CA16_6719", "v_CA16_4896", "v_CA16_4861", "v_CA16_4859",
                "v_CA16_2398", "v_CA16_2540", "v_CA16_3852", "v_CA16_3855",
                "v_CA16_5123", "v_CA16_5096"),
    geo_format = "sf") %>% 
  st_transform(32618) %>% 
  select(-c(Type, Households, `Adjusted Population (previous Census)`:CSD_UID, PR_UID:`Area (sq km)`)) %>% 
  set_names(c("GeoUID", "dwellings", "parent_condo", "condo", "parent_tenure", 
              "renter", "parent_thirty", "p_thirty_renter", "parent_repairs", "major_repairs",
              "median_rent", "average_value_dwellings", "unsuitable_housing", "parent_unsuitable",
              "vm", "parent_vm", "immigrants", "parent_immigrants", "mobility_one_year",
              "parent_mobility_one_year", "mobility_five_years", "parent_mobility_five_years", 
              "median_HH_income_AT", "p_low_income_AT", "parent_aboriginal",
              "aboriginal", "bachelor_above", "parent_education",
              "geometry")) %>% 
  mutate(p_condo = condo / parent_condo,
         p_renter = renter / parent_tenure, 
         p_repairs = major_repairs / parent_repairs,
         p_vm = vm/parent_vm,
         p_immigrants = immigrants/parent_immigrants,
         p_mobility_one_year = mobility_one_year/parent_mobility_one_year,
         p_mobility_five_years = mobility_five_years/parent_mobility_five_years,
         p_unsuitable = unsuitable_housing/parent_unsuitable,
         p_aboriginal = aboriginal/parent_aboriginal,
         p_bachelor_above = bachelor_above / parent_education) %>% 
  select(GeoUID, dwellings, renter, median_rent, average_value_dwellings, median_HH_income_AT, p_thirty_renter, p_condo, 
         p_renter, p_repairs, p_mobility_one_year, p_mobility_five_years, p_unsuitable,
         p_vm, p_immigrants, p_aboriginal, p_low_income_AT, p_bachelor_above) %>% 
  as_tibble() %>% 
  st_as_sf(agr = "constant")

# Montreal DAs 2006 -------------------------------------------------------

DA_06 <-
  get_census(
    dataset = "CA06", regions = list(CSD = c("2466023")), level = "DA",
    vectors = c("v_CA06_101", "v_CA06_103", "v_CA06_2049", "v_CA06_2051", 
                "v_CA06_105", "v_CA06_108", "v_CA06_1303", "v_CA06_1302", 
                "v_CA06_478", "v_CA06_474", "v_CA06_453", "v_CA06_451",
                "v_CA06_462", "v_CA06_460", "v_CA06_2030", "v_CA06_1981",
                "v_CA06_1292", "v_CA06_1293", "v_CA06_1257", "v_CA06_1258", "v_CA06_1248",
                "v_CA06_2050", "v_CA06_2054"),
    geo_format = "sf") %>% 
  st_transform(32618) %>% 
  select(-c(`Shape Area`:`Quality Flags`, CSD_UID:CT_UID, CD_UID:`Area (sq km)`)) %>% 
  set_names(c("GeoUID", "dwellings", "parent_tenure", "renter", 
              "parent_repairs", "major_repairs", "parent_thirty", "median_HH_income_AT", "average_gross_rent",
              "thirty_renter", "average_value_dwellings", "vm", "parent_vm", "immigrants", "parent_immigrants",
              "mobility_one_year", "parent_mobility_one_year", "mobility_five_years", "parent_mobility_five_years", 
              "p_low_income_AT", "parent_aboriginal", "aboriginal",
              "bachelor", "above_bachelor", "parent_education",
              "geometry")) %>% 
  mutate(bachelor_above = bachelor + above_bachelor,
         p_renter = renter / parent_tenure, 
         p_repairs = major_repairs / parent_repairs,
         p_thirty_renter = thirty_renter / parent_thirty,
         p_vm = vm/parent_vm,
         p_immigrants = immigrants/parent_immigrants,
         p_mobility_one_year = mobility_one_year/parent_mobility_one_year,
         p_mobility_five_years = mobility_five_years/parent_mobility_five_years,
         p_aboriginal = aboriginal/parent_aboriginal,
         p_bachelor_above = bachelor_above / parent_education) %>% 
  select(GeoUID, dwellings, renter, average_gross_rent, average_value_dwellings, median_HH_income_AT, p_thirty_renter, 
         p_renter, p_repairs, p_mobility_one_year, p_mobility_five_years,
         p_vm, p_immigrants, p_aboriginal, p_low_income_AT, p_bachelor_above) %>% 
  as_tibble() %>% 
  st_as_sf(agr = "constant")

# Montreal CTs 2006 -------------------------------------------------------

CT_06 <-
  get_census(
    dataset = "CA06", regions = list(CSD = c("2466023")), level = "CT",
    vectors = c("v_CA06_101", "v_CA06_103", "v_CA06_2049", "v_CA06_2051", 
                "v_CA06_105", "v_CA06_108", "v_CA06_1303", "v_CA06_1302", 
                "v_CA06_478", "v_CA06_474", "v_CA06_453", "v_CA06_451",
                "v_CA06_462", "v_CA06_460", "v_CA06_2030", "v_CA06_1981",
                "v_CA06_1292", "v_CA06_1293", "v_CA06_1257", "v_CA06_1258", "v_CA06_1248",
                "v_CA06_2050", "v_CA06_2054"),
    geo_format = "sf") %>% 
  st_transform(32618) %>% 
  select(-c(`Shape Area`:`Quality Flags`, CMA_UID:CSD_UID, PR_UID:`Area (sq km)`)) %>% 
  set_names(c("GeoUID", "dwellings", "parent_tenure", "renter", 
              "parent_repairs", "major_repairs", "parent_thirty", "median_HH_income_AT", "average_gross_rent",
              "p_thirty_renter", "average_value_dwellings", "vm", "parent_vm", "immigrants", "parent_immigrants",
              "mobility_one_year", "parent_mobility_one_year", "mobility_five_years", "parent_mobility_five_years", 
              "p_low_income_AT", "parent_aboriginal", "aboriginal",
              "bachelor", "above_bachelor", "parent_education",
              "geometry")) %>% 
  mutate(bachelor_above = bachelor + above_bachelor,
         p_renter = renter / parent_tenure, 
         p_repairs = major_repairs / parent_repairs,
         p_vm = vm/parent_vm,
         p_immigrants = immigrants/parent_immigrants,
         p_mobility_one_year = mobility_one_year/parent_mobility_one_year,
         p_mobility_five_years = mobility_five_years/parent_mobility_five_years,
         p_aboriginal = aboriginal/parent_aboriginal,
         p_bachelor_above = bachelor_above / parent_education) %>% 
  select(GeoUID, dwellings, renter, average_gross_rent, average_value_dwellings, median_HH_income_AT, p_thirty_renter, 
         p_renter, p_repairs, p_mobility_one_year, p_mobility_five_years,
         p_vm, p_immigrants, p_aboriginal, p_low_income_AT, p_bachelor_above) %>% 
  as_tibble() %>% 
  st_as_sf(agr = "constant")


# Quebec province ---------------------------------------------------------

province <- 
  get_census("CA16", regions = list(PR = "24"), geo_format = "sf") %>% 
  st_transform(32618) %>% 
  select(geometry)


# Montreal boroughs -------------------------------------------------------

boroughs_raw <-
  read_sf("data/limadmin-shp/LIMADMIN.shp") %>% 
  filter(TYPE == "Arrondissement") %>% 
  select(borough = NOM) %>% 
  st_set_agr("constant") %>% 
  st_transform(32618) 

boroughs <- 
  boroughs_raw %>% 
  st_intersection(province)

boroughs <- 
  DA %>% 
  select(dwellings) %>% 
  st_interpolate_aw(boroughs, extensive = TRUE) %>% 
  st_drop_geometry() %>% 
  select(dwellings) %>% 
  cbind(boroughs, .) %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  arrange(borough)


# Montreal CSD ------------------------------------------------------------

city <-
  boroughs_raw %>% 
  st_combine() %>% 
  st_union() %>% 
  st_cast("POLYGON") %>% 
  st_union() %>%
  smoothr::fill_holes(400)


# Unite evaluation fonciere -----------------------------------------------

uef_raw <-
  read_sf("data/uniteevaluationfonciere/uniteevaluationfonciere.shp") %>%
  st_transform(32618)


# Streets -----------------------------------------------------------------

streets <- 
  (getbb("Région administrative de Montréal") * c(1.01, 0.99, 0.99, 1.01)) %>% 
  opq(timeout = 200) %>% 
  add_osm_feature(key = "highway") %>% 
  osmdata_sf()

streets <-
  rbind(
    streets$osm_polygons %>% st_set_agr("constant") %>% st_cast("LINESTRING"), 
    streets$osm_lines) %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  st_transform(32618) %>%
  st_set_agr("constant") %>%
  st_intersection(city)

streets <- 
  streets %>% 
  filter(highway %in% c("primary", "secondary")) %>% 
  select(osm_id, name, highway, geometry)

downtown_poly <- 
  st_polygon(list(matrix(c(607000, 5038000,
                           614000, 5038000,
                           614000, 5045000,
                           607000, 5045000,
                           607000, 5038000), 
                         ncol = 2, byrow = TRUE))) %>% 
  st_sfc(crs = 32618)

streets_downtown <- 
  streets %>% 
  st_intersection(downtown_poly)

# Save output -------------------------------------------------------------

save(DA, CT, DA_06, CT_06, boroughs, boroughs_raw, province, city, streets, streets_downtown,   
     file = "output/geometry.Rdata")

