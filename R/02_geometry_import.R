#### 02 GEOMETRY IMPORT ########################################################

source("R/01_startup.R")
library(cancensus)
library(osmdata)

View(cancensus::list_census_regions("CA16"))
cancensus::list_census_vectors("CA16")

# Montreal DAs ------------------------------------------------------------

View(cancensus::list_census_vectors("CA16"))

DA <-
  get_census(
    dataset = "CA16", regions = list(CSD = c("2466023")), level = "DA",
    vectors = c("v_CA16_4840", "v_CA16_4841", "v_CA16_4836", "v_CA16_4838",
                "v_CA16_4897", "v_CA16_4899", "v_CA16_4870", "v_CA16_4872",
                "v_CA16_4900", "v_CA16_3957", "v_CA16_3954", "v_CA16_3411", 
                "v_CA16_3405", "v_CA16_6698", "v_CA16_6692", "v_CA16_6725", 
                "v_CA16_6719", "v_CA16_4896", "v_CA16_4861", "v_CA16_4859",
                "v_CA16_2398", "v_CA16_2540", "v_CA16_3852", "v_CA16_3855",
                "v_CA16_5123", "v_CA16_5096"),
    geo_format = "sf") %>% 
  st_transform(32618) %>% 
  select(-c(`Shape Area`:Households, CSD_UID:`Area (sq km)`)) %>% 
  set_names(c("dwellings", "GeoUID", "parent_condo", "condo", "parent_tenure", 
              "renter", "parent_thirty", "p_thirty_renter", "parent_repairs", "major_repairs",
              "median_rent", "average_value_dwellings", "vm", "parent_vm", "immigrants", "parent_immigrants", "mobility_one_year",
              "parent_mobility_one_year", "mobility_five_years", "parent_mobility_five_years", 
              "unsuitable_housing", "parent_unsuitable", "median_HH_income_AT", "p_low_income_AT",
              "parent_aboriginal", "aboriginal", "bachelor_above", "parent_education",
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

DA %>% 
  ggplot()+
  geom_sf(aes(fill=p_vm), color=NA) 


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
  set_names(c("dwellings", "GeoUID", "parent_condo", "condo", "parent_tenure", 
              "renter", "parent_thirty", "p_thirty_renter", "parent_repairs", "major_repairs",
              "median_rent", "average_value_dwellings", "vm", "parent_vm", "immigrants", "parent_immigrants", "mobility_one_year",
              "parent_mobility_one_year", "mobility_five_years", "parent_mobility_five_years", 
              "unsuitable_housing", "parent_unsuitable", "median_HH_income_AT", "p_low_income_AT",
              "parent_aboriginal", "aboriginal", "bachelor_above", "parent_education",
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

# Download permit dataset -------------------------------------------------------------

permits <-
  read_sf("data/permis-construction/permis-construction.shp") %>%
  st_transform(32618) %>%
  as_tibble() %>%
  st_as_sf()


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


# Unite evaluation fonciere --------------

uef_raw <-
  read_sf("data/uniteevaluationfonciere/uniteevaluationfonciere.shp") %>%
  st_transform(32618)


# Save output -------------------------------------------------------------

save(DA, CT,   
     file = "output/geometry.Rdata")