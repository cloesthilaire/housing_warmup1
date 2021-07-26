
library(tidyverse)
library(sf)
library(stringr)
library(tidyr)
library(qs)
library(cancensus)
library(ggpubr)

#bivariate map
library(scales)
library(biscale)
library(patchwork)

# neighbourhood change

# get census data -----
View(cancensus::list_census_vectors("CA01"))

DA_01_test <-
  get_census(
    dataset = "CA01", regions = list(CSD = c("2466025")), level = "DA",
    vectors = c("v_CA01_96", "v_CA01_100", "v_CA01_96","v_CA01_104",
                "v_CA01_1666", "v_CA01_1621","v_CA01_1634","v_CA01_1667","v_CA01_1668",
                "v_CA01_1670", "v_CA01_1674", "v_CA01_703", "v_CA01_702", "v_CA01_406", "v_CA01_402",
                "v_CA01_383","v_CA01_381","v_CA01_392","v_CA01_390",
                "v_CA01_1618","v_CA01_1617","v_CA01_725","v_CA01_726",
                "v_CA01_1397","v_CA01_1384"),
    geo_format = "sf") %>% 
  st_transform(32618) %>% 
  select(-c(`Shape Area`:`Households`, CSD_UID, key, CT_UID, CD_UID:`Area (sq km)`)) %>% 
  set_names(c("GeoUID", "population", "dwellings", "tenure_total", "renter",
              "major_repairs","repairs_total","renter_total","gross_rent_avg","thirty_renter",
              "value_dwellings_total", "value_dwellings_avg", "HH_income_total","HH_income_median", 
              "low_income", "low_income_total",  "vm", "vm_total", "aboriginal_total", "aboriginal",
              "immigrants", "immigrants_total", "mobility_one_year", "mobility_one_year_total", "mobility_five_years", "mobility_five_years_total",
              "bachelor_above", "education_total","geometry" )) %>% 
  mutate(low_income_prop = low_income / low_income_total
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
  #select(-c(low_income_AT_prop)) %>% 
  as_tibble() %>% 
  st_as_sf(agr = "constant")

# 2001 incomes are before-tax, household income and low income affected
DA_01_test <- 
  DA_01_test %>% 
  rename(ID = GeoUID)

# data for 1996
view(DA_96_test)
DA_96_test <-
  get_census(
    dataset = "CA1996", regions = list(CSD = c("2466025")), level = "DA",
    vectors = c("v_CA1996_1678", "v_CA1996_1683", "v_CA1996_1678","v_CA1996_1687",
                "v_CA1996_1614","v_CA1996_1627","v_CA1996_1701","v_CA1996_1702",
                "v_CA1996_1682", "v_CA1996_1681", "v_CA1996_784", "v_CA1996_783", "v_CA1996_128","v_CA1996_125",
                "v_CA1996_1387","v_CA1996_1385","v_CA1996_1396","v_CA1996_1394",
                "v_CA1996_1611","v_CA1996_1610","v_CA1996_472","v_CA1996_473",
                "v_CA1996_1356","v_CA1996_1347"),
    geo_format = "sf") %>% 
  st_transform(32618) %>% 
  select(-c(`Shape Area`:`Households`, CSD_UID, key, CT_UID, CD_UID:`Area (sq km)`)) %>% 
  set_names(c("GeoUID", "population", "dwellings", "tenure_total", "renter",
              "repairs_total", "major_repairs", "value_dwellings_total", "value_dwellings_avg",
              "HH_income_total","HH_income_median", "low_income", "low_income_total", 
              "gross_rent_avg", "thirty_renter", "vm", "vm_total", "immigrants", "immigrants_total", 
              "mobility_one_year", "mobility_one_year_total", "mobility_five_years", "mobility_five_years_total",
              "bachelor_above", "aboriginal_total", "aboriginal", "education_total",
              "geometry" )) %>%
  mutate(low_income_prop = low_income / low_income_total,
         renter_total = renter
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
  #select(-c(low_income_AT_prop)) %>% 
  as_tibble() %>% 
  st_as_sf(agr = "constant")
 
# 1996 in CT to check NAs
CT_96_test <-
  get_census(
    dataset = "CA1996", regions = list(CSD = c("2466025")), level = "DA",
    vectors = c("v_CA1996_1678", "v_CA1996_1683", "v_CA1996_1678","v_CA1996_1687",
                "v_CA1996_1614","v_CA1996_1627","v_CA1996_1701","v_CA1996_1702",
                "v_CA1996_1682", "v_CA1996_1681", "v_CA1996_784", "v_CA1996_783", "v_CA1996_128","v_CA1996_125",
                "v_CA1996_1387","v_CA1996_1385","v_CA1996_1396","v_CA1996_1394",
                "v_CA1996_1611","v_CA1996_1610","v_CA1996_472","v_CA1996_473",
                "v_CA1996_1356","v_CA1996_1347"),
    geo_format = "sf")

# test if nonmover+mover = total in 1996
test_2 <-
  view(get_census(
    dataset = "CA1996", regions = list(CSD = c("2466025")), level = "DA",
    vectors = c("v_CA1996_1395","v_CA1996_1396","v_CA1996_1394"),
    geo_format = "sf"))

test_2 <- test_2 %>% 
  rename(nonmover = "v_CA1996_1395: Non-movers",
         mover = "v_CA1996_1396: Movers")%>%
  mutate(sum_mobility = nonmover + mover)

sum(DA_96_test$population)

# correlation test --------------------------------------------------------
ggqqplot(var_DA_06_16$p_vm, ylab = "percentage of vm")
ggqqplot(var_DA_06_16$p_immigrants, ylab = "percentage of immigrants")

ggscatter(var_DA_06_16, x = "p_vm", y = "p_immigrants", 
          add = "reg.line", conf.int = FALSE,
          cor.coef = TRUE,cor.method = "pearson",
          xlab = "p_vm", ylab = "p_immigrants")

shapiro.test(var_DA_06_16$p_vm)


# Inflation adjustment 06 to 16 ----------------------------------------------------

var_DA_06_16_2 <- 
var_DA_06_16 %>%
  mutate(var_avg_rent_adjusted = 
           (average_rent - (gross_rent_avg_06*1.1741)) / (gross_rent_avg_06*1.1741),
         var_avg_value_dwelling_adjusted = 
           (average_value_dwellings - (value_dwellings_avg_06*1.1741)) / (value_dwellings_avg_06*1.1741),
         var_avg_median_HH_income_AT_adjusted = 
           (median_HH_income_AT-(HH_income_AT_median_06*1.1741)) / (HH_income_AT_median_06*1.1741))





# GI --------------------------------------------------------------
# metric 1: binary map ----
# step 1: gentrifiable criteria:
# 1. median_hh_income< CMA
# 2. bacholar_above< CMA
# (3. New housing construction proportion*< CMA)

Hmisc::wtd.quantile(var_DA_06_16$median_HH_income_AT, probs = c(.5), na.rm = TRUE)+ 
  sd(var_DA_06_16$median_HH_income_AT, na.rm = TRUE)
mean(var_DA_06_16$median_HH_income_AT, na.rm = TRUE)

# var_DA_06_16 is CSD
var_DA_06_16_2 <- 
  var_DA_06_16_2 %>% 
  mutate(var_avg_rent_adjusted = 
           (average_rent - (gross_rent_avg_06*1.1741)) / (gross_rent_avg_06*1.1741),
         var_avg_value_dwelling_adjusted = 
           (average_value_dwellings - (value_dwellings_avg_06*1.1741)) / (value_dwellings_avg_06*1.1741),
         var_avg_median_HH_income_AT_adjusted = 
           (median_HH_income_AT-(HH_income_AT_median_06*1.1741)) / (HH_income_AT_median_06*1.1741)) %>% 
  mutate(p_bachelor_above_06_csd = wtd.quantile(p_bachelor_above_06, probs = c(.5), na.rm = TRUE),
         HH_income_AT_median_06_csd = wtd.quantile(HH_income_AT_median_06, probs = c(.5), na.rm = TRUE)) %>% 
  mutate(gentrifiable = ifelse(p_bachelor_above_06 < p_bachelor_above_06_csd & 
                              HH_income_AT_median_06 < HH_income_AT_median_06_csd, TRUE, FALSE))

# step2: Gentrified criteria:
# Demographic change
# 1.The increase proportion of people with bachelor degree > CMA
# 2.The increase proportion of housing ownership > CMA (--> renter decline??)
# 3.The increase Median household  income > CMA 
# 4.The increase of professional occupation > CMA 
# Reinvestment
#1. The increase of Housing value > CMA
#2. The increase of Monthly rent  > CMA

var_DA_06_16_2 <- 
var_DA_06_16_2 %>% 
  mutate(var_bacholar_above = p_bachelor_above - p_bachelor_above_06,
         var_bachelor_above_median = wtd.quantile(var_bacholar_above, probs = c(.5), na.rm = TRUE),
         # should change to ownership in new dataset
         var_prop_renter_median = wtd.quantile(var_prop_renter, probs = c(.5), na.rm = TRUE),
         var_median_hh_income_median = wtd.quantile(var_avg_median_HH_income_AT_adjusted, probs = c(.5), na.rm = TRUE),
         # var_profession_median = wtd.quantile(var_profession, probs = c(.5), na.rm = TRUE),
         var_avg_value_dwelling_median = wtd.quantile(var_avg_value_dwelling_adjusted, probs = c(.5), na.rm = TRUE),
         var_avg_rent_median = wtd.quantile(var_avg_rent_adjusted, probs = c(.5), na.rm = TRUE),
         var_prop_vm_median = wtd.quantile(var_prop_vm, probs = c(.5), na.rm = TRUE)
         )%>% 
  mutate(gentrified = ifelse(gentrifiable & 
                               var_bacholar_above > var_bachelor_above_median &
                               var_prop_renter < var_prop_renter_median &
                               var_avg_median_HH_income_AT_adjusted > var_median_hh_income_median &
                               var_avg_value_dwelling_adjusted > var_avg_value_dwelling_median &
                               var_avg_rent_adjusted > var_avg_rent_median, TRUE, FALSE))

# create graphs  
gentrifiable_06_map <- 
  var_DA_06_16_2 %>% 
  ggplot()+
  geom_sf(aes(fill = gentrifiable), colour = NA)+
  scale_fill_manual(name = "Gentriable in 2006",
                    values = col_palette[c(5, 1)], na.value = "grey60")+
  theme_void()

gentrified_06_16_map <- 
  var_DA_06_16_2 %>% 
  ggplot()+
  geom_sf(aes(fill = gentrified), colour = NA)+
  #geom_sf(data = boroughs, fill = NA, colour = "grey20", lwd= 0.5)+
  scale_fill_manual(name = "Gentried through\n2006 to 2016",
                    values = col_palette[c(5, 1)], na.value = "grey60")+
  theme_void()



# metric 2: : quartile score ---- 
#indicators qualified in the process of gentrification:
# Changes in percentage of people with bachelor degrees
# Changes in professional occupation  *
# Changes in median household  income
# Changes in Housing value 
# Changes in Monthly rent
# Changes in proportion of housing ownership

# <25%--1
# 25-50%--2
# 50--75%--3
# >75%--4

var_list <- c("var_bacholar_above", "var_avg_median_HH_income_AT_adjusted", "var_avg_value_dwelling_adjusted","var_avg_rent_adjusted", 
              "var_prop_renter", "var_prop_vm") 

gentrif_q <- 
var_DA_06_16_test %>% 
  mutate(across(all_of(var_list), ntile, 4, .names = "{.col}_q4"))

q_list <- str_subset(names(gentrif_q), "_q4")

#gentrif_q %>% 
#    mutate(across(all_of(q_list), ~(./4)))

v2 <- gentrif_q %>% 
  mutate (sum_variable_qq = (var_bacholar_above_q4 + var_avg_median_HH_income_AT_adjusted_q4+
                              var_avg_value_dwelling_adjusted_q4 + var_avg_rent_adjusted_q4-
                              var_prop_renter_q4 - var_prop_vm_q4)) %>% 
  mutate(sum_variable_qq2 = scales::rescale(v2$sum_variable_qq, to= c(0, 1)))  #%>%
  #group_by(sum_variable_qq2) %>% 
  #dplyr::summarize(n()) 



# metric: Z-score draft---------------------------------------------------------
v2_test <- 
  v2 %>% 
  mutate(across(where(is.numeric), ~replace(., is.infinite(.), NA))) #replace Inf by NAs

v2_test <- na.omit(v2_test) #for taking out the NAs
# 295 removed

var_z <- 
  v2_test %>% 
  select(var_bacholar_above, var_avg_median_HH_income_AT_adjusted, 
         var_avg_value_dwelling_adjusted,var_avg_rent_adjusted, 
         var_prop_renter, var_prop_vm) %>%
  rename_with(~paste0(.,"_z"), -geometry) %>% 
  st_drop_geometry()

var_zscore <-scale(var_z)
GI_06_16 <- cbind(v2_test,var_zscore)

GI_06_16 <- 
  GI_06_16 %>% 
  mutate(sum_z = var_bacholar_above_z + var_avg_median_HH_income_AT_adjusted_z + var_avg_value_dwelling_adjusted_z
         +var_avg_rent_adjusted_z -var_prop_renter_z -var_prop_vm_z) 

v2_z_map <-
  GI_06_16 %>% 
  ggplot()+
  geom_sf(aes(fill = sum_z), colour = NA)+
  geom_sf(data = boroughs, fill = NA, colour = "grey20", lwd= 0.3)+
  scale_fill_viridis_c( name="Neighbourhood Change\n2006 to 2016",
                        limits = c(-2.5, 2.5), 
                        oob = scales::squish)+
  #scale_fill_gradientn(name="Neighbourhood Change\n2006 to 2016",
  #                    #n.breaks = 3,
  #                    #breaks = c(0.3, 0.6, 0.9),
  #                    colours=col_palette[c(9,1,4)],
  #                     limits = c(0, 1),
  #                     oob = scales::squish)+
  theme_void()
  

# cleaned metric: Z-score ---------------------------------------------------------

# mutate var_ columns
# DA level/ 06-16 as example

process_change_data <- function (variable, x, y){
DA %>% 
mutate(variable = variable_x - variable_y/ variable_y)}


DA %>% 
  mutate(var_bacholar_above_prop = bacholar_above_prop_16 - bacholar_above_prop_06/ bacholar_above_prop_06,
         var_inc_median_dollar_adjusted_prop = inc_median_dollar_adjusted_16- inc_median_dollar_adjusted_06/ inc_median_dollar_adjusted_06,
         var_housing_value_avg_adjusted_prop = housing_value_avg_adjusted_16- housing_value_avg_adjusted_06/ housing_value_avg_adjusted_06,
         var_housing_rent_avg_adjusted_prop = housing_rent_avg_adjusted_16- housing_rent_avg_adjusted_06/ housing_rent_avg_adjusted_06,
         var_housing_tenant_prop = housing_tenant_prop_16- housing_tenant_prop_06/ housing_tenant_prop_06,
         var_imm_vm_prop = imm_vm_prop_16- imm_vm_prop_06/ imm_vm_prop_06)
# "var_xxx"--> prefix of percentage of change from last census year
# "_adjusted_" --> use inflation-adjusted dollar


# deal with NA and Inf
DA_test <- 
  DA %>% 
  mutate(across(where(is.numeric), ~replace(., is.infinite(.), NA))) #replace Inf by NAs
DA_test <- na.omit(DA_test) #for taking out the NAs

# make list for indicators to make z-score
var_z <- 
  DA_test %>% 
  select(var_bacholar_above_prop, var_inc_median_dollar_adjusted_prop, 
         var_housing_value_avg_adjusted_prop,var_housing_rent_avg_adjusted_prop, 
         var_housing_tenant_prop, var_imm_vm_prop) %>%    #these two are negative corelation
  rename_with(~paste0(.,"_z"), -geometry) %>% 
  st_drop_geometry()

var_z <-scale(var_z)
DA_test <- cbind(DA_test, var_z)

DA_test <- 
  DA_test %>% 
  mutate(sum_z = var_bacholar_above_z + var_avg_median_HH_income_AT_adjusted_z + var_avg_value_dwelling_adjusted_z
         +var_avg_rent_adjusted_z -var_prop_renter_z -var_prop_vm_z) 

rm(var_z)

# create graphs
GI_map <-
  DA_test %>% 
  ggplot()+
  geom_sf(aes(fill = sum_z), colour = NA)+
  geom_sf(data = boroughs, fill = NA, colour = "grey20", lwd= 0.3)+
  scale_fill_viridis_c( name="Neighbourhood Change\n2006 to 2016",
                        limits = c(-2.5, 2.5),  #need to try if this limits works throughout years
                        oob = scales::squish)+
  theme_void()


#### Raster, Contour map----
GI_06_16 %>% 
  extract(geometry, c('lat', 'lon'), '\\((.*), (.*)\\)', convert = TRUE) %>% 
  select(lat, lon) %>% 
  as_tibble()

GI_06_16 %>% 
  ggplot()+
  geom_sf(aes(x = longitude, y = latitude))+
  geom_raster(aes(fill = sum_z))+
  scale_fill_gradientn(name="Gentrification",
                       colours=col_palette[c(4, 1, 2, 9)] #,
                       #limits = c(10, 200),
                       #oob = scales::squish
                       )
  #scale_fill_gradientn(name="Neighbourhood Change\n2006 to 2016",
  #                    #n.breaks = 3,
  #                    #breaks = c(0.3, 0.6, 0.9),
  #                    colours=col_palette[c(9,1,4)],
  #                     limits = c(0, 1),
  #                     oob = scales::squish)+
  #theme_void()
  
# other analysis ----
GI_06_16 %>% 
  ggplot(aes(x = sum_variable_qq, y = p_thirty_renter))+
  geom_jitter(aes())+
  ylim(c(0, 1))

GI_06_16 %>% 
  ggplot(aes(x = sum_z))+
  geom_histogram()+
  xlim(c(-4,4))

#comparing 
#bi_compare <- function(v2, variable){}
bi <-
   bi_class(na.omit(v2), x = sum_variable_qq, y = p_repairs, style = "quantile", dim = 3) 

plot <- 
   ggplot() +
   geom_sf(data = bi, mapping = aes(fill = bi_class), color = NA, size = 0.1, show.legend = FALSE) +
   bi_scale_fill(pal = bivar, dim = 3) +
   bi_theme()+
   theme(legend.position = "bottom")

bi_legend <- bi_legend(pal = bivar,
                       dim = 3,
                       xlab = "GI",
                       ylab = "repairs",
                       size = 8)

bi_gen_thirty_map <- plot + inset_element(bi_legend, left = 0, bottom = 0.6, right = 0.4, top = 1)

bi_gen_thirty_map 
 
# add vm
var_DA_06_16_test <- 
  var_DA_06_16_2 %>%
  mutate(var_prop_vm_median = wtd.quantile(var_prop_vm, probs = c(.5), na.rm = TRUE))

#save plot
ggsave("output/figures/v2_map.pdf", plot = v2_map, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)
