library(ggpubr)
# neighbourhood change
# 2001, 2006, 2016
# trend in neighbourhood change/ story and key pattern

view(cancensus::list_census_vectors("CA06"))
View(cancensus::list_census_regions("CA1996"))
View(cancensus::list_census_vectors("CA1996"))

# get data for 2001
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

glimpse(DA_01_test)
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

CT_96_test <-
  view(get_census(
    dataset = "CA1996", regions = list(CSD = c("2466025")), level = "CT",
    vectors = c("v_CA1996_1678", "v_CA1996_1683", "v_CA1996_1678","v_CA1996_1687",
                "v_CA1996_1614","v_CA1996_1627","v_CA1996_1701","v_CA1996_1702",
                "v_CA1996_1682", "v_CA1996_1681", "v_CA1996_784", "v_CA1996_783", "v_CA1996_128","v_CA1996_125",
                "v_CA1996_1387","v_CA1996_1385","v_CA1996_1396","v_CA1996_1394",
                "v_CA1996_1611","v_CA1996_1610","v_CA1996_472","v_CA1996_473",
                "v_CA1996_1356","v_CA1996_1347"),
    geo_format = "sf"))
view(CT_96_test)

# boundary change
# correlation test --------------------------------------------------------
head(DA,6)
ggscatter(var_DA_06_16, x = "var_dwellings", y = "var_population", 
          add = "reg.line", conf.int = FALSE,
          cor.coef = TRUE,cor.method = "spearman",
          xlab = "dwelling change", ylab = "population change")

ggscatter(DA_01_test, x = "renter", y = "renter_total", 
          add = "reg.line", conf.int = FALSE,
          cor.coef = TRUE,cor.method = "pearson",
          xlab = "renter", ylab = "renter_total")

shapiro.test(DA$p_immigrants)
ggqqplot(var_DA_06_16$p_renter, ylab = "percentage of renter")
ggqqplot(DA$p_immigrants, ylab = "percentage of immigrants")

# Inflation adjustment ----------------------------------------------------

var_DA_06_16 <- 
var_DA_06_16 %>%
  mutate(var_avg_rent_adjusted = 
           (average_rent - (gross_rent_avg_06*1.1741)) / (gross_rent_avg_06*1.1741),
         var_avg_value_dwelling_adjusted = 
           (average_value_dwellings - (value_dwellings_avg_06*1.1741)) / (value_dwellings_avg_06*1.1741),
         var_avg_median_HH_income_AT_adjusted = 
           (median_HH_income_AT-(HH_income_AT_median_06*1.1741)) / (HH_income_AT_median_06*1.1741))

glimpse(var_DA_06_16)


# make graphs -------------------------------------------------------------

# map for tenant change
# change0616_tenant_map <- 
#DA %>% 
  #ggplot()+
  #geom_sf(data = province, fill="grey90", color=NA)+
  #geom_sf(mapping = aes(fill=p_renter), color=NA) +
  #scale_fill_gradientn(name="Percentage of renter change",
  #                   colors=col_palette[c(4,1,9)],
  #                    labels = scales::percent) +
  #coord_sf(xlim = c(581000, 621000), ylim = c(5027500, 5065000),
  #        expand = FALSE) +
  #ggtitle("Tenant change from 2006 to 2016")+
  #theme_void()

#map: dwellings change/ 06-16/ DA
dwellingschange_06_16 <- 
  var_DA_06_16 %>% 
  ggplot()+
  geom_sf(mapping = aes(fill=var_dwellings), color=NA)+
  scale_fill_gradient2(name="Percentage of dwellings change",
                       low = col_palette[3],
                       mid = "white",
                       high = col_palette[1],
                       midpoint = 0,
                       label = scales::percent,
                       #oob = scales::squish,
                       limits = c(-0.5, 0.5))+
  theme_void()

ggsave("output/figures/dwellingschange_06_16_ns.pdf", plot = dwellingschange_06_16_ns, width = 8, 
       height = 4.2, units = "in", useDingbats = FALSE)

#map: population change/ 06-16/ DA
popchange_06_16_ns <- 
  var_DA_06_16 %>% 
  ggplot()+
  geom_sf(mapping = aes(fill=var_population), color=NA)+
  scale_fill_gradient2(name="Percentage of population change",
                       low = col_palette[3],
                       mid = "white",
                       high = col_palette[1],
                       midpoint = 0,
                       label = scales::percent,
                       #oob = scales::squish,
                       limits = c(-0.5, 0.5))+
  theme_void()

ggsave("output/figures/popchange_06_16_ns.pdf", plot = popchange_06_16_ns, width = 8, 
       height = 4.2, units = "in", useDingbats = FALSE)

#map: renter change/ 06-16/ DA
renterchange_06_16_ns <-
  var_DA_06_16 %>% 
  ggplot()+
  geom_sf(mapping = aes(fill=var_renter), color=NA)+
  scale_fill_gradient2(name="Percentage of renter change",
                       low = col_palette[3],
                       mid = "white",
                       high = col_palette[1],
                       midpoint = 0,
                       label = scales::percent,
                       #oob = scales::squish,
                       limits = c(-0.5, 0.5))+
  theme_void()

ggsave("output/figures/renterchange_06_16_ns.pdf", plot = renterchange_06_16_ns, width = 8, 
       height = 4.2, units = "in", useDingbats = FALSE)

# map: proportion of renter
renterpropchange_06_16_ns <-
  var_DA_06_16 %>% 
  ggplot()+
  geom_sf(mapping = aes(fill=var_prop_renter), color=NA)+
  scale_fill_gradient2(name="Percentage of renter proportion change",
                       low = col_palette[3],
                       mid = "white",
                       high = col_palette[1],
                       midpoint = 0,
                       label = scales::percent,
                       #oob = scales::squish,
                       limits = c(-0.5, 0.5))+
  theme_void()

ggsave("output/figures/renterpropchange_06_16_ns.pdf", plot = renterpropchange_06_16_ns, width = 8, 
       height = 4.2, units = "in", useDingbats = FALSE)

# map: hh income change adjusted/ 06-16/ DA
medianhhincomechange_ad_06_16_ns <-var_DA_06_16 %>% 
  ggplot()+
  geom_sf(mapping = aes(fill=var_avg_median_HH_income_AT_adjusted), color=NA)+
  scale_fill_gradient2(name="Percentage of household income change",
                       low = col_palette[3],
                       mid = "white",
                       high = col_palette[1],
                       midpoint = 0,
                       label = scales::percent,
                       oob = scales::squish,
                       limits = c(-0.5, 0.5))+
  theme_void()

ggsave("output/figures/medianhhincomechange_ad_06_16_ns.pdf", plot = medianhhincomechange_ad_06_16_ns, width = 8, 
       height = 4.2, units = "in", useDingbats = FALSE)


# low income proportion
# weird!
propLIMAT_06_16_ns <- var_DA_06_16 %>% 
  ggplot()+
  geom_sf(mapping = aes(fill=var_prop_LIMAT), color=NA)+
  scale_fill_gradient2(name="Percentage of low income proportion change",
                       low = col_palette[3],
                       mid = "white",
                       high = col_palette[1],
                       midpoint = 0,
                       label = scales::percent,
                       #oob = scales::squish,
                       limits = c(-0.5, 0.5))+
  theme_void()

ggsave("output/figures/mpropLIMAT_06_16_ns.pdf", plot = propLIMAT_06_16_ns, width = 8, 
       height = 4.2, units = "in", useDingbats = FALSE)


# dwelling value adjusted
dewelling_value_06_16_ns <- var_DA_06_16 %>% 
  ggplot()+
  geom_sf(mapping = aes(fill=var_avg_value_dwelling_adjusted), color=NA)+
  scale_fill_gradient2(name="Percentage of average dwelling value change",
                       low = col_palette[3],
                       mid = "white",
                       high = col_palette[1],
                       midpoint = 0,
                       label = scales::percent,
                       #oob = scales::squish,
                       limits = c(-0.5, 0.5))+
  theme_void()

ggsave("output/figures/dewelling_value_06_16_ns.pdf", plot = dewelling_value_06_16_ns, width = 8, 
       height = 4.2, units = "in", useDingbats = FALSE)

# rent adjusted
rent_06_16_ns <- var_DA_06_16 %>% 
  ggplot()+
  geom_sf(mapping = aes(fill=var_avg_rent_adjusted), color=NA)+
  scale_fill_gradient2(name="Percentage of rent change",
                       low = col_palette[3],
                       mid = "white",
                       high = col_palette[1],
                       midpoint = 0,
                       label = scales::percent,
                       #oob = scales::squish,
                       limits = c(-0.5, 0.5))+
  theme_void()

ggsave("output/figures/rent_06_16_ns.pdf", plot = rent_06_16_ns, width = 8, 
       height = 4.2, units = "in", useDingbats = FALSE)

# renter spending 30% or more income on housing
prop_30renter_06_16 <- var_DA_06_16 %>% 
  ggplot()+
  geom_sf(mapping = aes(fill=var_prop_thirty_renter), color=NA)+
  scale_fill_gradient2(name="Percentage of renter spending 30%\nor more income on housing proportion change",
                       low = col_palette[3],
                       mid = "white",
                       high = col_palette[1],
                       midpoint = 0,
                       label = scales::percent,
                       oob = scales::squish,
                       limits = c(-0.5, 0.5))+
  theme_void()

ggsave("output/figures/prop_30renter_06_16.pdf", plot = prop_30renter_06_16, width = 8, 
       height = 4.2, units = "in", useDingbats = FALSE)


# repairs proportion
proprepairs_06_16 <- var_DA_06_16 %>% 
  ggplot()+
  geom_sf(mapping = aes(fill=var_prop_repairs), color=NA)+
  scale_fill_gradient2(name="Percentage of housing need repairs proportion change",
                       low = col_palette[3],
                       mid = "white",
                       high = col_palette[1],
                       midpoint = 0,
                       label = scales::percent,
                       oob = scales::squish,
                       limits = c(-0.5, 0.5))+
  theme_void()

ggsave("output/figures/proprepairs_06_16.pdf", plot = proprepairs_06_16, width = 8, 
       height = 4.2, units = "in", useDingbats = FALSE)

# prop one year mobility
prop_oneyear_06_16_ns <- var_DA_06_16 %>% 
  ggplot()+
  geom_sf(mapping = aes(fill=var_prop_mobility_one_year), color=NA)+
  scale_fill_gradient2(name="Percentage of mobility in one year proportion change",
                       low = col_palette[3],
                       mid = "white",
                       high = col_palette[1],
                       midpoint = 0,
                       label = scales::percent,
                       #oob = scales::squish,
                       limits = c(-0.5, 0.5))+
  theme_void()

ggsave("output/figures/prop_oneyear_06_16_ns.pdf", plot = prop_oneyear_06_16_ns, width = 8, 
       height = 4.2, units = "in", useDingbats = FALSE)

# prop five year mobility
prop_fiveyears_06_16 <- var_DA_06_16 %>% 
  ggplot()+
  geom_sf(mapping = aes(fill=var_prop_mobility_five_years), color=NA)+
  scale_fill_gradient2(name="Percentage of mobility in five years proportion change",
                       low = col_palette[3],
                       mid = "white",
                       high = col_palette[1],
                       midpoint = 0,
                       label = scales::percent,
                       oob = scales::squish,
                       limits = c(-0.5, 0.5))+
  theme_void()

ggsave("output/figures/prop_fiveyears_06_16.pdf", plot = prop_fiveyears_06_16, width = 8, 
       height = 4.2, units = "in", useDingbats = FALSE)

# prop of visible minority
prop_vm_06_16_ns <- var_DA_06_16 %>% 
  ggplot()+
  geom_sf(mapping = aes(fill=var_prop_vm), color=NA)+
  scale_fill_gradient2(name="Percentage of visible minority proportion change",
                       low = col_palette[3],
                       mid = "white",
                       high = col_palette[1],
                       midpoint = 0,
                       label = scales::percent,
                       #oob = scales::squish,
                       limits = c(-0.5, 0.5))+
  theme_void()

ggsave("output/figures/prop_vm_06_16_ns.pdf", plot = prop_vm_06_16_ns, width = 8, 
       height = 4.2, units = "in", useDingbats = FALSE)

# Save data



# bivariate map--
# palette
bivar <- bi_pal_manual(val_1_1 = "#e8e8e8",
                       val_1_2 = "#b8d6be",
                       val_2_1 = "#b5c0da", 
                       val_2_2 = "#90b2b3", 
                       val_3_1 = "#6c83b5",  
                       val_3_2 = "#567994", 
                       val_1_3 = "#73ae80", 
                       val_2_3 = "#5a9178",
                       val_3_3 = "#2a5a5b", preview=FALSE)

show_col(bivar)

# Prepare the dataset to display in a bivariate choropleth map

renter_proprenter <-
  bi_class(na.omit(var_DA_06_16), x = var_renter, y = var_prop_renter, style = "quantile", dim = 3) #na.omit() is useful when analyses require you to have a dataset free of NA values!

# Plot for the bivariate choropleth map

plot <- 
  ggplot() +
  geom_sf(data = renter_proprenter, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = bivar, dim = 3) +
  bi_theme()+
  theme(legend.position = "bottom")

plot #to see your plot

# Add bivariate legend
bi_legend <- bi_legend(pal = bivar,
                       dim = 3,
                       xlab = "Renter change",
                       ylab = "Renter proportion change",
                       size = 8)

renter_proprenter_map <- plot + inset_element(bi_legend, left = 0, bottom = 0.6, right = 0.4, top = 1)

renter_proprenter_map

# Save in PDF in your output/figures folder to see the true sizes of your plot, ajust accordingly

ggsave("output/figures/renter_proprenter_map.pdf", plot = renter_proprenter_map, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)


