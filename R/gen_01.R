library(ggpubr)
# neighbourhood change
# 2006, 2016
# trend in neighbourhood change/ story and key pattern

view(cancensus::list_census_vectors("CA16"))
View(cancensus::list_census_regions("CA16"))

# CT boundary change
# data

# run correlation test
head(DA,6)
ggscatter(DA, x = "p_renter", y = "p_immigrants", 
          add = "reg.line", conf.int = FALSE,
          cor.coef = TRUE,cor.method = "spearman",
          xlab = "percentage of renter", ylab = "percentage of immigrants")

shapiro.test(DA$p_immigrants)
ggqqplot(DA$p_renter, ylab = "percentage of renter")
ggqqplot(DA$p_immigrants, ylab = "percentage of immigrants")

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
renterpropchange_06_16 <-
  var_DA_06_16 %>% 
  ggplot()+
  geom_sf(mapping = aes(fill=var_prop_renter), color=NA)+
  scale_fill_gradient2(name="Percentage of renter proportion change",
                       low = col_palette[3],
                       mid = "white",
                       high = col_palette[1],
                       midpoint = 0,
                       label = scales::percent,
                       oob = scales::squish,
                       limits = c(-0.5, 0.5))+
  theme_void()

ggsave("output/figures/renterpropchange_06_16.pdf", plot = renterpropchange_06_16, width = 8, 
       height = 4.2, units = "in", useDingbats = FALSE)

# map: hh income change/ 06-16/ DA
# inflation?
medianhhincomechange_06_16 <-var_DA_06_16 %>% 
  ggplot()+
  geom_sf(mapping = aes(fill=var_median_HH_income_AT), color=NA)+
  scale_fill_gradient2(name="Percentage of household income change",
                       low = col_palette[3],
                       mid = "white",
                       high = col_palette[1],
                       midpoint = 0,
                       label = scales::percent,
                       oob = scales::squish,
                       limits = c(-0.5, 0.5))+
  theme_void()

ggsave("output/figures/medianhhincomechange_06_16.pdf", plot = medianhhincomechange_06_16, width = 8, 
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


# dwelling value
var_DA_06_16 %>% 
  ggplot()+
  geom_sf(mapping = aes(fill=var_avg_value_dwelling), color=NA)+
  scale_fill_gradient2(name="Percentage of average dwelling value change",
                       low = col_palette[3],
                       mid = "white",
                       high = col_palette[1],
                       midpoint = 0,
                       label = scales::percent,
                       #oob = scales::squish,
                       limits = c(-0.5, 0.5))+
  theme_void()

# repairs proportion
proprepairs_06_16 <- var_DA_06_16 %>% 
  ggplot()+
  geom_sf(mapping = aes(fill=var_prop_repairs), color=NA)+
  scale_fill_gradient2(name="Percentage of repairment proportion change",
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
prop_fiveyears_06_16_ns <- var_DA_06_16 %>% 
  ggplot()+
  geom_sf(mapping = aes(fill=var_prop_mobility_five_years), color=NA)+
  scale_fill_gradient2(name="Percentage of mobility in five years proportion change",
                       low = col_palette[3],
                       mid = "white",
                       high = col_palette[1],
                       midpoint = 0,
                       label = scales::percent,
                       #oob = scales::squish,
                       limits = c(-0.5, 0.5))+
  theme_void()

ggsave("output/figures/prop_fiveyears_06_16_ns.pdf", plot = prop_fiveyears_06_16_ns, width = 8, 
       height = 4.2, units = "in", useDingbats = FALSE)

# prop of visible minority
prop_vm_06_16 <- var_DA_06_16 %>% 
  ggplot()+
  geom_sf(mapping = aes(fill=var_prop_vm), color=NA)+
  scale_fill_gradient2(name="Percentage of mobility in five years proportion change",
                       low = col_palette[3],
                       mid = "white",
                       high = col_palette[1],
                       midpoint = 0,
                       label = scales::percent,
                       oob = scales::squish,
                       limits = c(-0.5, 0.5))+
  theme_void()

ggsave("output/figures/prop_vm_06_16.pdf", plot = prop_vm_06_16, width = 8, 
       height = 4.2, units = "in", useDingbats = FALSE)


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

renter_prop-renter <-
  bi_class(na.omit(var_DA_06_16), x = var_renter, y = var_prop_renter, style = "quantile", dim = 3) #na.omit() is useful when analyses require you to have a dataset free of NA values!

# Plot for the bivariate choropleth map

plot <- 
  ggplot() +
  geom_sf(data = BIVARIATE_DATASET_NAME, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = bivar, dim = 3) +
  bi_theme()+
  theme(legend.position = "bottom")

plot #to see your plot

# Add bivariate legend
bi_legend <- bi_legend(pal = bivar,
                       dim = 3,
                       xlab = "Percentage of VARIABLE_X",
                       ylab = "Percentage of VARIABLE_Y",
                       size = 8)

PLOT_FINAL_NAME <- plot + inset_element(bi_legend, left = 0, bottom = 0.6, right = 0.4, top = 1)

PLOT_FINAL_NAME

# Save in PDF in your output/figures folder to see the true sizes of your plot, ajust accordingly

ggsave("output/figures/PLOT_FINAL_NAME.pdf", plot = PLOT_FINAL_NAME, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)
