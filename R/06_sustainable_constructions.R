new_con <- 
  uef_raw %>% 
  filter(ANNEE_CONS >= 2015) %>% 
  filter(ANNEE_CONS < 2025) %>% 
  group_by(CIVIQUE_DE, CIVIQUE_FI, NOM_RUE, ANNEE_CONS) %>% 
  summarize(number_units=sum(NOMBRE_LOG, na.rm=TRUE)) %>% 
  filter(number_units > 15)

plot1 <- 
  new_con %>% 
  set_names(c("civic_start", "civic_end", "street_name", "year_construction", "number_units", "geometry")) %>% 
  mutate(civic_start = as.numeric(civic_start), civic_end = as.numeric(civic_end)) %>% 
  right_join(., sus_survey_dev, by = c("civic_start", "civic_end", "street_name", "number_units")) %>% 
  st_as_sf() %>% 
  mutate(geometry = st_centroid(geometry)) %>% 
  filter(Sustainability == 1) %>% 
  ggplot() +
  geom_sf(data = province, fill="grey90", color=NA)+
  geom_sf(aes(size=number_units, color=year_construction))+
  geom_rect(xmin = 607600, ymin = 5036000, xmax = 614600, ymax = 5043000,
            fill = NA, colour = "black", size = 0.3)+
  scale_size_binned(name = "Number of units",
                    breaks = c(10, 50, 100, 300, 400, 500))+
  scale_color_gradient(name = "Year of construction",
                       low = col_palette[8], high = col_palette[9])+
  gg_bbox(boroughs)+
  theme_void()

plot1_zoom <- 
  new_con %>% 
  set_names(c("civic_start", "civic_end", "street_name", "year_construction", "number_units", "geometry")) %>% 
  mutate(civic_start = as.numeric(civic_start), civic_end = as.numeric(civic_end)) %>% 
  right_join(., sus_survey_dev, by = c("civic_start", "civic_end", "street_name", "number_units")) %>% 
  st_as_sf() %>% 
  mutate(geometry = st_centroid(geometry)) %>% 
  filter(Sustainability == 1) %>% 
  ggplot() +
  geom_sf(data = province, fill="grey90", color=NA)+
  geom_sf(data = streets_downtown, size = 0.3, colour = "white") +
  geom_sf(aes(size=number_units, color=year_construction), show.legend = FALSE)+
  scale_size_binned(breaks = c(10, 50, 100, 300, 400, 500))+
  scale_color_gradient(low = col_palette[8], high = col_palette[9])+
  coord_sf(xlim = c(607600, 614600), ylim = c(5036000, 5043000),
           expand = FALSE) +
  theme_void()+
  theme(legend.position = "none",
        plot.background = element_rect(color = "white"),
        panel.border = element_rect(fill = NA, colour = "black", size = 1))

sustainability_branded_constructions <- 
  plot1 + 
  inset_element(plot1_zoom, left = 0, bottom = 0.5, right = 0.6, top = 1)

ggsave("output/figures/sustainability_branded_constructions.pdf", plot = sustainability_branded_constructions, width = 8, 
       height = 4.2, units = "in", useDingbats = FALSE)


new_con %>% 
  set_names(c("civic_start", "civic_end", "street_name", "year_construction", "number_units", "geometry")) %>% 
  mutate(civic_start = as.numeric(civic_start), civic_end = as.numeric(civic_end)) %>% 
  right_join(., sus_survey_dev, by = c("civic_start", "civic_end", "street_name", "number_units")) %>% 
  filter(Sustainability == 1) %>% 
  ggplot()+
  geom_histogram(aes(x=year_construction), binwidth = 0.5)
