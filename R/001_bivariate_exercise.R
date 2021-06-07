library(biscale)
library(scales)
library(patchwork)

# Set the bivariate color palette

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

BIVARIATE_DATASET_NAME <-
  bi_class(na.omit(DATASET), x = VARIABLE_X, y = VARIABLE_Y, style = "quantile", dim = 3) #na.omit() is useful when analyses require you to have a dataset free of NA values!

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
