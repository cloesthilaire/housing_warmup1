library(biscale)
library(scales)

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

NAMEOFYOURNEWBIVARIATE <-
  bi_class(YOURDATASET, x = FIRSTVARIABLE, y = SECONDVARIABLE, style = "quantile", dim = 3)

# Plot for the bivariate choropleth map

PLOTNAME <- 
  ggplot() +
  geom_sf(data = NAMEOFYOURNEWBIVARIATE, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = bivar, dim = 3) +
  bi_theme()+
  theme(legend.position = "bottom")

PLOTNAME #to see your plot

# Add bivariate legend

bi_legend <- bi_legend(pal = bivar,
                       dim = 3,
                       xlab = "NAME OF YOUR FIRST VARIABLE",
                       ylab = "NAME OF YOUR SECOND VARIABLE",
                       size = 20)

PLOTNAMEWITHLEGEND <- ggdraw() +
  draw_plot(PLOTNAME, 0, 0, 1, 1) +
  draw_plot(bi_legend, 0.11, 0.11, 0.11, 0.15)

PLOTNAMEWITHLEGEND

# Save in PDF in your output/figures folder to see the true sizes of your plot, ajust accordingly

ggsave("output/figures/PLOTNAMEWITHLEGEND.pdf", plot = PLOTNAMEWITHLEGEND, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)