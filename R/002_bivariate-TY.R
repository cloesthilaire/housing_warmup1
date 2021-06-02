library(biscale)
library(scales)
library(cowplot)
library(patchwork)

view(DA)
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

reim <-
  bi_class(na.omit(DA), x = p_renter, y = p_immigrants, 
           style = "quantile", dim = 3)
#NA become one variable-->na.omit() to get rid of NA

plot(reim)

mapreim <- 
  ggplot() +
  geom_sf(data = reim, mapping = aes(fill = bi_class), 
          color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = bivar, dim = 3) +
  bi_theme()+
  theme(legend.position = "bottom")

mapreim

bi_legend <- bi_legend(pal = bivar,
                       dim = 3,
                       xlab = "Percentage of Renter",
                       ylab = "Percentage of Immigrants",
                       size = 8)

# the legend 2
plotlegend <- mapreim + 
  inset_element(bi_legend, left = 0, bottom = 0.6, right = 0.4, top = 1)

plotlegend

# the legend1 --ggdraw
# mapreim2 <- cowplot::ggdraw() + draw_plot(mapreim, 0, 0, 1, 1) + draw_plot(bi_legend, 0.1, 0.1, 0, 0)

#save
ggsave("output/figures/plotlegend.pdf", plot = plotlegend, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

