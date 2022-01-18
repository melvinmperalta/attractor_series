library(tidyverse)
library(ggplot2)
library(Rcpp)

# Import C++ code
sourceCpp('generate_points.cpp')

df <- ikeda_shape(175000, 0, 0, 0.8044083, 0.3449557, -3.86561, 0.01, 5)

tiff("day15_1.tiff", height = 3000, width = 3000, units = "px", pointsize = 12, res = 300, compression = 'lzw')
x <- densCols(df$x,df$y, colramp=colorRampPalette(c("black", "white")))
df$dens <- col2rgb(x)[1,] + 1L
cols <-  colorRampPalette(c(hcl(80, 50, 90),
                            hcl(60, 80, 80)))(256)
df$col <- cols[df$dens]
ggplot(data = df, aes(x = x, y = y, colour = col)) +
  geom_point(size = 0.001, alpha = 0.5) +
  scale_color_identity() +
  coord_equal() +
  theme_void() +
  theme(legend.position = "none", plot.background = element_rect(fill = "firebrick4"))

dev.off()
