library(tidyverse)
# install.packages(c("raster","rgdal"), dependencies=TRUE)
library(raster)

shp <- raster::shapefile("data/shapefiles/goiania_regiao_metropolitana/goiania_regiao_metropolitana")
zonas <- raster::shapefile("data/shapefiles/zonas_trafego/zonas_trafego")

plot(shp, col="#f2f2f2", bg="gray", lwd=0.95, border=1)
plot(zonas, col="white", bg="gray", lwd=0.95, border=1)

ggplot2::ggplot() + 
    geom_polygon(data = zonas, 
                 aes(x = long, y = lat, group = group), 
                 colour = "black", 
                 fill = NA) +
    theme_void()

# shp_df <- broom::tidy(shp, region = "CD_GEOCODM")
# lapply(shp_df, class)
# 
# ggplot() +
#     geom_sf(data = shp, size = 3, color = "black", fill = "cyan1") +
#     ggtitle("AOI Boundary Plot") +
#     coord_sf()
# 
# library(broom)
# shp_fortified <- tidy(shp, region = "NAMES")
# 
# # Plot it
# library(ggplot2)
# ggplot() +
#     geom_polygon(data = shp_fortified, aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
#     theme_void() 