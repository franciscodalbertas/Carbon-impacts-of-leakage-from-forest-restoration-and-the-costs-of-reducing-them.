# load packages ----------------------------------------------------------------

library(geobr)
library(sf)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(cowplot)
library(ggspatial)
library(ggrepel)
library(shadowtext)
#-------------------------------------------------------------------------------

mun <- read_municipality(year = 2020)
rgi <-readRDS("input_tables/imediate_regions_am_af.rds")
biomes <- readRDS("input_tables/am_af.rds")

# intersect biomes with mun
mun_pj <- st_transform(mun,st_crs(biomes))
mun_pj <- st_make_valid(mun_pj)
mun_biomes <- st_intersection(biomes,mun_pj)

Br <- read_country() %>%
  st_transform(st_crs(mun_pj))

# Get label coordinates for each biome
biome_labels <- biomes %>%
  st_point_on_surface() %>%   # safer than centroid, guarantees point inside polygon
  cbind(st_coordinates(.)) %>%
  mutate(name_biome = if_else(name_biome == "Amazônia","Amazon","Atlantic Forest"))

main <- ggplot() +
  geom_sf(data = Br, fill = "white", color = "black", size = 0.5) +
  geom_sf(data = mun_biomes, fill = "lightgray", color = "white", size = 0.5) +
  geom_sf(data = rgi, fill = "transparent", color = "black", size = 0.5) +
  geom_sf(data = biomes, fill = "transparent", color = "black", size = 0.5) +
  geom_shadowtext(
    data = biome_labels,
    aes(x = X, y = Y, label = name_biome),  # replace biome_name with your column
    colour   = "black", 
    bg.colour = "white", 
    bg.r     = 0.2,
    size     = 4
  ) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  theme_void()


main

ggsave(plot = main,filename = "figures/supp_figures/supp_fig_2F.png",
       width = 12,
       height = 12,units = "cm"
       )
