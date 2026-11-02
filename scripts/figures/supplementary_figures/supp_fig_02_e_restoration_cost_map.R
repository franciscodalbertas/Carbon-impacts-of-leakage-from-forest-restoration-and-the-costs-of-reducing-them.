# load packages ----------------------------------------------------------------

library(terra)
library(dplyr)
library(ggpubr)
library(viridis)
library(geobr)
library(sf)
#-------------------------------------------------------------------------------

#r <- rast("variables/restoration_costs_reais_ha_2023_tropical_forests_albers_regreen_vals_1km.tif")
r <- rast("variables/restoration_costs_reais_ha_2023_tropical_forests_albers_regreen_vals_1km_my_model.tif")

# clip to the am and af!

am_af <- read_biomes(year= 2019,simplified = T)%>%
  filter(name_biome %in% c("Amazônia", "Mata Atlântica")) %>%
  st_transform(crs(r))


r_c <- crop(r, am_af) %>%
  mask(am_af)

r_c <- mask(r_c, am_af)


# converting to USD

IPCA_2024 <- 1.1583
USD_rate_2021 = 5.3955

r <- r/IPCA_2024/USD_rate_2021


r_df <- as.data.frame(r_c,xy=T) %>%
  rename_at(vars(3), ~ "restoration_costs") %>%
  filter(!is.na(restoration_costs))

# Br shapefile
br <- read_country(year = 2020, simplified = TRUE)%>%
  st_transform(crs(r))

# filtering mainland only
br <- st_make_valid(br) %>%
  st_cast("POLYGON")%>%
  mutate(area = (st_area(.)/10^6))%>%
  #filter only mainland
  slice_max(area, n = 1)

#breaks = c(0,0.31,1,2,3,4,5,6)*320

map <- r_df %>%
  ggplot()+
  geom_sf(data = br, fill="gray",color ="black") +
  geom_raster(aes(x = x, y= y, fill = restoration_costs))+
  scale_fill_viridis_c(
    name = "Restoration costs",
    #breaks = breaks
    #trans = "log"
    ) + 
  # coord_sf(
  #   xlim = c(bbox["xmin"], bbox["xmax"]),
  #   ylim = c(bbox["ymin"], bbox["ymax"]),
  #   expand = FALSE,
  #   crs = st_crs(r)
  # ) +
  theme_void() +
  theme(text = element_text(size = 7))

map


ggsave("figures/supp_figures/supp_fig_2E.png",
       map,
       width = 16,
       height = 16,
       units = "cm")
