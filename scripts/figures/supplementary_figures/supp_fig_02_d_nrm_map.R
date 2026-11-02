# load packages ----------------------------------------------------------------

library(terra)
library(dplyr)
library(ggpubr)
library(viridis)
library(geobr)
library(sf)
#-------------------------------------------------------------------------------

r <- rast("variables/prob_nat_regeneration_all_vegetation_albers1km_pj.tif")

r_df <- as.data.frame(r,xy=T) %>%
  rename_at(vars(3), ~ "pnr") %>%
  filter(!is.na(pnr))

br <- read_country() %>%
  st_transform(crs(r))


# explode to delete islands

br <- st_make_valid(br) %>%
  st_cast("POLYGON")%>%
  mutate(area = (st_area(.)/10^6))%>%
  #filter only mainland
  slice_max(area, n = 1)



# Crop or clip shapefile to raster extent
br_cropped <- st_crop(br, ext(r))

# Convert raster extent to an sf polygon
r_ext <- as.polygons(ext(r), crs = crs(r)) %>% st_as_sf()

# Keep only features that intersect with the raster extent
br_mainland <- st_intersection(br, r_ext)


bbox <- st_bbox(r_ext)

r_df <- r_df %>%
  mutate(pnr_quantile = cut_number(pnr, n = 10))


#breaks = c(0,0.31,1,2,3,4,5,6)*320

map <- r_df %>%
  ggplot()+
  geom_sf(data = br, fill="gray",color ="black") +
  geom_raster(aes(x = x, y= y, fill = pnr))+
  scale_fill_viridis_c(
    name = "Probability of \n natural regeneration",
    #breaks = breaks
    #trans = "log"
  ) + 
  # coord_sf(
  #   xlim = c(bbox["xmin"], bbox["xmax"]),
  #   ylim = c(bbox["ymin"], bbox["ymax"]),
  #   expand = FALSE,
  #   crs = st_crs(r)
  #) +
  theme_void() +
  theme(text = element_text(size = 7))

map

map_quant <- ggplot() +
  geom_sf(data = br, fill = "gray", color = "black") +
  geom_raster(data = r_df, aes(x = x, y = y, fill = pnr_quantile)) +
  scale_fill_viridis_d(
    name = "Quantile of \n natural regeneration",
    option = "viridis"
  ) +
  theme_void() +
  theme(text = element_text(size = 7))

map_quant


ggsave("figures/supp/manuscript/pnr_map.png",
       map,
       width = 16,
       height = 16,
       units = "cm")
