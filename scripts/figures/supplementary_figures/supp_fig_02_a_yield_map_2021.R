# load packages ----------------------------------------------------------------

library(terra)
library(dplyr)
library(ggpubr)
library(viridis)
library(scales)
library(geobr)
library(sf)
#-------------------------------------------------------------------------------


# yield map

yield_raster <- rast("variables/beef_yield_kg_ha_2021_albers_1km.tif")

# Br shapefile
br <- read_country(year = 2020, simplified = TRUE)%>%
  st_transform(crs(yield_raster))

# filtering mainland only
br <- st_make_valid(br) %>%
  st_cast("POLYGON")%>%
  mutate(area = (st_area(.)/10^6))%>%
  #filter only mainland
  slice_max(area, n = 1)


# need to filter out values that are too low

yield_df_org <- readRDS("input_tables/beef_yield_municipality_kg_ha_2021.rds")%>%
  filter(outlier == 0,
         yield>0)

top <- max(yield_df_org$yield)
min <- min(yield_df_org$yield)


yield_df <- as.data.frame(yield_raster,xy=T) %>%
  rename_at(vars(3), ~ "yield") %>%
  filter(!is.na(yield),
         yield>0) %>%
  # recplace values below the first quantile by it
  mutate(yield = if_else(yield<min,NA,yield),
         yield = if_else(yield>top,NA,yield))

# categorize to help vizualizing

yield_df <- yield_df %>%
  mutate(cat = cut(yield,breaks = 
                     c(0,20,30,40,50,100,200,300,400,550),
                   labels = c("<20","20-30","30-40","40-50","50-100",
                              "100-200","200-300","300-400",">400")),
         cat2 = cut(yield,breaks = c(0,10,100,550),
                    labels = c("<10","10-100",">100")))


yield_map <- yield_df %>%
  filter(!is.na(cat2)) %>%
  ggplot()+
  geom_sf(data = br, fill="gray",color ="black") +
  geom_raster(aes(x = x, y= y, fill = cat))+
  # scale_fill_viridis_c(
  #   name = "kg/ha/y",
  #   #breaks = c(-Inf,,Inf),
  #   trans = "log10", 
  #   labels  = comma ) +
  scale_fill_viridis_d(name = "Beef yield")+
  ggtitle("") +
  theme_void() +
  theme(text = element_text(size = 7))

yield_map

ggsave("figures/supp_figures/supp_fig_2A.png",
       yield_map,
       width = 16,
       height = 16,
       units = "cm",
       bg="white")
