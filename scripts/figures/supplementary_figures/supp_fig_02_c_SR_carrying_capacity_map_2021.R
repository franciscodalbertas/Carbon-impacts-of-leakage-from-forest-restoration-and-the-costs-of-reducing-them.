# load packages ----------------------------------------------------------------

library(terra)
library(dplyr)
library(ggpubr)
library(viridis)
library(scales)
library(geobr)
library(sf)
#-------------------------------------------------------------------------------

sr <- rast("variables/stocking_rate_carrying_capacity_albers_pj.tif")


# Br shapefile
br <- read_country(year = 2020, simplified = TRUE)%>%
  st_transform(crs(sr))

# filtering mainland only
br <- st_make_valid(br) %>%
  st_cast("POLYGON")%>%
  mutate(area = (st_area(.)/10^6))%>%
  #filter only mainland
  slice_max(area, n = 1)

# capping it to 90

sr <-  sr *0.9


sr_df <- as.data.frame(sr,xy=T) %>%
  rename_at(vars(3), ~ "sr")

# categorize to help vizualizing

# yield_df <- yield_df %>%
#   mutate(cat = cut(yield,breaks = 
#                      c(0,20,30,40,50,100,200,300,400,550),
#                    labels = c("<20","20-30","30-40","40-50","50-100",
#                               "100-200","200-300","300-400",">400")),
#          cat2 = cut(yield,breaks = c(0,10,100,550),
#                     labels = c("<10","10-100",">100")))


sr_map <- sr_df %>%
  # filter(!is.na(cat2)) %>%
  ggplot()+
  geom_sf(data = br, fill="transparent",color ="black") +
  geom_raster(aes(x = x, y= y, fill = sr))+
  scale_fill_viridis_c(
    name = "stocking rate",
    #breaks = c(-Inf,,Inf),
    #trans = "log10",
    #labels  = comma 
    ) +
  #scale_fill_viridis_d(name = "stocking rate")+
  ggtitle("") +
  theme_void() +
  theme(text = element_text(size = 7))

sr_map

ggsave("figures/supp_figures/supp_fig_2C.png",
       sr_map,
       width = 16,
       height = 16,
       units = "cm",
       bg="white")
