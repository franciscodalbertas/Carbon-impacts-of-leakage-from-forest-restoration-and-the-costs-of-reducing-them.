# load packages ----------------------------------------------------------------
library(scales)
library(terra)
library(dplyr)
library(ggplot2)
library(scales)
library(sf)
library(geobr)
#-------------------------------------------------------------------------------

# load rasters
# current <- rast("variables/AGC_BGC_SOC_current_Walter_22_layer_albers_1km_res.tif")
# 
# potential <- rast("variables/AGC_BGC_SOC_potential_Walter_22_layer_albers_1km_res.tif")

delta <- rast("variables/delta_AGC_BGC_SOC_Walter_22_albers_50y_1km.tif")

# current_df <- as.data.frame(current,xy=T) %>%
#   rename_at(vars(3), ~ "carbon") 
# potential_df <- as.data.frame(potential,xy=T) %>%
#   rename_at(vars(3), ~ "carbon") 
delta_df <- as.data.frame(delta,xy=T)%>%
  rename_at(vars(3), ~ "carbon") 


# Br shapefile
br <- read_country(year = 2020, simplified = TRUE)%>%
  st_transform(crs(delta))

# filtering mainland only
br <- st_make_valid(br) %>%
  st_cast("POLYGON")%>%
  mutate(area = (st_area(.)/10^6))%>%
  #filter only mainland
  slice_max(area, n = 1)


# Define intervals

breaks <- c(0 ,seq(50,350,50) ,Inf)
labels <- (c("<50","50-100","100-150","150-200","200-250","250-300","300-350",">350"))
#current_df$category <- factor(cut(current_df$carbon, breaks = breaks, labels = labels, include.lowest = TRUE, right = FALSE), levels = (labels))

# Generate colors using viridis
#category_colors <- rev(viridis(length(labels)))
#max(vtn_df$predicted_VTN_MA_noxy,na.rm=T)

# current_map <- current_df %>%
#   filter(carbon>0)%>%
#   ggplot()+
#   geom_raster(aes(x = x, y= y, fill = category))+
#   # scale_fill_viridis_d(
#   #   name = "Carbon density",
#   #   breaks = c(0,50,150,350,Inf),
#   #   trans = "log10",
#   #   labels  = comma
#   #   ) +
#   scale_fill_viridis_d(name = "carbon density")+
#   #scale_fill_manual(values = category_colors)+
#   ggtitle("") +
#   theme_void() +
#   theme(text = element_text(size = 7))
# 
# current_map
# 
# potential_df$category <- factor(cut(potential_df$carbon, breaks = breaks, labels = labels, include.lowest = TRUE, right = FALSE), levels = (labels))
# 
# potential_map <- potential_df %>%
#   filter(carbon>0)%>%
#   ggplot()+
#   geom_raster(aes(x = x, y= y, fill = category))+
#   # scale_fill_viridis_c(
#   #   name = "Carbon tons/ha",
#   #   breaks = c(0,1,10,35,100,350,Inf),
#   #   trans = "log10", 
#   #   labels  = comma 
#   # ) +
#   scale_fill_viridis_d(name = "carbon density")+
#   #scale_fill_manual(values = category_colors)+
#   ggtitle("") +
#   theme_void()+
#   theme(text = element_text(size = 7))
# 
# potential_map

delta_df$category <- factor(cut(delta_df$carbon, breaks =c(-11,0,10,20,40,80,160,Inf),
                                labels = c("<0","0-10","10-20","20-40","40-80","80-160",">160"), 
                                include.lowest = TRUE, right = T), 
                            levels = c("<0","0-10","10-20","20-40","40-80","80-160",">160"))

#unique(delta_df$category)

delta_map <- delta_df %>%
  filter(carbon>0)%>%
  ggplot()+
  geom_sf(data = br, fill="transparent",color ="black") +
  geom_raster(aes(x = x, y= y, fill = category))+
  # scale_fill_viridis_c(
  #   name = "carbon density",
  #   breaks = c(0,1,10,20,40,80,160,260),
  #   #trans = "log1p",
  #   #labels  = c("-10","0","10","20","40","80","160","260")
  # ) +
  scale_fill_viridis_d(name = "carbon density")+
  ggtitle("") +
  theme_void()+
  theme(text = element_text(size = 7))


# ggsave("figures/methods/manuscript/current_carbon.png",
#        current_map,
#        width = 16,
#        height = 16,
#        units = "cm",
#        bg="white")
# 
# 
# 
# ggsave("figures/methods/manuscript/potential_carbon.png",
#        potential_map,
#        width = 16,
#        height = 16,
#        units = "cm",
#        bg="white")

ggsave("figures/supp_figures/supp_fig_2B.png",
       delta_map,
       width = 16,
       height = 16,
       units = "cm",
       bg="white")