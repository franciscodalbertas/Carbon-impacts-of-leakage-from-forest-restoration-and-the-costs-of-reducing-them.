# load packages ----------------------------------------------------------------

library(terra)
library(sf)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(dplyr)
library(scales)
library(ggridges)

#-------------------------------------------------------------------------------

# values for all cells withing the cluster!! For biome and country scales,
# this is just a sample of the actual heterogeneity (as theres more single pixels
# available!)

# and the country scale is not right as there is no value outside the amazon or the 
# atlantic forest!!

# I need the full distribution of y_c ratio across pasture pixels!!
# but then, for local scale, it only makes sense to consider the 
# immediate regions withing the AF and AM!


# # load yield data
yield_w <- rast("variables/beef_yield_kg_ha_2021_albers_1km.tif")
yield_nw <- rast("variables/beef_yield_kg_ha_2021_no_weights_albers_1km.tif")

plot(yield_w)
plot(yield_nw)

# opening additional carbon
carbon <- rast("variables/delta_AGC_BGC_SOC_Walter_22_albers_50y_1km.tif")

# opening pasture

pasture <- rast("variables/pasture_coverage_lapig_2021_col8albers_pj_1000mres.tif")

# # Combine rasters
rasters <- c(yield_w,yield_nw, carbon)
names(rasters) <- c("yield_w","yield_nw", "carbon")



# # Convert your data frame to a SpatVector
points <- vect(as.data.frame(yield_w,xy=T), geom = c("x", "y"), crs = crs(rasters))

# Extract raster values at point locations
values_df <- terra::extract(rasters, points, bind=TRUE)

# this is across the entire country!
# now I need it to get it for immediate regions within the biomes of interest!

local <- readRDS("input_tables/imediate_regions_am_af.rds")


# join info on immediate regions

values_df_immediate <- st_join(st_as_sf(values_df),local)

values_df_immediate <- st_drop_geometry(values_df_immediate)

# clear NAs
# this is just for immediate regions withing my biomes of interest!!
values_df_immediate <- values_df_immediate %>%
  filter(!is.na(code_immediate ))


# this is the entire distribution
values_df_country <- as.data.frame(values_df)

# calculate y_c_ratio
values_df_immediate <- values_df_immediate %>%
  mutate(y_c_ratio_w = yield_w/carbon,
         y_c_ratio_nw = yield_nw/carbon)%>%
  # get rid of cells with 0 carbon add
  filter(!is.infinite(y_c_ratio_w),
         !is.na(y_c_ratio_w),
         !is.infinite(y_c_ratio_nw),
         !is.na(y_c_ratio_nw),
         y_c_ratio_w>0,
         y_c_ratio_nw>0
  )

# there is a lot of weird values for carbon, that I should have set
# to zero!


values_df_country <- values_df_country %>%
  mutate(y_c_ratio_w = yield_w/carbon,
         y_c_ratio_nw = yield_nw/carbon)%>%
  # get rid of cells with 0 carbon add
  filter(!is.infinite(y_c_ratio_w),
         !is.na(y_c_ratio_w),
         y_c_ratio_w>0,
         !is.infinite(y_c_ratio_nw),
         !is.na(y_c_ratio_nw),
         y_c_ratio_nw>0
  )



# filter out extreme values of y_c_ratio!!
quant_bounds <- values_df_immediate %>%
  group_by(name_biome) %>%
  summarize(
    q05w = quantile(y_c_ratio_w, 0.025, na.rm = TRUE),
    q95w = quantile(y_c_ratio_w, 0.975, na.rm = TRUE),
    q05nw = quantile(y_c_ratio_nw, 0.025, na.rm = TRUE),
    q95nw = quantile(y_c_ratio_nw, 0.975, na.rm = TRUE),
    .groups = "drop"
  )




# Join quantiles back and filter accordingly
values_df_immediate <- values_df_immediate %>%
  left_join(quant_bounds, by = "name_biome") %>%
  filter(
    ( y_c_ratio_w >= q05w & y_c_ratio_w <= q95w),
    ( y_c_ratio_nw >= q05nw & y_c_ratio_nw <= q95nw)
  ) %>%
  select(-q05w, -q95w,-q05nw, -q95nw)  # optional: drop helper columns

biomes <- values_df_immediate %>%
  filter(name_biome %in% c("Amazônia", "Mata Atlântica")) %>%
  mutate(name_biome = if_else(name_biome=="Mata Atlântica","AF", "AM"),
         group = paste("Biome:", name_biome)) %>%
  select(y_c_ratio_w,y_c_ratio_nw, group)


# quantile(values_df_country$y_c_ratio_w,0.95)
# quantile(values_df_country$y_c_ratio_w,0.05)
# 
# quantile(values_df_country$y_c_ratio_nw,0.95)
# quantile(values_df_country$y_c_ratio_nw,0.05)

country <- values_df_country %>%
  mutate(group = "Country") %>%
  filter(y_c_ratio_w<=12.050849&y_c_ratio_w>=0.14313749,
         y_c_ratio_nw<=12.630007&y_c_ratio_nw>=0.16627707) %>%
  select(y_c_ratio_w,y_c_ratio_nw, group) 

plot_data <- bind_rows( biomes, country)

# ordering!

library(forcats)

# rename

plot_data$group <- factor(plot_data$group,levels = c("Biome: AF",
                                                     "Biome: AM",
                                                     "Country"))


# # get mean values from projects in the amazon and af!
# 
# selected_projects <- readRDS("project_location/project_location_round8_sampled100projects.rds")
# 
# # get unique values
# selected_projects <-  selected_projects %>%
#   st_drop_geometry()%>%
#   select(yield_carbon_ratio,name_biome,mean_y_c) %>%
#   ungroup()%>%
#   distinct()
# 
# # get mean per location
# 
# means <- selected_projects %>%
#   group_by(name_biome,yield_carbon_ratio)%>%
#   summarise(mean_y_c = mean(mean_y_c))
# 
# # Define line data
# vlines_df <- data.frame(
#   xintercept = c(means %>%filter(yield_carbon_ratio=="min"&name_biome=="Amazônia")%>%pull(mean_y_c), 
#                  means %>%filter(yield_carbon_ratio=="min"&name_biome!="Amazônia")%>%pull(mean_y_c), 
#                  means %>%filter(yield_carbon_ratio=="max"&name_biome=="Amazônia")%>%pull(mean_y_c), 
#                  means %>%filter(yield_carbon_ratio=="max"&name_biome!="Amazônia")%>%pull(mean_y_c)),
#   label = c("AM", "AF", "AM", "AF"),
#   color = c("darkgreen", "darkgreen", "lightgreen", "lightgreen"),
#   linetype = c("solid", "dashed", "solid", "dashed"),  # AM = solid, AF = dashed
#   y = Inf  # To position text near the top
# )
# 
# 
# heterog_plot <- ggplot(plot_data, aes(x = y_c_ratio, y =group, fill = group)) +
#   geom_density_ridges(scale = 1.5, alpha = 0.8, rel_min_height = 0.01, color = "white") +
#   geom_vline(data = vlines_df, aes(xintercept = xintercept, color = color,linetype = label), size = 0.4, show.legend = T) +
#   geom_text(data = vlines_df, aes(x = xintercept, y = y, label = label, color = color),
#             inherit.aes = FALSE, vjust = -0.5, size = 2.5, show.legend = F) +
#   scale_color_identity() +
#   scale_fill_viridis_d(guide = "none")+
#   scale_x_continuous(breaks = seq(0,12,2),
#                      minor_breaks = seq(0, 12, 0.5))+
#   theme_classic() +
#   labs(
#     #title = "Heterogeneity in Yield-to-Carbon Ratio Across Nested Spatial Scales",
#     x = "Yield-to-Carbon Ratio (y/c)",
#     y = "",
#     fill = "Scale"
#   ) +
#   theme(
#     legend.position = c(0.98, 0.98),
#     legend.justification = c("right", "top"),
#     legend.background = element_rect(fill = "white", color = NA),
#     #legend.box.background = element_rect(color = "black"),
#         text = element_text(size = 8)
#         )+
#   guides(linetype = guide_legend(title = NULL))
#   
# 
# heterog_plot
# 
# 
# 

# now calculate the coeficient of variation for the 3 levels!

# coefficient of variation for the country

cv_country <- plot_data %>%
  filter(group == "Country")%>%
  summarise(cv_w = sd(y_c_ratio_w) / mean(y_c_ratio_w),
            cv_nw = sd(y_c_ratio_nw) / mean(y_c_ratio_nw)) %>%
  mutate(scale = "Country")

cv_biome <- plot_data %>%
  filter(group %in% c("Biome: AF","Biome: AM"))%>%
  group_by(group)%>%
  summarise(cv_w = sd(y_c_ratio_w) / mean(y_c_ratio_w),
            cv_nw = sd(y_c_ratio_nw) / mean(y_c_ratio_nw)) %>%
  mutate(scale = group )


cv_local <- values_df_immediate %>%
  group_by(code_immediate,name_biome)%>%
  summarise(cv_w = sd(y_c_ratio_w) / mean(y_c_ratio_w),
            cv_nw = sd(y_c_ratio_nw) / mean(y_c_ratio_nw)) %>%
  mutate(
    name_biome = if_else(name_biome=="Amazônia","Amazon","Atlantic Forest"),
    scale = paste0("Local: ",name_biome)
  )

combined_cv <- rbind(cv_local[,c(3:5)],cv_biome[,c(2:4)],cv_country)

combined_cv <- combined_cv%>%
  mutate(scale = gsub(pattern = "Amazon",replacement = "AM",scale),
         scale = gsub(pattern = "Atlantic Forest",replacement = "AF",scale))




# pivot_longer

combined_cv_l <- pivot_longer(combined_cv,cols = 1:2)


# plot it as boxplot?
combined_cv_l$scale <- factor(combined_cv_l$scale,
                              levels = rev(c(
                                "Local: AF",
                                "Local: AM",
                                "Biome: AF",
                                "Biome: AM",
                                "Country")))

# cv_plot <- combined_cv_l %>%
#   filter(!is.na(scale)) %>%
#   ggplot(aes(x = scale,y = value, fill = name))+
#   #geom_jitter(aes(x = scale,y = value, colour = name)) +
#   geom_boxplot(outliers = F)+
#   ylab("cv on y/c")+
#   xlab("scale for leakage and mitigation") +
#   theme_classic()+
#   theme(text = element_text(size= 8,
#                             angle = 0))


summary_df <- combined_cv_l %>%
  filter(!is.na(scale)) %>%
  group_by(scale, name) %>%
  summarise(
    med  = median(value, na.rm = TRUE),
    low  = if (n() > 1) quantile(value, 0.025, na.rm = TRUE) else NA,
    high = if (n() > 1) quantile(value, 0.975, na.rm = TRUE) else NA,
    .groups = "drop"
  )

cv_plot <- ggplot(summary_df, aes(x = scale, y = med, colour = name)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(
    aes(ymin = low, ymax = high),
    position = position_dodge(width = 0.5),
    width = 0.2,
    na.rm = TRUE   # <- ignores missing intervals
  ) +
  ylab("Coefficient of variation of \n forgone-production/carbon-lost ratio") +
  xlab("scale for leakage and mitigation") +
  scale_colour_manual(
    name = "sr distribution",  # <- change legend title here
    values = c("cv_w" = "black", "cv_nw" = "gray"), # custom colours
    labels = c("cv_w" = "weighted", 
               "cv_nw" = "non-weighted"
    ) )+
  theme_classic() +
  theme(text = element_text(size = 8))


cv_plot

ggsave(plot = cv_plot,
       "figures/supp_figures/supp_fig_11.png",
       height = 10,
       width = 14,
       units = "cm",
       bg = "white")