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


# # load yield data
yield <- rast("variables/beef_yield_kg_ha_2021_albers_1km.tif")
 
# opening additional carbon
carbon <- rast("variables/delta_AGC_BGC_SOC_Walter_22_albers_50y_1km.tif")

# opening pasture

pasture <- rast("variables/pasture_coverage_lapig_2021_col8albers_pj_1000mres.tif")

# # Combine rasters
rasters <- c(yield, carbon)
names(rasters) <- c("yield", "carbon")


 
# # Convert your data frame to a SpatVector
points <- vect(as.data.frame(yield,xy=T), geom = c("x", "y"), crs = crs(rasters))
 
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
  mutate(y_c_ratio = yield/carbon)%>%
  # get rid of cells with 0 carbon add
  filter(!is.infinite(y_c_ratio),
         !is.na(y_c_ratio),
         y_c_ratio>0
  )

values_df_country <- values_df_country %>%
  mutate(y_c_ratio = yield/carbon)%>%
  # get rid of cells with 0 carbon add
  filter(!is.infinite(y_c_ratio),
         !is.na(y_c_ratio),
         y_c_ratio>0
  )



# filter out extreme values of y_c_ratio!!
quant_bounds <- values_df_immediate %>%
  group_by(name_biome) %>%
  summarize(
    q05 = quantile(y_c_ratio, 0.025, na.rm = TRUE),
    q95 = quantile(y_c_ratio, 0.975, na.rm = TRUE),
    .groups = "drop"
  )
 

# Join quantiles back and filter accordingly
values_df_immediate <- values_df_immediate %>%
  left_join(quant_bounds, by = "name_biome") %>%
  filter(
      ( y_c_ratio >= q05 & y_c_ratio <= q95)
  ) %>%
  select(-q05, -q95)  # optional: drop helper columns

biomes <- values_df_immediate %>%
  filter(name_biome %in% c("Amazônia", "Mata Atlântica")) %>%
  mutate(name_biome = if_else(name_biome=="Mata Atlântica","AF", "AM"),
    group = paste("Biome:", name_biome)) %>%
  select(y_c_ratio, group)
    
country <- values_df_country %>%
  mutate(group = "Country") %>%
  filter(y_c_ratio<=11.85829&y_c_ratio>=0.07077003) %>%
  select(y_c_ratio, group) 

plot_data <- bind_rows( biomes, country)

# ordering!

library(forcats)

# rename

plot_data$group <- factor(plot_data$group,levels = c("Biome: AF",
                                                     "Biome: AM",
                                                     "Country"))


# get mean values from projects in the amazon and af!

selected_projects <- readRDS("project_location/project_location_round1_sampled1000projects.rds")

# get unique values
selected_projects <-  selected_projects %>%
  st_drop_geometry()%>%
  select(yield_carbon_ratio,name_biome,mean_y_c) %>%
  ungroup()%>%
  distinct()

# get mean per location

means <- selected_projects %>%
  group_by(name_biome,yield_carbon_ratio)%>%
  summarise(mean_y_c = mean(mean_y_c))

# Define line data
vlines_df <- data.frame(
  xintercept = c(means %>%filter(yield_carbon_ratio=="min"&name_biome=="Amazônia")%>%pull(mean_y_c), 
                 means %>%filter(yield_carbon_ratio=="min"&name_biome!="Amazônia")%>%pull(mean_y_c), 
                 means %>%filter(yield_carbon_ratio=="max"&name_biome=="Amazônia")%>%pull(mean_y_c), 
                 means %>%filter(yield_carbon_ratio=="max"&name_biome!="Amazônia")%>%pull(mean_y_c)),
  label = c("AM", "AF", "AM", "AF"),
  color = c("darkgreen", "darkgreen", "lightgreen", "lightgreen"),
  linetype = c("solid", "dashed", "solid", "dashed"),  # AM = solid, AF = dashed
  y = Inf  # To position text near the top
)


heterog_plot <- ggplot(plot_data, aes(x = y_c_ratio, y =group, fill = group)) +
  geom_density_ridges(scale = 1.5, alpha = 0.8, rel_min_height = 0.01, color = "white") +
  geom_vline(data = vlines_df, aes(xintercept = xintercept, color = color,linetype = label), size = 0.4, show.legend = T) +
  geom_text(data = vlines_df, aes(x = xintercept, y = y, label = label, color = color),
            inherit.aes = FALSE, vjust = -0.5, size = 2.5, show.legend = F) +
  scale_color_identity() +
  scale_fill_viridis_d(guide = "none")+
  scale_x_continuous(breaks = seq(0,12,2),
                     minor_breaks = seq(0, 12, 0.5))+
  theme_classic() +
  labs(
    #title = "Heterogeneity in Yield-to-Carbon Ratio Across Nested Spatial Scales",
    x = "Forgone-production-to-carbon-gained-ratio",
    y = "",
    fill = "Scale"
  ) +
  coord_cartesian(xlim=c(0,8))+
  theme(
    legend.position = c(0.98, 0.98),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = "white", color = NA),
    #legend.box.background = element_rect(color = "black"),
        text = element_text(size = 8)
        )+
  guides(linetype = guide_legend(title = NULL))
  

heterog_plot




# now calculate the coeficient of variation for the 3 levels!

# coefficient of variation for the country

cv_country <- plot_data %>%
  filter(group == "Country")%>%
  summarise(cv = sd(y_c_ratio) / mean(y_c_ratio)) %>%
  mutate(scale = "Country")

cv_biome <- plot_data %>%
  filter(group %in% c("Biome: AF","Biome: AM"))%>%
  group_by(group)%>%
  summarise(cv = sd(y_c_ratio) / mean(y_c_ratio)) %>%
  mutate(scale = group )


cv_local <- values_df_immediate %>%
  group_by(code_immediate,name_biome)%>%
  summarise(cv = sd(y_c_ratio) / mean(y_c_ratio)) %>%
  mutate(
    name_biome = if_else(name_biome=="Amazônia","Amazon","Atlantic Forest"),
    scale = paste0("Local: ",name_biome)
    )

combined_cv <- rbind(cv_local[,c(3,4)],cv_biome[,c(2,3)],cv_country)

combined_cv <- combined_cv%>%
  mutate(scale = gsub(pattern = "Amazon",replacement = "AM",scale),
         scale = gsub(pattern = "Atlantic Forest",replacement = "AF",scale))

# plot it as boxplot?
combined_cv$scale <- factor(combined_cv$scale,
                            levels = rev(c(
                               "Local: AF",
                               "Local: AM",
                               "Biome: AF",
                               "Biome: AM",
                               "Country")))


cv_plot <- combined_cv %>%
  filter(!is.na(scale)) %>%
  ggplot(aes(x = scale,y = cv))+
  geom_boxplot()+
  ylab("cv on forgone-production- \n carbon-gained")+
  xlab("scale for leakage and mitigation") +
  theme_classic()+
  theme(text = element_text(size= 8,
                            angle = 0))

cv_plot

# do it as a plot +CI ==========================================================
#

summary_df <- combined_cv %>%
  filter(!is.na(scale)) %>%
  group_by(scale) %>%
  summarise(
    med  = median(cv, na.rm = TRUE),
    low  = if (n() > 1) quantile(cv, 0.025, na.rm = TRUE) else NA,
    high = if (n() > 1) quantile(cv, 0.975, na.rm = TRUE) else NA,
    .groups = "drop"
  )

cv_plot_error <- ggplot(summary_df, aes(x = scale, y = med)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(
    aes(ymin = low, ymax = high),
    position = position_dodge(width = 0.5),
    width = 0.2,
    na.rm = TRUE   # <- ignores missing intervals
  ) +
  ylab("Coefficient of variation \n of forgone-production-to-carbon-gained") +
  xlab("Scale for leakage and mitigation") +
  theme_classic() +
  theme(text = element_text(size = 8))


combined_plot1 <- ggarrange(heterog_plot,
                           cv_plot,
                           nrow = 2,
                           align = "hv",
                           heights = c(2,1),
                           labels = "AUTO",
                           font.label = list(size = 9))


combined_plot2 <- ggarrange(heterog_plot,
                           cv_plot_error,
                           nrow = 2,
                           align = "hv",
                           heights = c(2,1),
                           labels = "AUTO",
                           font.label = list(size = 9))




ggsave(plot = combined_plot2,
       "figures/extended_figures/extended_figure_4.png",
       height = 14,
       width = 13,
       units = "cm",
       bg = "white")
