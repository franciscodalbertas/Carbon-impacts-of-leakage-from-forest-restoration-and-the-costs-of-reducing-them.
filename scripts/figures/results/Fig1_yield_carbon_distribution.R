# load packages ----------------------------------------------------------------

library(terra)
library(dplyr)
library(sf)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(scales)
library(patchwork)

#-------------------------------------------------------------------------------

# this is how to generate the df for ploting (only need to do it once) ---------

# get cluster data

ma <- readRDS("input_tables/clustered_pixels_MA_full_distribution.RDS")
# # AM
am <- readRDS("input_tables/clustered_pixels_AM_full_distribution.RDS")
# 
# #Split and unnest
coords_ma <- ma %>%
  mutate(coord_pairs = strsplit(coordinates, ";")) %>%
  unnest(coord_pairs) %>%
  separate(coord_pairs, into = c("x", "y"), sep = ",", convert = TRUE)
# 
coords_am <- am %>%
  mutate(coord_pairs = strsplit(coordinates, ";")) %>%
  unnest(coord_pairs) %>%
  separate(coord_pairs, into = c("x", "y"), sep = ",", convert = TRUE)
# 
# 
# # add biome data
# 
coords_am <- coords_am %>% mutate(name_biome = "Amazon")
coords_ma <- coords_ma %>% mutate(name_biome = "Atlantic Forest")
# 
# # combine the dataframes
# 
df <- rbind(coords_am,coords_ma)
# 
# 
# # load yield data
yield <- rast("variables/beef_yield_kg_ha_2021_albers_1km.tif")
# 
# # opening additional carbon
carbon <- rast("variables/delta_AGC_BGC_SOC_Walter_22_albers_50y_1km.tif")
# 

# area restorable

pasture_area <- rast("variables/pasture_coverage_lapig_2021_col8albers_pj_1000mres.tif")

# # Combine rasters
rasters <- c(yield, carbon,pasture_area)
names(rasters) <- c("yield", "carbon","restorable_area")
# 
# # Convert your data frame to a SpatVector
points <- vect(df, geom = c("x", "y"), crs = crs(rasters))
# 
# # Extract raster values at point locations
values_df <- terra::extract(rasters, points)
# 
# # Join the extracted values back to original df
df_with_values <- bind_cols(df, values_df[, -1])  # Remove ID column from extract()
# 
# # save so I dont need to wait so much time to generate it again!
# 
saveRDS(df_with_values,"input_tables/yield_carbon_values_all_cluster_cells.rds")

#-------------------------------------------------------------------------------

# load df again

df_with_values <- readRDS("input_tables/yield_carbon_values_all_cluster_cells.rds")

# aggregate by project

aggregated_values <- df_with_values %>%
  mutate(
    restorable_area = 100*restorable_area,
    # production
    production = yield * restorable_area,
    total_carbon = carbon * restorable_area
  )%>%
  group_by(coordinates,name_biome)%>%
  summarise(production = sum(production),
            total_carbon = sum(total_carbon),
            restorable_area = sum(restorable_area))%>%
  mutate(yield = production/restorable_area,
         carbon = total_carbon/restorable_area)%>%
  select(coordinates,yield,carbon,name_biome)

# calculate y_c ratio
aggregated_values <- aggregated_values %>%
  mutate(
    # transform carbon in CO2
    carbon = carbon*44/12,
    y_c_ratio = yield/carbon,
  )%>%
  # get rid of cells with 0 carbon add
  filter(!is.infinite(y_c_ratio))

# pivot to longer format
df_l <- aggregated_values %>%
  pivot_longer(cols =c(yield,carbon,y_c_ratio))

# Define the order
name_levels <- c("yield", "carbon","y_c_ratio")

df_l$name <- factor(df_l$name,levels = name_levels)

# Step 1: Calculate mean and median per group (transform in CO2)
summ_stats <- df_l %>%
  group_by(name_biome, name) %>%
  summarize(
    mean_value = round(mean(value, na.rm = TRUE),2),
    median_value = round(median(value, na.rm = TRUE),2),
    y_max = 300*10^3,
    .groups = "drop"
  )

summ_stats$name <- factor(summ_stats$name, levels = name_levels)


# Define x positions per variable (name)
x_positions <- tibble(
  name = factor(c("yield", "carbon", "y_c_ratio"), levels = c("yield", "carbon", "y_c_ratio")),
  x_annot = c(200, 600, 3)
)

# Define y positions per biome
y_positions <- tibble(
  name_biome = c("Amazon", "Atlantic Forest"),
  y_annot_top = c(20000, 20000),
  y_annot_bottom = c(20000 * 0.94, 20000 * 0.94)
)

summ_stats <- summ_stats %>%
  left_join(x_positions, by = "name") %>%
  left_join(y_positions, by = "name_biome")

# For the y-axis facet (name_biome)
biome_labels <- c(
  "Amazônia" = "Amazon",
  "Mata Atlântica" = "Atlantic Forest"
)

# For the x-axis facet (name)
name_labels <- c(
  "y_c_ratio" = "forgone-production-to-carbon-gained"
)


# filter out extreme values of y_c_ratio!!
quant_bounds <- df_l %>%
  filter(name == "y_c_ratio") %>%
  group_by(name_biome) %>%
  summarize(
    q05 = quantile(value, 0.025, na.rm = TRUE),
    q95 = quantile(value, 0.975, na.rm = TRUE),
    .groups = "drop"
  )


df_l_filtered <- df_l %>%
  left_join(quant_bounds, by = "name_biome") %>%
  group_by(coordinates) %>%
  mutate(flag_out = any(name == "y_c_ratio" & (value < q05 | value > q95))) %>%
  ungroup() %>%
  filter(!flag_out) %>%
  select(-q05, -q95, -flag_out)


df_l_filtered$name_biome <- factor(df_l_filtered$name_biome,levels=c("Atlantic Forest","Amazon")) 

biome_order <- c("Atlantic Forest", "Amazon")

library(forcats)

df_base <- df_l_filtered %>%
  filter(name %in% c("yield", "carbon")) 

biome_labels_ok <- setNames(names(biome_labels), unname(biome_labels))

# 2) A helper function to build one "column" plot
make_hist_col <- function(metric, xlim_vec) {
  
  ggplot(
    data = df_base %>% filter(name == metric),
    aes(x = value)
  ) +
    # geom_rect(
    #   data = quant_bands_min %>% filter(name == metric),
    #   aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max),
    #   fill = "darkgreen", alpha = 0.2, inherit.aes = FALSE
    # ) +
    # geom_rect(
    #   data = quant_bands_max %>% filter(name == metric),
    #   aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max),
    #   fill = "lightgreen", alpha = 0.2, inherit.aes = FALSE
    # ) +
    geom_histogram(fill = "blue", alpha = 0.25, color = "transparent", bins = 20) +
    
    geom_vline(
      data = summ_stats %>%
        filter(name == metric),
      aes(xintercept = mean_value),
      color = "black", linetype = "solid", linewidth = 0.3
    ) +
    geom_vline(
      data = summ_stats %>%
        filter(name == metric),
      aes(xintercept = median_value),
      color = "darkgray", linetype = "dashed", linewidth = 0.3
    ) +
    
    geom_text(
      data = summ_stats %>% filter(name == metric),
      aes(x = x_annot, y = y_annot_top,
          label = paste0("Mean: ", round(mean_value, 2))),
      color = "black", hjust = 0, vjust = 1, size = 2, inherit.aes = FALSE
    ) +
    geom_text(
      data = summ_stats %>% filter(name == metric),
      aes(x = x_annot, y = y_annot_bottom,
          label = paste0("Median: ", round(median_value, 2))),
      color = "darkgray", hjust = 0, vjust = 1, size = 2, inherit.aes = FALSE
    ) +
    
    scale_y_continuous(labels = comma) +
    
    facet_grid(
      rows = vars(fct_relevel(name_biome, biome_order)),
      cols = vars(name),
      labeller = labeller(
        name_biome = as_labeller(biome_labels_ok),
        name       = as_labeller(name_labels)
      ),
      scales = "free_y"  # only y varies by row; x is fixed by coord_cartesian below
    ) +
    
    coord_cartesian(xlim = xlim_vec) +
    
    #xlab("") + 
    ylab("") +
    theme_classic() +
    theme(
      text = element_text(size = 7),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      # optional: remove repeated strip on top if you prefer
      strip.background = element_blank()
    )
}

# 3) Build left and right columns
p_yield  <- make_hist_col("yield",  c(0, 250))
p_carbon <- make_hist_col("carbon", c(0, 700)) 



theme_left_col <- theme(
  # keep y axis
  axis.title.y = element_text(),
  axis.text.y  = element_text(),
  axis.ticks.y = element_line(),
  
  # 🔑 explicitly keep x-axis title
  axis.title.x = element_text(),
  axis.text.x  = element_text(),
  axis.ticks.x = element_line(),
  
  # 🔑 remove biome facet labels on the left
  strip.text.y = element_blank(),
  strip.background.y = element_blank()
)

theme_right_col <- theme(
  axis.title.y = element_blank(),
  axis.text.y  = element_blank(),
  axis.ticks.y = element_blank()
)


p_yield  <- make_hist_col("yield",  c(0, 250)) + theme_left_col+xlab(expression(kg~ha^{-1}~yr^{-1}))
p_carbon <- make_hist_col("carbon", c(0, 750)) + theme_right_col+xlab(expression(tCO[2]*e~ha^{-1}))

# 4) Combine side-by-side: need to fix the labelling!
hist_panel_1 <- p_yield | p_carbon

# compute quantiles
q_vals <- df_l_filtered %>%
  filter(name == "y_c_ratio") %>%
  group_by(name_biome, name) %>%
  summarise(
    q20 = quantile(value, 0.20, na.rm = TRUE),
    q80 = quantile(value, 0.80, na.rm = TRUE),
    .groups = "drop"
  )


hist_panel_2 <- df_l_filtered %>%
  filter(name =="y_c_ratio") %>%
  ggplot(aes(x = value)) +
  # quantile treshold!!
  # geom_rect(data = quant_band,
  #           aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max),
  #           fill = "lightgoldenrod1", alpha = 0.2, inherit.aes = FALSE) +
  # geom_rect(data = quant_bands_min%>%
  #             filter(name =="y_c_ratio"),
  #           aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max),
  #           fill = "darkgreen", alpha = 0.2, inherit.aes = FALSE) +
  # geom_rect(data = quant_bands_max%>%
  #             filter(name =="y_c_ratio"),
  #           aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max),
  #           fill = "lightgreen", alpha = 0.2, inherit.aes = FALSE) +
  
  geom_histogram(fill = "blue", alpha = 0.25, color = "transparent",bins = 20) +
  
  # Vertical lines
  geom_vline(data = summ_stats%>%
               filter(name =="y_c_ratio"), aes(xintercept = mean_value), 
             color = "black", linetype = "solid", linewidth = 0.3) +
  geom_vline(data = summ_stats%>%
               filter(name =="y_c_ratio"), aes(xintercept = median_value), 
             color = "darkgray", linetype = "dashed", linewidth = 0.3) +
  geom_vline(
    data = q_vals,
    aes(xintercept = q20),
    #linetype = "dashed",
    linewidth = 0.8,   # increase this
    colour = "darkgreen"
  ) +
  geom_vline(
    data = q_vals,
    aes(xintercept = q80),
    #linetype = "dashed",
    linewidth = 0.8,   # increase this
    colour = "lightgreen"
  ) +
  # Text annotations
  # geom_text(data = summ_stats, aes(x = 0.001, y = y_max, 
  #                                  label = paste0("Mean: ", round(mean_value, 2))),
  #           color = "black", hjust = 0, vjust = 1, size = 2) +
  # geom_text(data = summ_stats, aes(x = 0.001, y = y_max * 0.94, 
  #                                  label = paste0("Median: ", round(median_value, 2))),
  #           color = "darkgray", hjust = 0, vjust = 1, size = 2) +
  geom_text(data = summ_stats%>%
              filter(name =="y_c_ratio"), aes(x = x_annot, y = y_annot_top, 
                                              label = paste0("Mean: ", round(mean_value, 2))),
            color = "black", hjust = 0, vjust = 1, size = 2) +
  geom_text(data = summ_stats%>%
              filter(name =="y_c_ratio"), aes(x = x_annot, y = y_annot_bottom, 
                                              label = paste0("Median: ", round(median_value, 2))),
            color = "darkgray", hjust = 0, vjust = 1, size = 2)+
  
  #geom_vline(data = quant_lines, aes(xintercept = q20), 
  #           color = "purple", linetype = "dotted", linewidth = 0.4) +
  #geom_vline(data = quant_lines, aes(xintercept = q80), 
  #           color = "purple", linetype = "dotted", linewidth = 0.4) +
  #scale_x_continuous(trans = "log10", labels = comma) +
  scale_y_continuous(labels = comma) +
  # facet_grid(name_biome ~ name, labeller = labeller(
  #   name_biome = as_labeller(biome_labels),
  #   name = as_labeller(name_labels,)
  # ),scales = "free") +
  facet_grid(
    rows = vars(fct_relevel(name_biome, biome_order)),
    cols  = vars(name),
    labeller = labeller(
      name_biome = as_labeller(biome_labels),
      name       = as_labeller(name_labels)
    ),
    scales = "free"
  )+
  xlab("") +
  ylab("") +
  theme_classic() +
  theme(
    text = element_text(size = 7),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )


hist_panel_2 <- hist_panel_2 +
  theme_classic() +
  theme(
    text = element_text(size = 7),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_blank(),
    strip.placement = "outside"
  )


hist_panel_2 <- hist_panel_2 + xlab(expression(kg~yr^{-1}~t~CO[2]))

final <- ggarrange(hist_panel_1,hist_panel_2,nrow = 2)

final

# ggsave(
#   "figures/results/manuscript/distribution_y_cv04.png",
#   final,
#   height = 15,
#   width = 14,
#   units = "cm",
#   bg = "white")


ggsave(
  "figures/Fig1.svg",
  final,
  height = 15,
  width = 14,
  units = "cm",
  bg = "white")
