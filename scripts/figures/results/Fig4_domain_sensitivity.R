# load packages ----------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(shadowtext)
library(scales)
#-------------------------------------------------------------------------------

df <- readRDS("output_tables/abacus_high_low_leakage_projects_r1_local_biome_country_scale_prj_yield_SCC_3_USD.rds")

# Compute means
medians <- df %>%
  # focus on carbon optimization
  mutate(name_biome= if_else(name_biome == "Amazônia","Amazon","Atlantic Forest"),
         yield_carbon_ratio =  if_else(yield_carbon_ratio=="max","high","low"),
         name_biome = factor(name_biome,levels = c("Atlantic Forest","Amazon"))) %>%
  filter(mitigation_strategy =="optimize_carbon",
         CO2_cost_COC_USD>0,
         ) %>%
  group_by(leakage_scale, yield_carbon_ratio,name_biome) %>%
  summarise(median_cost = median(CO2_cost_COC_USD))

y_label = expression("Cost per tonne of CO"[2]*"e (USD)")

# adjusting label = cluster  to local

df <- df %>%
  mutate(leakage_scale = if_else(leakage_scale=="cluster","local",leakage_scale))

medians <- medians %>%
  mutate(leakage_scale = if_else(leakage_scale=="cluster","local",leakage_scale))




# adjusting the order
df$yield_carbon_ratio <- factor(df$yield_carbon_ratio,levels = c("max","min"))
df$leakage_scale <- factor(df$leakage_scale,levels = c("ignoring","local","biome","country"))


cost_scale <- df %>%
  mutate(name_biome= if_else(name_biome == "Amazônia","Amazon","Atlantic Forest"),
         yield_carbon_ratio =  if_else(yield_carbon_ratio=="max","high","low"),
         name_biome = factor(name_biome,levels = c("Atlantic Forest","Amazon"))) %>%
  filter(mitigation_strategy =="optimize_carbon",
         CO2_cost_COC_USD>0) %>%
  ggplot( aes(x = leakage_scale, y = CO2_cost_COC_USD, fill = yield_carbon_ratio)) +
    geom_violin() +
    scale_fill_manual(values = c("low" = "darkgreen",
                                 "high" = "lightgreen"),
                      name = "Yield/carbon ratio",
                      #labels = c("min" = "low",
                      #           "max" = "high")
                      ) +
  scale_y_continuous(trans = "log10",labels = comma)+
  shadowtext::geom_shadowtext(data = medians,
            aes(x = leakage_scale, 
                y = median_cost, 
                label = round(median_cost, 0)),
            inherit.aes = FALSE,
            #vjust = -2.5,
            size = 2.5,
            bg.colour = "white",
            bg.r = 0.2 ,
            colour = "black") +  
  facet_grid(name_biome~yield_carbon_ratio,scales = "free") +
    theme_classic() +
    labs(x = "Spatial Scale", y = y_label,
         title = "",
         subtitle = "") +
  theme(
    legend.position = "none",
    text = element_text(size = 7),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    # ✅ remove facet label rectangles
    strip.background = element_blank()
      )

cost_scale

ggsave(filename = "figures/Fig4.png",
       cost_scale,
       width = 14,
       height = 11,
       units = "cm")
