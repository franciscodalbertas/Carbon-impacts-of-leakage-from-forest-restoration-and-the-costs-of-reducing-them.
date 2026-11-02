# load packages ----------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(scales)
library(shadowtext)

#-------------------------------------------------------------------------------

#df <- readRDS("output_tables/aggregated_results_sample_projects_round9_local_biome_country_scale.rds")

df <- readRDS("output_tables/stefan_results//abacus_high_low_leakage_projects_r12_local_biome_country_scale_prj_yield_SCC_3_USD.rds")

# adjusting the order
df$yield_carbon_ratio <- factor(df$yield_carbon_ratio,levels = c("max","min"))
df$leakage_scale <- factor(df$leakage_scale,levels = c("ignoring","cluster","biome","country"))



# adjust labels
#biome <- biome %>% mutate(mitigation_strategy = if_else(mitigation_strategy =="minimize_costs","minimize costs","optmize for carbon"))

#unique(biome$mitigation_strategy)

# Compute means of verra vs abacus
medians <- df %>%
  # focus on carbon optimization
  mutate(name_biome= if_else(name_biome == "Amazônia","Amazon","Atlantic Forest"),
         yield_carbon_ratio =  if_else(yield_carbon_ratio=="max","high","low"),
         co2_ha = final_CO2_balance_non_corrected/total_area_restored) %>%
  filter(
    CO2_cost_COC_USD>0,
    mitigation_strategy %in% c("optimize_carbon","no_mitigation")
  ) %>%
  group_by(name_biome, mitigation_strategy, yield_carbon_ratio) %>%
  summarise(co2_ha = median(co2_ha))


y_label = expression("Tonnes CO"[2]*"e ha"^-1)

co2_balance <- df %>%
   # focus on carbon optimization
  mutate(name_biome= if_else(name_biome == "Amazônia","Amazon","Atlantic Forest"),
         name_biome = factor(name_biome,levels = c("Atlantic Forest","Amazon")),
         yield_carbon_ratio =  if_else(yield_carbon_ratio=="max","high","low"),
         co2_ha = final_CO2_balance_non_corrected/total_area_restored) %>%
  filter(
    CO2_cost_COC_USD>0,
    mitigation_strategy %in% c("optimize_carbon","no_mitigation")
  ) %>%
  ggplot( aes(x = mitigation_strategy, y = co2_ha, fill = yield_carbon_ratio)) +
  geom_violin() +
  # geom_point(data = medians, aes(x = mitigation_strategy, y = co2_ha, group = yield_carbon_ratio), 
  #            size = 2, color = "red") +
  # geom_line(data = medians, aes(x = mitigation_strategy, y = co2_ha, group = yield_carbon_ratio), 
  #           size = 0.5, color = "red") +
  scale_x_discrete(labels = c("no mitigation","optmize for carbon")) +
  scale_fill_manual(values = c("low" = "darkgreen",
                               "high" = "lightgreen"),
                    name = "Yield/carbon ratio",
                    #labels = c("min" = "low",
                    #           "max" = "high")
  ) +
  shadowtext::geom_shadowtext(data = medians%>% mutate(name_biome = factor(name_biome,levels = c("Atlantic Forest","Amazon"))),
            aes(x = mitigation_strategy, 
                y = co2_ha, 
                label = round(co2_ha, 0)),
            inherit.aes = FALSE,
            #vjust = -2.5,
            size = 2.5,
            bg.colour = "white",
            r.g = 0.2,
            colour = "black") +  
  facet_grid(name_biome~yield_carbon_ratio,scales ="free") +
  theme_classic() +
  labs(x = "", y = y_label,
       title = "",
       subtitle = "") +
  theme(
    # ✅ remove facet label rectangles
    strip.background = element_blank(),
    legend.position = "none",
    text = element_text(size = 7),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
  )


co2_balance

ggsave(filename = "figures/results/manuscript/stefan_data/updated_COC_PNR/CO2_balance_updated.png",
       co2_balance,
       width = 14,
       height = 11,
       units = "cm")

