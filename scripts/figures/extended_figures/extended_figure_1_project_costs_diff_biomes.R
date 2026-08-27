# load packages ----------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)

#-------------------------------------------------------------------------------

df <- readRDS("output_tables/abacus_verra_random_projects_local_biome_country_scale_prj_yield_SCC_3_USD.rds")


medians <- df %>%
  # focus on carbon optimization
  mutate(name_biome= if_else(name_biome == "Amazônia","Amazon","Atlantic Forest"),
         #yield_carbon_ratio =  if_else(yield_carbon_ratio=="max","high","low")
  ) %>%
  filter(mitigation_strategy =="no_mitigation",
         CO2_cost>0,
         leakage_scale != "ignoring"
  ) %>%
  # get cost data
  pivot_longer(c(total_land_value_USD_2021,
                 total_restoration_costs_USD_2021,
                 mitigation_costs_USD_50y_2021)) %>%
  mutate(value = if_else(name =="total_land_value_USD_2021",value/total_area,
                         value/total_area_restored)) %>%
  group_by(yield_carbon_ratio,name_biome,name) %>%
  summarise(median_values = median(value))

y_label = expression("Project costs (USD ha"^-1*")") 

# adjusting the order
df$yield_carbon_ratio <- factor(df$yield_carbon_ratio,levels = c("max","min"))
#df$leakage_scale <- factor(df$leakage_scale,levels = c("ignoring","local","biome","country"))

cost_layers <- df %>%
  mutate(
    name_biome= if_else(name_biome == "Amazônia","Amazon","Atlantic Forest"),
    #yield_carbon_ratio =  if_else(yield_carbon_ratio=="max","high","low")
  ) %>%
  filter(mitigation_strategy =="no_mitigation",
         CO2_cost>0) %>%
  # get cost data
  pivot_longer(c(total_land_value_USD_2021,
                 total_restoration_costs_USD_2021,
                 mitigation_costs_USD_50y_2021)) %>%
  mutate(value = if_else(name =="total_land_value_USD_2021",value/total_area,
                         value/total_area_restored)) %>%
  filter(name!= "mitigation_costs_USD_50y_2021") %>%
  ggplot( aes(x = name, y = value, fill = name_biome)) +
  geom_violin() +
  scale_fill_manual(values = c("Amazon" = "white",
                               "Atlantic Forest" = "darkgray"),
                    name = "",
                    # labels = c("min" = "low",
                    #            "max" = "high")
  ) +
  scale_x_discrete(labels = c("land value","restoration costs")) +
  shadowtext::geom_shadowtext(data = medians%>%filter(name!="mitigation_costs_USD_50y_2021"),
                              aes(x = name, 
                                  y = median_values, 
                                  label = round(median_values, 0),
                                  group = name_biome),
                              inherit.aes = FALSE,
                              #vjust = -2.5,
                              size = 2.5,
                              bg.colour = "white",
                              colour = "black",
                              position = position_dodge(width = 0.9)) +  
  theme_classic() +
  labs(x = "", y = y_label,
       title = "",
       subtitle = "") +
  theme(
    legend.position = "top",
    text = element_text(size = 7),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
  )

cost_layers

ggsave(filename = "figures/extended_figures/extended_figure_1.png",
       plot = cost_layers,
       width = 8,
       height = 9,
       units = "cm")
