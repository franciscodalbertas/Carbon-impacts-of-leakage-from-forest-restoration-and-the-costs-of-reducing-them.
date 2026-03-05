# load packages ----------------------------------------------------------------

library(tidyr)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(scales)
library(shadowtext)

#-------------------------------------------------------------------------------


# read results 
df <- readRDS("output_tables/abacus_high_low_leakage_projects_r1_local_biome_country_scale_prj_yield_SCC_3_USD.rds")

# producing output just for ignoring to include in the text!

df_ignoring <- df %>% filter(approach == "ignoring")

# filter out ignoring
df <- df %>% filter(approach != "ignoring")

# defining order
df$approach <- factor(df$approach,levels = c( "no_mitigation",
                                               "intensification"))

# filter strategy and biome
local <- df %>% filter(leakage_scale %in% c("ignoring","cluster"),
                       mitigation_strategy !="minimize_costs")

biome <- df %>% filter(leakage_scale %in% c("ignoring","biome"),
                          mitigation_strategy !="minimize_costs")


country <- df %>% filter(leakage_scale %in% c("ignoring","country"),
                       mitigation_strategy !="minimize_costs")

y_label = expression("Cost per tonne of CO"[2]*"e (USD)") 

# mean labels
mean_labels_local <- local %>%
  mutate(name_biome= if_else(name_biome == "Amazônia","Amazon","Atlantic Forest"),
         name_biome = factor(name_biome,levels = c("Atlantic Forest","Amazon"))) %>%
  filter(CO2_cost_COC_USD > 0) %>%
  group_by(name_biome, approach, yield_carbon_ratio) %>%
  summarize(mean_cost = median(CO2_cost_COC_USD, na.rm = TRUE), .groups = "drop")


mean_labels_biome <- biome %>%
  mutate(name_biome= if_else(name_biome == "Amazônia","Amazon","Atlantic Forest"),
         name_biome = factor(name_biome,levels = c("Atlantic Forest","Amazon"))) %>%
  filter(CO2_cost_COC_USD > 0) %>%
  group_by(name_biome, approach, yield_carbon_ratio) %>%
  summarize(mean_cost = median(CO2_cost_COC_USD, na.rm = TRUE), .groups = "drop")


mean_labels_country <- country %>%
  mutate(name_biome= if_else(name_biome == "Amazônia","Amazon","Atlantic Forest"),
         name_biome = factor(name_biome,levels = c("Atlantic Forest","Amazon"))) %>%
  filter(CO2_cost_COC_USD > 0) %>%
  group_by(name_biome, approach, yield_carbon_ratio) %>%
  summarize(mean_cost = median(CO2_cost_COC_USD, na.rm = TRUE), .groups = "drop")

# add annotation for no carbon net of leakage

# local

annotation_df_local <- data.frame(
  name_biome = c("Amazon", "Atlantic Forest"),
  approach = "no_mitigation",
  x = 0.5,  # left of x-axis
  y = c(mean_labels_local$mean_cost[mean_labels_local$approach=="no_mitigation"]),     # adjust based on your y-scale
  label = "no carbon net-of-leakage"
)


# biome
annotation_df <- data.frame(
  name_biome = c("Amazon", "Atlantic Forest"),
  approach = "no_mitigation",
  x = 0.5,  # left of x-axis
  y = c(mean_labels_biome$mean_cost[mean_labels_biome$approach=="no_mitigation"]),     # adjust based on your y-scale
  label = "no carbon net-of-leakage"
)%>%
  mutate(name_biome = factor(name_biome,levels = c("Atlantic Forest","Amazon")),
         approach = factor(approach,levels=c("no_mitigation","mitigation")))

# country
annotation_df_country <- data.frame(
  name_biome = c("Atlantic Forest"),
  approach = "no_mitigation",
  x = 0.5,  # left of x-axis
  y = mean_labels_country%>%filter(name_biome=="Atlantic Forest",approach =="no_mitigation")%>%
    pull(mean_cost),     # adjust based on your y-scale
  label = "no carbon net-of-leakage"
) 
annotation_df_country <- annotation_df_country%>%
  mutate(name_biome=factor(name_biome,levels = c("Atlantic Forest","Amazon")))
# local
annotation_df_symbol_local <- annotation_df_local
annotation_df_symbol_local$label <- "×"
annotation_df_symbol_local$y <- annotation_df_symbol_local$y *2 
annotation_df_local$approach <- factor(annotation_df_local$approach,
                                 levels = levels(local$approach))
annotation_df_symbol_local$approach <- factor(annotation_df_symbol_local$approach,
                                        levels = levels(biome$approach))

# biome
annotation_df_symbol <- annotation_df
annotation_df_symbol$label <- "×"
annotation_df_symbol$y <- annotation_df$y *2 
annotation_df_symbol$approach <- factor(annotation_df_symbol$approach,
                                 levels = levels(biome$approach))

annotation_df$approach <- factor(annotation_df$approach,
                                 levels = levels(biome$approach))

# country
annotation_df_symbol_country <- annotation_df_country
annotation_df_symbol_country$label <- "×"
annotation_df_symbol_country$y <- annotation_df_symbol_country$y *2 
annotation_df_country$approach <- factor(annotation_df_country$approach,
                                       levels = levels(country$approach))
annotation_df_symbol_country$approach <- factor(annotation_df_symbol_country$approach,
                                             levels = levels(biome$approach))


plot_abacus_biome <- biome %>%
  mutate(CO2_cost_COC_USD = if_else(CO2_cost_COC_USD<=0,NA,CO2_cost_COC_USD),
         name_biome= if_else(name_biome == "Amazônia","Amazon","Atlantic Forest"),
         name_biome = factor(name_biome,levels=c("Atlantic Forest","Amazon")))%>%
  ggplot(aes(x = yield_carbon_ratio,y = CO2_cost_COC_USD,fill = yield_carbon_ratio))+
  geom_violin()+
  shadowtext::geom_shadowtext(data = mean_labels_biome,
                              aes(x = yield_carbon_ratio,
                                  y = mean_cost,
                                  label = round(mean_cost, 0)),
                              size = 2.5,
                              bg.colour = "white", # color of the halo
                              bg.r = 0.2,
                              colour = "black",    # color of the text
                              inherit.aes = FALSE) +
  scale_fill_manual(values = c("min" = "darkgreen",
                               "max" = "lightgreen"),
                    name = "Forgone-prduction/carbon-gained",
                    labels = c("min" = "low",
                               "max" = "high")) +
  scale_y_continuous(trans = "log10",labels = comma)+
  facet_grid(name_biome~approach,
             labeller = labeller(approach = c(
               "ignoring" = "Ignoring leakage", 
               "no_mitigation" = "Accounting for leakage (no mitigation)",
               "intensification" = "Accounting for leakage (with mitigation)"
             )),scales = "free")+
  xlab("")+
  ylab(y_label)+
  theme_classic()+
  theme(
    legend.position = "top",
    text = element_text(size = 7),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    # ✅ remove facet label rectangles
    strip.background = element_blank(),
  )


plot_abacus_biome

plot_abacus_biome <- plot_abacus_biome +
  ggtitle("Biome")+
  shadowtext::geom_shadowtext(data = annotation_df,
            aes(x = 0.75, 
                y = y-8.5, 
                label = label),
            # make it vertical
            angle = 90,
            hjust = -0.05, # push slightly to the right of -Inf
            vjust = 0,
            size = 2,
            colour = "darkgray",
            bg.colour = "lightgreen",
            g.r = 0.001,
            inherit.aes = FALSE)

plot_abacus_biome

plot_abacus_local<- local %>%
  mutate(CO2_cost_COC_USD = if_else(CO2_cost_COC_USD<=0,NA,CO2_cost_COC_USD),
         name_biome= if_else(name_biome == "Amazônia","Amazon","Atlantic Forest"),
         name_biome = factor(name_biome,levels = c("Atlantic Forest","Amazon")))%>%
  ggplot(aes(x = yield_carbon_ratio,y = CO2_cost_COC_USD,fill = yield_carbon_ratio))+
  geom_violin()+
  shadowtext::geom_shadowtext(data = mean_labels_local,
            aes(x = yield_carbon_ratio, 
                y = mean_cost, 
                label = round(mean_cost, 0)),
            inherit.aes = FALSE,
            vjust = 0,
            size = 2.5,
            bg.colour ="white",
            g.r = 0.2,
            colour = "black") +
  scale_fill_manual(values = c("min" = "darkgreen",
                               "max" = "lightgreen"),
                    name = "Forgone-production/carbon-gained",
                    labels = c("min" = "low",
                               "max" = "high")) +
  scale_y_continuous(trans = "log10",labels = comma)+
  facet_grid(name_biome~approach,
             labeller = labeller(approach = c(
               "ignoring" = "Ignoring leakage", 
               "no_mitigation" = "Accounting for leakage (no mitigation)",
               "intensification" = "Accounting for leakage (with mitigation)"
             )),scales = "free")+
  xlab("")+
  ylab(y_label)+
  theme_classic()+
  ggtitle("Local")+
  theme(
    legend.position = "top",
    text = element_text(size = 7),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    # ✅ remove facet label rectangles
    strip.background = element_blank(),
  )

plot_abacus_local

plot_abacus_country<- country %>%
  mutate(CO2_cost_COC_USD = if_else(CO2_cost_COC_USD<=0,NA,CO2_cost_COC_USD),
         name_biome= if_else(name_biome == "Amazônia","Amazon","Atlantic Forest"),
         name_biome = factor(name_biome,levels = c("Atlantic Forest","Amazon")))%>%
  ggplot(aes(x = yield_carbon_ratio,y = CO2_cost_COC_USD,fill = yield_carbon_ratio))+
  geom_violin()+
  shadowtext::geom_shadowtext(data = mean_labels_country,
            aes(x = yield_carbon_ratio, 
                y = mean_cost, 
                label = round(mean_cost, 0)),
            inherit.aes = FALSE,
            vjust = 0,
            size = 2.5,
            bg.colour = "white",
            g.r = 0.2,
            colour = "black") +
  scale_fill_manual(values = c("min" = "darkgreen",
                               "max" = "lightgreen"),
                    name = "Forgone-production/carbon-gained",
                    labels = c("min" = "low",
                               "max" = "high")) +
  scale_y_continuous(trans = "log10",labels = comma)+
  facet_grid(name_biome~approach,
             labeller = labeller(approach = c(
               "ignoring" = "Ignoring leakage", 
               "no_mitigation" = "Accounting for leakage (no mitigation)",
               "intensification" = "Accounting for leakage (with mitigation)"
             )),scales = "free")+
  xlab("")+
  ylab(y_label)+
  theme_classic()+
  theme(
    legend.position = "top",
    text = element_text(size = 7),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    # ✅ remove facet label rectangles
    strip.background = element_blank(),
  )

plot_abacus_country <- plot_abacus_country +
  shadowtext::geom_shadowtext(data = annotation_df_country,
            aes(x = 0.75, y = y, label = label),
            angle = 90,
            bg.colour = "lightgreen",
            colour = "darkgray",
            g.r = 0.001,
            hjust = -0.05, # push slightly to the right of -Inf
            vjust = 0,
            size = 2,
            inherit.aes = FALSE)


plot_abacus_country

# saving! now combining local and biome!!
  
local_biome <- ggarrange(plot_abacus_local,plot_abacus_biome,common.legend = T,
                         nrow = 2)  
  

# main text
ggsave(filename = "figures/Fig3.png",
       plot = plot_abacus_country,
       width = 14,
       height = 11,
       units = "cm")

# supp
ggsave(filename = "figures/results/manuscript/stefan_data/carbon_cost_abacus_local_biome_scale_COC.png",
       plot = local_biome,
       width = 14,
       height = 18,
       units = "cm")
