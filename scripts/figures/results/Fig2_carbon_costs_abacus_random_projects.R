# load packages ----------------------------------------------------------------

library(tidyr)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(scales)
library(shadowtext)

#-------------------------------------------------------------------------------

# read results 
df <- readRDS("output_tables/abacus_random_projects_local_biome_country_scale_prj_yield_SCC_3_USD.rds")

# defining order
df$approach <- factor(df$approach,levels = c("ignoring","no_mitigation"))

df$name_biome <- factor(df$name_biome,levels = c("Mata Atlântica","Amazônia"))


# filter strategy and scale

local <- df %>% filter(leakage_scale %in% c("ignoring","local"),
                       mitigation_strategy !="minimize_costs")

biome <- df %>% filter(leakage_scale %in% c("ignoring","biome"),
                          mitigation_strategy !="minimize_costs")


country <- df %>% filter(leakage_scale %in% c("ignoring","country"),
                       mitigation_strategy !="minimize_costs")


# make it more like figure 4!
y_label = expression("Cost per tonne of CO"[2]*"e (USD)") 

# mean labels
mean_labels_local <- local %>%
  mutate(name_biome= if_else(name_biome == "Amazônia","Amazon","Atlantic Forest")) %>%
  filter(CO2_cost_USD > 0) %>%
  group_by(name_biome, approach, yield_carbon_ratio) %>%
  summarize(mean_cost = median(CO2_cost_USD, na.rm = TRUE), .groups = "drop")


mean_labels_biome <- biome %>%
  mutate(name_biome= if_else(name_biome == "Amazônia","Amazon","Atlantic Forest")) %>%
  filter(CO2_cost_USD > 0) %>%
  group_by(name_biome, approach, yield_carbon_ratio) %>%
  summarize(mean_cost = median(CO2_cost_USD, na.rm = TRUE), .groups = "drop")


mean_labels_country <- country %>%
  mutate(name_biome= if_else(name_biome == "Amazônia","Amazon","Atlantic Forest")) %>%
  filter(CO2_cost_USD > 0) %>%
  group_by(name_biome, approach, yield_carbon_ratio) %>%
  summarize(mean_cost = median(CO2_cost_USD, na.rm = TRUE), .groups = "drop")


plot_abacus_biome <- biome %>%
  mutate(CO2_cost_USD = if_else(CO2_cost_USD<=0,NA,CO2_cost_USD),
         name_biome= if_else(name_biome == "Amazônia","Amazon","Atlantic Forest"),
         # put Atlantic Forest first
         name_biome = factor(name_biome, levels = c("Atlantic Forest", "Amazon"))
         )%>%
  ggplot(aes(x = name_biome,y = CO2_cost_USD
             #, fill= name_biome
             ))+
  geom_violin()+
  shadowtext::geom_shadowtext(data = mean_labels_biome,
                              aes(x = name_biome,
                                  y = mean_cost,
                                  label = round(mean_cost, 0)),
                              #hjust = -0.05,
                              #vjust = 0,
                              size = 2.5,
                              bg.colour = "white", # color of the halo
                              colour = "black",    # color of the text
                              inherit.aes = FALSE) +
  scale_fill_manual(values = c("Amazon" = "white",
                               "Atlantic Forest" = "darkgray"),
                    name = "",
                    labels = c("ignoring" = "ignoring leakage",
                               "no_mitigation" = "accounting for leakage (no mitigation)")) +
  scale_y_continuous(trans = "log10",labels = comma)+
  facet_grid(~approach,
             labeller = labeller(approach = c(
               "ignoring" = "Ignoring leakage", 
               "no_mitigation" = "Accounting for leakage (no mitigation)")),
             scales = "free")+
  xlab("")+
  ylab(y_label)+
  theme_classic()+
  theme(
    legend.position = "top",
    text = element_text(size = 7),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    #axis.text.x = element_blank(),
    #axis.ticks.x = element_blank()
    strip.background = element_rect(colour = NA)
  )

plot_abacus_local<- local %>%
  mutate(CO2_cost = if_else(CO2_cost_USD<=0,NA,CO2_cost_USD),
         name_biome= if_else(name_biome == "Amazônia","Amazon","Atlantic Forest"),
         # put Atlantic Forest first
         name_biome = factor(name_biome, levels = c("Atlantic Forest", "Amazon")))%>%
  ggplot(aes(x = name_biome,y = CO2_cost
             #,fill = name_biome
             ))+
  geom_violin()+
  shadowtext::geom_shadowtext(data = mean_labels_local,
            aes(x = name_biome, 
                y = mean_cost, 
                label = round(mean_cost, 0)),
            inherit.aes = FALSE,
            vjust = 0,
            size = 2.5,
            bg.colour ="white",
            colour = "black") +
  scale_fill_manual(values = c("Amazon" = "white",
                               "Atlantic Forest" = "darkgray"),
                    name = "",
                    labels = c("ignoring" = "ignoring leakage",
                               "no_mitigation" = "accounting for leakage (no mitigation)")) +
  scale_y_continuous(trans = "log10",labels = comma)+
  facet_grid(~approach,
             labeller = labeller(approach = c(
               "ignoring" = "Ignoring leakage", 
               "no_mitigation" = "Accounting for leakage (no mitigation)")),
             scales = "free")+
  xlab("")+
  ylab(y_label)+
  theme_classic()+
  theme(
    legend.position = "top",
    text = element_text(size = 7),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    #axis.text.x = element_blank(),
    #axis.ticks.x = element_blank()
    strip.background = element_rect(colour = NA)
  )

plot_abacus_country <- country %>%
  mutate(CO2_cost = if_else(CO2_cost_USD<=0,NA,CO2_cost_USD),
         name_biome= if_else(name_biome == "Amazônia","Amazon","Atlantic Forest"),
         # put Atlantic Forest first
         name_biome = factor(name_biome, levels = c("Atlantic Forest", "Amazon")))%>%
  ggplot(aes(x = name_biome,y = CO2_cost
             #,fill = name_biome
             ))+
  geom_violin()+
  shadowtext::geom_shadowtext(data = mean_labels_country,
            aes(x = name_biome, 
                y = mean_cost, 
                label = round(mean_cost, 0)),
            inherit.aes = FALSE,
            vjust = 0,
            size = 2.5,
            bg.colour = "white",
            colour = "black") +
  scale_fill_manual(values = c("Amazon" = "white",
                               "Atlantic Forest" = "darkgray"),
                    name = "",
                    labels = c("ignoring" = "ignoring leakage",
                               "no_mitigation" = "accounting for leakage (no mitigation)")) +
  scale_y_continuous(trans = "log10",labels = comma)+
  facet_grid(~approach,
             labeller = labeller(approach = c(
               "ignoring" = "Ignoring leakage", 
               "no_mitigation" = "Accounting for leakage (no mitigation)")),
             scales = "free")+
  xlab("")+
  ylab(y_label)+
  theme_classic()+
  theme(
    legend.position = "top",
    text = element_text(size = 7),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    #axis.text.x = element_blank(),
    #axis.ticks.x = element_blank()
    strip.background = element_rect(colour = NA)
  )

plot_abacus_country

# combine local and biome

local_biome <- rbind(local %>% mutate(scale = "local"),biome %>% mutate(scale = "biome"))
mean_labels_local_biome <- rbind(mean_labels_local%>% mutate(scale = "local"),mean_labels_biome%>% mutate(scale = "biome"))

# DEFINING ORDER

local_biome$scale <- factor(local_biome$scale,levels = rev(c("local","biome")))

plot_abacus_local_biome <- local_biome %>%
  mutate(CO2_cost = if_else(CO2_cost_USD<=0,NA,CO2_cost_USD),
         name_biome= if_else(name_biome == "Amazônia","Amazon","Atlantic Forest"),
         # put Atlantic Forest first
         name_biome = factor(name_biome, levels = c("Atlantic Forest", "Amazon")))%>%
  ggplot(aes(x = name_biome,y = CO2_cost
             #,fill = name_biome
  ))+
  geom_violin()+
  shadowtext::geom_shadowtext(data = mean_labels_local_biome,
                              aes(x = name_biome, 
                                  y = mean_cost, 
                                  label = round(mean_cost, 0)),
                              inherit.aes = FALSE,
                              vjust = 0,
                              size = 2.5,
                              bg.colour ="white",
                              colour = "black") +
  scale_fill_manual(values = c("Amazon" = "white",
                               "Atlantic Forest" = "darkgray"),
                    name = "",
                    labels = c("ignoring" = "ignoring leakage",
                               "no_mitigation" = "accounting for leakage (no mitigation)")) +
  scale_y_continuous(trans = "log10",labels = comma)+
  facet_grid(scale~approach,
             labeller = labeller(approach = c(
               "ignoring" = "Ignoring leakage", 
               "no_mitigation" = "Accounting for leakage (no mitigation)")),
             scales = "free")+
  xlab("")+
  ylab(y_label)+
  theme_classic()+
  theme(
    legend.position = "top",
    text = element_text(size = 7),
    
    # ✅ remove facet label rectangles
    strip.background = element_blank(),
    
    # ✅ keep panel outlines
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    
    # ✅ keep axis lines (classic has only left+bottom by default)
    axis.line = element_line(color = "black", linewidth = 0.3),
    axis.line.y.right = element_line(color = "black", linewidth = 0.3),
    axis.line.x.top    = element_line(color = "black", linewidth = 0.3)
  )


plot_abacus_local_biome

# saving!
# main text
ggsave(filename = "figures/Fig2.png",
       plot = plot_abacus_country,
       width = 11,
       height = 7,
       units = "cm")

# supp
# ggsave(filename = "figures/results/manuscript/carbon_cost_abacus_local_biome_scale_avg_proj_using_PNR.png",
#        plot = plot_abacus_local_biome,
#        width = 11,
#        height = 9,
#        units = "cm")
