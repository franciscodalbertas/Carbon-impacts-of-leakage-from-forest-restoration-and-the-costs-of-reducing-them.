# load packages ----------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(scales)
library(shadowtext)

#-------------------------------------------------------------------------------

abacus <- readRDS("output_tables/abacus_high_low_leakage_projects_r1_local_biome_country_scale_prj_yield_SCC_3_USD.rds")
verra <- readRDS("output_tables/verra_high_low_leakage_projects_r1_local_biome_country_scale_prj_yield_SCC_3_USD.rds")

required_cols <- c(
  "name_biome",
  "yield_carbon_ratio",
  "CO2_cost_COC_USD",
  "mitigation_strategy",
  "final_CO2_balance_non_corrected",
  "total_area_restored"
)

missing_abacus_cols <- setdiff(required_cols, names(abacus))
missing_verra_cols <- setdiff(required_cols, names(verra))

if (length(missing_abacus_cols) > 0) {
  stop("Missing columns in abacus data: ", paste(missing_abacus_cols, collapse = ", "))
}

if (length(missing_verra_cols) > 0) {
  stop("Missing columns in Verra data: ", paste(missing_verra_cols, collapse = ", "))
}

df <- bind_rows(
  verra %>% mutate(source = "Verra"),
  abacus %>% mutate(source = "ABACUS")
)


y_label = expression("Tonnes CO"[2]*"e ha"^-1)

plot_data <- df %>%
  mutate(
    source = factor(source, levels = c("Verra", "ABACUS")),
    name_biome = case_when(
      grepl("Amaz|Amazon", as.character(name_biome)) ~ "Amazon",
      grepl("Mata|Atlantic", as.character(name_biome)) ~ "Atlantic Forest",
      TRUE ~ as.character(name_biome)
    ),
    name_biome = factor(name_biome, levels = c("Atlantic Forest", "Amazon")),
    yield_carbon_ratio = if_else(yield_carbon_ratio == "max", "high", "low"),
    yield_carbon_ratio = factor(yield_carbon_ratio, levels = c("high", "low")),
    mitigation_strategy = factor(
      mitigation_strategy,
      levels = c("no_mitigation", "optimize_carbon")
    ),
    co2_ha = final_CO2_balance_non_corrected / total_area_restored
  ) %>%
  filter(
    CO2_cost_COC_USD > 0,
    mitigation_strategy %in% c("no_mitigation", "optimize_carbon"),
    !is.na(co2_ha),
    !is.na(name_biome)
  )

median_labels <- plot_data %>%
  group_by(name_biome, mitigation_strategy, yield_carbon_ratio, source) %>%
  summarise(
    n_positive = n(),
    median_co2_ha = median(co2_ha, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_positive >= 2)

mean_points <- plot_data %>%
  group_by(name_biome, mitigation_strategy, yield_carbon_ratio, source) %>%
  summarise(
    n_positive = n(),
    mean_co2_ha = mean(co2_ha, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_positive >= 2)

panel_labels <- expand.grid(
  name_biome = c("Atlantic Forest", "Amazon"),
  yield_carbon_ratio = c("high", "low"),
  stringsAsFactors = FALSE
) %>%
  mutate(
    name_biome = factor(name_biome, levels = levels(plot_data$name_biome)),
    yield_carbon_ratio = factor(yield_carbon_ratio, levels = levels(plot_data$yield_carbon_ratio)),
    panel_label = c("A", "C", "B", "D")
  )

violin_width <- 0.35

co2_balance <- plot_data %>%
  ggplot(aes(x = mitigation_strategy, y = co2_ha, fill = yield_carbon_ratio)) +
  geom_violin(
    aes(
      alpha = source,
      colour = source,
      group = interaction(mitigation_strategy, source)
    ),
    position = position_dodge(width = 0.75),
    width = violin_width,
    linewidth = 0.4,
    na.rm = TRUE
  ) +
  geom_point(
    data = mean_points,
    aes(
      x = mitigation_strategy,
      y = mean_co2_ha,
      group = source
    ),
    position = position_dodge(width = 0.75),
    inherit.aes = FALSE,
    shape = 21,
    size = 1.5,
    fill = "black",
    colour = "white",
    stroke = 0.6,
    show.legend = FALSE
  ) +
  shadowtext::geom_shadowtext(
    data = median_labels,
    aes(
      x = mitigation_strategy,
      y = median_co2_ha*0.88,
      label = round(median_co2_ha, 0),
      group = source,
      colour = source
    ),
    position = position_dodge(width = 0.75),
    inherit.aes = FALSE,
    size = 2.5,
    bg.colour = "white",
    bg.r = 0.2,
    show.legend = FALSE
  ) +
  geom_text(
    data = panel_labels,
    aes(x = -Inf, y = Inf, label = panel_label),
    inherit.aes = FALSE,
    hjust = -0.35,
    vjust = 1.15,
    fontface = "bold",
    size = 3
  ) +
  scale_x_discrete(
    labels = c(
      "no_mitigation" = "no mitigation",
      "optimize_carbon" = "optimize for carbon"
    )
  ) +
  scale_fill_manual(
    values = c("high" = "lightgreen", "low" = "darkgreen"),
    guide = "none"
  ) +
  scale_alpha_manual(
    values = c("Verra" = 1, "ABACUS" = 0.45),
    breaks = c("Verra", "ABACUS"),
    labels = c("Verra" = "30%", "ABACUS" = "100%"),
    name = ""
  ) +
  scale_colour_manual(
    values = c("Verra" = "gray60", "ABACUS" = "black"),
    guide = "none"
  ) +
  guides(
    alpha = guide_legend(override.aes = list(fill = "white", colour = c("gray60", "black")))
  ) +
  facet_grid(name_biome ~ yield_carbon_ratio, scales = "free") +
  theme_classic() +
  labs(x = "", y = y_label,
       title = "",
       subtitle = "") +
  theme(
    legend.position = "top",
    strip.background = element_blank(),
    text = element_text(size = 7),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )


co2_balance

ggsave(filename = "figures/extended_figures/extended_figure_6.png",
       co2_balance,
       width = 14,
       height = 11,
       units = "cm")

