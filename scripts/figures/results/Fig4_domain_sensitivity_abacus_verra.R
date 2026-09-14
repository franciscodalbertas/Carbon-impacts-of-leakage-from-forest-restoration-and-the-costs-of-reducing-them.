# load packages ----------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(shadowtext)
library(scales)
#-------------------------------------------------------------------------------

abacus <- readRDS("output_tables/abacus_high_low_leakage_projects_r1_local_biome_country_scale_prj_yield_SCC_3_USD.rds")
verra <- readRDS("output_tables/verra_high_low_leakage_projects_r1_local_biome_country_scale_prj_yield_SCC_3_USD.rds")

required_cols <- c(
  "name_biome",
  "leakage_scale",
  "yield_carbon_ratio",
  "mitigation_strategy",
  "CO2_cost_COC_USD"
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

y_label <- expression("Cost per tonne of CO"[2]*"e (USD)")

plot_data <- df %>%
  mutate(
    source = factor(source, levels = c("Verra", "ABACUS")),
    leakage_scale = if_else(leakage_scale == "cluster", "local", leakage_scale),
    leakage_scale = factor(leakage_scale, levels = c("ignoring", "local", "biome", "country")),
    yield_carbon_ratio = if_else(yield_carbon_ratio == "max", "high", "low"),
    yield_carbon_ratio = factor(yield_carbon_ratio, levels = c("high", "low")),
    name_biome = case_when(
      grepl("Amaz|Amazon", as.character(name_biome)) ~ "Amazon",
      grepl("Mata|Atlantic", as.character(name_biome)) ~ "Atlantic Forest",
      TRUE ~ as.character(name_biome)
    ),
    name_biome = factor(name_biome, levels = c("Atlantic Forest", "Amazon")),
    CO2_cost_COC_USD = if_else(CO2_cost_COC_USD <= 0, NA_real_, CO2_cost_COC_USD)
  ) %>%
  filter(
    mitigation_strategy == "optimize_carbon",
    !is.na(CO2_cost_COC_USD),
    !is.na(name_biome)
  )

median_labels <- plot_data %>%
  group_by(leakage_scale, yield_carbon_ratio, name_biome, source) %>%
  summarise(
    n_positive = n(),
    median_cost = median(CO2_cost_COC_USD, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_positive >= 2)

mean_points <- plot_data %>%
  group_by(leakage_scale, yield_carbon_ratio, name_biome, source) %>%
  summarise(
    n_positive = n(),
    mean_cost = mean(CO2_cost_COC_USD, na.rm = TRUE),
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

dodge_width <- 0.75
violin_width <- 0.8

cost_scale_abacus_verra <- plot_data %>%
  ggplot(aes(x = leakage_scale, y = CO2_cost_COC_USD, fill = yield_carbon_ratio)) +
  geom_violin(
    aes(
      alpha = source,
      colour = source,
      group = interaction(leakage_scale, source)
    ),
    position = position_dodge(width = dodge_width),
    width = violin_width,
    linewidth = 0.4,
    na.rm = TRUE
  ) +
  geom_point(
    data = mean_points,
    aes(
      x = leakage_scale,
      y = mean_cost,
      group = source
    ),
    position = position_dodge(width = dodge_width),
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
      x = leakage_scale,
      y = median_cost*0.88,
      label = round(median_cost, 0),
      group = source,
      colour = source
    ),
    position = position_dodge(width = dodge_width),
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
  scale_fill_manual(
    values = c("high" = "lightgreen", "low" = "darkgreen"),
    guide = "none"
  ) +
  scale_alpha_manual(
    values = c("Verra" = 0.45, "ABACUS" = 1),
    breaks = c("Verra", "ABACUS"),
    labels = c("Verra" = "30%", "ABACUS" = "100%"),
    name = ""
  ) +
  scale_colour_manual(
    values = c("Verra" = "gray60", "ABACUS" = "black"),
    breaks = c("Verra", "ABACUS"),
    labels = c("Verra" = "30%", "ABACUS" = "100%"),
    name = ""
  ) +
  guides(
    alpha = guide_legend(override.aes = list(fill = "white", colour = c("gray60", "black"))),
    colour = "none"
  ) +
  scale_y_continuous(trans = "log10", labels = comma) +
  facet_grid(name_biome ~ yield_carbon_ratio, scales = "free") +
  theme_classic() +
  labs(
    x = "Spatial Scale",
    y = y_label,
    title = "",
    subtitle = ""
  ) +
  theme(
    legend.position = "top",
    text = element_text(size = 7),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_blank()
  )

cost_scale_abacus_verra

ggsave(
  filename = "figures/Fig4_abacus_verra.png",
  plot = cost_scale_abacus_verra,
  width = 14,
  height = 13,
  units = "cm"
)
