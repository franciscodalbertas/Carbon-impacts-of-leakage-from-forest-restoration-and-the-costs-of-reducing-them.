# load packages ----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(scales)
library(shadowtext)

#-------------------------------------------------------------------------------

# read results
abacus <- readRDS("output_tables/abacus_high_low_leakage_projects_r1_local_biome_country_scale_prj_yield_SCC_3_USD.rds")
verra <- readRDS("output_tables/verra_high_low_leakage_projects_r1_local_biome_country_scale_prj_yield_SCC_3_USD.rds")

required_cols <- c(
  "name_biome",
  "approach",
  "leakage_scale",
  "mitigation_strategy",
  "yield_carbon_ratio",
  "coordinates",
  "final_CO2_balance_corrected_minus_COC",
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

# producing output just for ignoring to include in the text
df_ignoring <- df %>% filter(approach == "ignoring")

# filter out ignoring
df <- df %>% filter(approach != "ignoring")

# defining order
df$approach <- factor(df$approach, levels = c("no_mitigation", "intensification"))
df$source <- factor(df$source, levels = c("Verra", "ABACUS"))

y_label <- expression("Cost per tonne of CO"[2]*"e (USD)")

# filter strategies and scales
local <- df %>%
  filter(
    leakage_scale %in% c("ignoring", "local", "cluster"),
    mitigation_strategy != "minimize_costs"
  ) %>%
  mutate(scale = "local")

biome <- df %>%
  filter(
    leakage_scale %in% c("ignoring", "biome"),
    mitigation_strategy != "minimize_costs"
  ) %>%
  mutate(scale = "biome")

plot_ready_local_biome <- bind_rows(local, biome) %>%
  mutate(
    CO2_cost_COC_USD = if_else(CO2_cost_COC_USD <= 0, NA_real_, CO2_cost_COC_USD),
    name_biome = case_when(
      grepl("Amaz|Amazon", as.character(name_biome)) ~ "Amazon",
      grepl("Mata|Atlantic", as.character(name_biome)) ~ "Atlantic Forest",
      TRUE ~ as.character(name_biome)
    ),
    name_biome = factor(name_biome, levels = c("Atlantic Forest", "Amazon")),
    scale = factor(scale, levels = c("local", "biome")),
    yield_carbon_ratio = factor(yield_carbon_ratio, levels = c("max", "min")),
    x_base = if_else(yield_carbon_ratio == "max", 1, 2),
    x_offset = if_else(source == "Verra", -0.18, 0.18),
    x_plot = x_base + x_offset
  ) %>%
  filter(!is.na(name_biome))

# Net carbon balance summary for reporting in the text.
project_net_co2_balance_table <- plot_ready_local_biome %>%
  group_by(source, scale, approach, leakage_scale, name_biome, yield_carbon_ratio, coordinates) %>%
  summarize(
    project_median_CO2_balance = median(final_CO2_balance_corrected_minus_COC, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(source, scale, approach, leakage_scale, name_biome, yield_carbon_ratio) %>%
  summarize(
    median_net_CO2_balance = median(project_median_CO2_balance, na.rm = TRUE),
    n_projects = n(),
    n_projects_positive_net_CO2_balance = sum(project_median_CO2_balance > 0, na.rm = TRUE),
    .groups = "drop"
  )

# Median labels and mean points for the plot.
median_labels_local_biome <- plot_ready_local_biome %>%
  filter(!is.na(CO2_cost_COC_USD)) %>%
  group_by(scale, name_biome, approach, yield_carbon_ratio, source, x_plot) %>%
  summarize(
    n_positive = n(),
    median_cost = median(CO2_cost_COC_USD, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_positive >= 2)

mean_points_local_biome <- plot_ready_local_biome %>%
  filter(!is.na(CO2_cost_COC_USD)) %>%
  group_by(scale, name_biome, approach, yield_carbon_ratio, source, x_plot) %>%
  summarize(
    n_positive = n(),
    mean_cost = mean(CO2_cost_COC_USD, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_positive >= 2)

# add annotation for cases with no net carbon sequestration after leakage
annotation_df_local_biome <- project_net_co2_balance_table %>%
  filter(
    approach == "no_mitigation",
    n_projects_positive_net_CO2_balance == 0
  ) %>%
  mutate(
    x_base = if_else(yield_carbon_ratio == "max", 1, 2),
    x_offset = if_else(source == "Verra", -0.18, 0.18),
    x = x_base + x_offset,
    y = 1,
    label = "no net carbon removal after leakage",
    scale = factor(scale, levels = levels(plot_ready_local_biome$scale)),
    name_biome = factor(name_biome, levels = levels(plot_ready_local_biome$name_biome)),
    approach = factor(approach, levels = levels(plot_ready_local_biome$approach))
  )

panel_labels <- expand.grid(
  scale = c("local", "biome"),
  name_biome = c("Atlantic Forest", "Amazon"),
  approach = c("no_mitigation", "intensification"),
  stringsAsFactors = FALSE
) %>%
  mutate(
    scale = factor(scale, levels = levels(plot_ready_local_biome$scale)),
    name_biome = factor(name_biome, levels = levels(plot_ready_local_biome$name_biome)),
    approach = factor(approach, levels = levels(plot_ready_local_biome$approach))
  ) %>%
  arrange(
    scale,
    name_biome,
    approach
  ) %>%
  mutate(
    panel_label = LETTERS[seq_len(n())]
  )

violin_width <- 0.35

plot_abacus_verra_local_biome <- plot_ready_local_biome %>%
  ggplot(aes(x = x_plot, y = CO2_cost_COC_USD, fill = yield_carbon_ratio)) +
  geom_violin(
    aes(
      alpha = source,
      colour = source,
      group = interaction(yield_carbon_ratio, source)
    ),
    width = violin_width,
    linewidth = 0.4,
    na.rm = TRUE
  ) +
  geom_point(
    data = mean_points_local_biome,
    aes(
      x = x_plot,
      y = mean_cost,
      colour = source
    ),
    inherit.aes = FALSE,
    shape = 21,
    size = 1.5,
    fill = "black",
    colour = "white",
    stroke = 0.6,
    show.legend = FALSE
  ) +
  shadowtext::geom_shadowtext(
    data = median_labels_local_biome,
    aes(
      x = x_plot,
      y = median_cost,
      label = round(median_cost, 0),
      colour = source
    ),
    inherit.aes = FALSE,
    vjust = 0,
    size = 2.5,
    bg.colour = "white",
    g.r = 0.2,
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
    values = c("max" = "lightgreen", "min" = "darkgreen"),
    breaks = c("max", "min"),
    name = "Forgone-production-to-carbon-gained",
    labels = c("max" = "high", "min" = "low")
  ) +
  scale_alpha_manual(
    values = c("Verra" = 1, "ABACUS" = 0.45),
    guide = "none"
  ) +
  scale_colour_manual(
    values = c(
      "Verra" = "gray60",
      "ABACUS" = "black",
      "black" = "black",
      "gray60" = "gray60"
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = c(0.82, 1.18, 1.82, 2.18),
    labels = c("30", "100", "30", "100"),
    limits = c(0.55, 2.45),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(trans = "log10", labels = comma) +
  facet_grid(
    scale + name_biome ~ approach,
    labeller = labeller(
      scale = c(
        "local" = "Local",
        "biome" = "Biome"
      ),
      approach = c(
        "ignoring" = "Ignoring leakage",
        "no_mitigation" = "Accounting for leakage (no mitigation)",
        "intensification" = "Accounting for leakage (with mitigation)"
      )
    ),
    scales = "free"
  ) +
  xlab("") +
  ylab(y_label) +
  theme_classic() +
  theme(
    legend.position = "top",
    text = element_text(size = 7),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_blank()
  )

if (nrow(annotation_df_local_biome) > 0) {
  plot_abacus_verra_local_biome <- plot_abacus_verra_local_biome +
    shadowtext::geom_shadowtext(
      data = annotation_df_local_biome,
      aes(x = x, y = y, label = label),
      angle = 90,
      bg.colour = "lightgreen",
      colour = "darkgray",
      g.r = 0.001,
      hjust = -0.05,
      vjust = 0,
      size = 2,
      inherit.aes = FALSE
    )
}

plot_abacus_verra_local_biome

ggsave(
  filename = "figures/extended_figures/extended_figure_3.png",
  plot = plot_abacus_verra_local_biome,
  width = 14,
  height = 18,
  units = "cm"
)
