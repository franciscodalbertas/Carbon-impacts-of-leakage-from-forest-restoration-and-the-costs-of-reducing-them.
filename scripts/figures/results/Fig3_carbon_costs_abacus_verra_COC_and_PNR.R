# load packages ----------------------------------------------------------------

library(tidyr)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(scales)
library(shadowtext)

#-------------------------------------------------------------------------------

# read results
abacus <- readRDS("output_tables/abacus_high_low_leakage_projects_r1_local_biome_country_scale_prj_yield_SCC_3_USD.rds")
verra <- readRDS("output_tables/verra_high_low_leakage_projects_r1_country_scale_prj_yield_SCC_3_USD.rds")

required_cols <- c(
  "name_biome",
  "approach",
  "leakage_scale",
  "mitigation_strategy",
  "yield_carbon_ratio",
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

country <- df %>%
  filter(
    leakage_scale %in% c("ignoring", "country"),
    mitigation_strategy != "minimize_costs"
  )

y_label <- expression("Cost per tonne of CO"[2]*"e (USD)")

plot_ready_country <- country %>%
  mutate(
    CO2_cost_COC_USD = if_else(CO2_cost_COC_USD <= 0, NA_real_, CO2_cost_COC_USD),
    name_biome = case_when(
      grepl("Amaz|Amazon", as.character(name_biome)) ~ "Amazon",
      grepl("Mata|Atlantic", as.character(name_biome)) ~ "Atlantic Forest",
      TRUE ~ as.character(name_biome)
    ),
    name_biome = factor(name_biome, levels = c("Atlantic Forest", "Amazon")),
    yield_carbon_ratio = factor(yield_carbon_ratio, levels = c("max", "min")),
    x_base = if_else(yield_carbon_ratio == "max", 1, 2),
    x_offset = if_else(source == "Verra", -0.18, 0.18),
    x_plot = x_base + x_offset
  ) %>%
  filter(!is.na(name_biome))

mean_labels_country <- plot_ready_country %>%
  filter(!is.na(CO2_cost_COC_USD)) %>%
  group_by(name_biome, approach, yield_carbon_ratio, source, x_plot) %>%
  summarize(
    n_positive = n(),
    mean_cost = mean(CO2_cost_COC_USD, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_positive >= 2)

# add annotation for cases with no net carbon sequestration after leakage
annotation_df_country <- data.frame(
  name_biome = c("Atlantic Forest"),
  approach = "no_mitigation",
  x = 1.18,
  # y = mean_labels_country %>%
  #   filter(name_biome == "Atlantic Forest", approach == "no_mitigation") %>%
  #   summarize(y = max(mean_cost, na.rm = TRUE)) %>%
  #   pull(y),
  y = 1,
  label = "no net carbon removal after leakage"
)

annotation_df_country <- annotation_df_country %>%
  mutate(
    name_biome = factor(name_biome, levels = c("Atlantic Forest", "Amazon")),
    approach = factor(approach, levels = levels(country$approach))
  )

panel_labels <- expand.grid(
  name_biome = c("Atlantic Forest", "Amazon"),
  approach = c("no_mitigation", "intensification"),
  stringsAsFactors = FALSE
) %>%
  mutate(
    name_biome = factor(name_biome, levels = c("Atlantic Forest", "Amazon")),
    approach = factor(approach, levels = levels(country$approach)),
    panel_label = c("A", "C", "B", "D")
  )

violin_width <- 0.35

plot_abacus_verra_country <- plot_ready_country %>%
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
  shadowtext::geom_shadowtext(
    data = mean_labels_country,
    aes(
      x = x_plot,
      y = mean_cost,
      label = round(mean_cost, 0),
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
      "Verra" = "black",
      "ABACUS" = "gray60",
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
    name_biome ~ approach,
    labeller = labeller(approach = c(
      "ignoring" = "Ignoring leakage",
      "no_mitigation" = "Accounting for leakage (no mitigation)",
      "intensification" = "Accounting for leakage (with mitigation)"
    )),
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

plot_abacus_verra_country <- plot_abacus_verra_country +
  shadowtext::geom_shadowtext(
    data = annotation_df_country,
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

plot_abacus_verra_country

# main text
ggsave(
  filename = "figures/Fig3_abacus_verra.png",
  plot = plot_abacus_verra_country,
  width = 14,
  height = 11,
  units = "cm"
)
