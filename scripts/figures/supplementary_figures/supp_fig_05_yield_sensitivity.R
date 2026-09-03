#---- loading packages ---------------------------------------------------------

library(dplyr)
library(ggplot2)
library(scales)
library(shadowtext)

#-------------------------------------------------------------------------------

input_file <- "output_tables/abacus_high_low_leakage_projects_country_scale_comparing_yield_model_SCC_3_USD.rds"
output_dir <- "figures/supp_figures"
output_file <- file.path(output_dir, "supp_fig_5.png")

df <- readRDS(input_file)
unique(df$yield_model)

plot_data <- df %>%
  mutate(
    FP_type = factor(FP_type, levels = c("nogrow", "prj", "carrying")),
    yield_carbon_ratio = recode(
      yield_carbon_ratio,
      "max" = "high",
      "min" = "low",
      "high" = "high",
      "low" = "low"
    ),
    yield_carbon_ratio = factor(yield_carbon_ratio, levels = c("high", "low")),
    name_biome = recode(
      name_biome,
      "Mata Atlântica" = "Atlantic forest",
      "Amazônia" = "Amazon"
    ),
    name_biome = factor(name_biome, levels = c("Atlantic forest", "Amazon"))
  ) %>%
  filter(
    mitigation_strategy == "optimize_carbon",
    mitigation_scale == "country",
    CO2_cost_COC > 0
  )

medians <- plot_data %>%
  group_by(yield_carbon_ratio, name_biome, FP_type) %>%
  summarise(
    median_cost = median(CO2_cost_COC, na.rm = TRUE),
    .groups = "drop"
  )

#---- plotting -----------------------------------------------------------------

position_dodge_val <- position_dodge(width = 0.9)

y_label <- expression("Cost per tonne of CO"[2] * "e (USD)")

p <- plot_data %>%
  ggplot(aes(x = FP_type, y = CO2_cost_COC, fill = yield_carbon_ratio)) +
  geom_violin(
    position = position_dodge_val,
    trim = FALSE,
    scale = "width",
    colour = "grey35",
    linewidth = 0.25
  ) +
  scale_y_log10(labels = label_comma()) +
  scale_x_discrete(labels = c("baseline", "projected", "carrying")) +
  scale_fill_manual(
    values = c("high" = "lightgreen", "low" = "darkgreen"),
    name = "Forgone-production-to-carbon-gained"
  ) +
  shadowtext::geom_shadowtext(
    data = medians,
    aes(
      x = FP_type,
      y = median_cost,
      label = round(median_cost, 0),
      fill = yield_carbon_ratio
    ),
    position = position_dodge_val,
    size = 2.5,
    bg.colour = "white",
    r = 0.2,
    colour = "black",
    inherit.aes = FALSE
  ) +
  facet_grid(name_biome ~ yield_carbon_ratio, scales = "free") +
  labs(
    x = "yield assumption",
    y = y_label,
    title = "",
    subtitle = ""
  ) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    legend.position = "none",
    text = element_text(size = 7),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

p

#---- save ---------------------------------------------------------------------

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = output_file,
  plot = p,
  width = 14,
  height = 11,
  units = "cm"
)
