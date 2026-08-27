# load packages ----------------------------------------------------------------

library(ggtern)
library(dplyr)

#-------------------------------------------------------------------------------

abacus <- readRDS("output_tables/abacus_high_low_leakage_projects_r1_local_biome_country_scale_prj_yield_SCC_3_USD.rds")

required_cols <- c(
  "name_biome",
  "yield_carbon_ratio",
  "mitigation_strategy",
  "mitigation_scale",
  "project_costs_USD",
  "total_restoration_costs_USD_2021",
  "total_land_value_USD_2021",
  "mitigation_costs_USD_50y_2021"
)

missing_abacus_cols <- setdiff(required_cols, names(abacus))

if (length(missing_abacus_cols) > 0) {
  stop("Missing columns in abacus data: ", paste(missing_abacus_cols, collapse = ", "))
}

ternary_data <- abacus %>%
  filter(
    mitigation_strategy == "optimize_carbon",
    mitigation_scale == "country",
    project_costs_USD > 0
  ) %>%
  mutate(
    name_biome = case_when(
      grepl("Amaz|Amazon", as.character(name_biome)) ~ "Amazon",
      grepl("Mata|Atlantic", as.character(name_biome)) ~ "Atlantic Forest",
      TRUE ~ as.character(name_biome)
    ),
    name_biome = factor(name_biome, levels = c("Amazon", "Atlantic Forest")),
    yield_carbon_ratio = if_else(yield_carbon_ratio == "max", "high", "low"),
    yield_carbon_ratio = factor(yield_carbon_ratio, levels = c("high", "low")),
    R = total_restoration_costs_USD_2021 / project_costs_USD,
    L = total_land_value_USD_2021 / project_costs_USD,
    M = mitigation_costs_USD_50y_2021 / project_costs_USD
  ) %>%
  filter(
    !is.na(name_biome),
    !is.na(yield_carbon_ratio),
    is.finite(R),
    is.finite(L),
    is.finite(M)
  )

relative_costs <- ggtern(data = ternary_data, aes(x = R, y = L, z = M)) +
  stat_density_tern(
    geom = "polygon",
    n = 300,
    aes(fill = ..level.., alpha = ..level..),
    bins = 10
  ) +
  scale_fill_gradient(low = "lightyellow", high = "red") +
  facet_grid(name_biome ~ yield_carbon_ratio) +
  theme_minimal() +
  labs(
    title = "",
    T = "R\n(\u2199)",
    L = "L(\u2192)",
    R = "(\u2196)\nM"
  ) +
  theme(
    legend.position = "none",
    text = element_text(size = 7)
  ) +
  guides(fill = guide_colorbar(title = "Density"), alpha = FALSE)

relative_costs

ggsave(
  filename = "figures/extended_figures/extended_figure_7.png",
  plot = relative_costs,
  width = 11,
  height = 11,
  units = "cm"
)

ggsave(
  filename = "figures/extended_figures/extended_figure_7.svg",
  plot = relative_costs,
  width = 11,
  height = 11,
  units = "cm"
)
