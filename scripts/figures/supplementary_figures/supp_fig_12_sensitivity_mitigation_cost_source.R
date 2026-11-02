#---- load packages ------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(purrr)
library(scales)
library(tibble)
library(tidyr)

#-------------------------------------------------------------------------------

mitigation_scale <- "country"
source_params <- "Abacus"
strategy <- "optimize_carbon"
yield_param <- "yield_prj"
SCC_param <- "SCC_3"
round_param <- "round1"

biomes <- c(
  mataatlantica = "Mata Atlântica",
  amazonia = "Amazônia"
)

cost_sources <- c("carlos", "wirsenius")
cost_source_labels <- c(
  carlos = "Carlos",
  wirsenius = "Wirsenius"
)
cost_source_colours <- c(
  carlos = "#F8766D",
  wirsenius = "#00BFC4"
)

results_dir <- "results_with_mitigation"
output_dir <- "figures/supp_figures/"
output_file <- file.path(output_dir, "supp_fig_12.png")

cost_file <- function(biome, cost_source) {
  file.path(
    results_dir,
    paste0(
      "mitigation_costs_", mitigation_scale,
      "_scale_intensification_", strategy, "_",
      source_params, "_", biome, "_",
      yield_param, "_", SCC_param, "_", round_param, "_",
      cost_source, ".rds"
    )
  )
}

cost_file_variants <- function(biome, cost_source) {
  source_variants <- unique(c(
    cost_source,
    tolower(cost_source),
    tools::toTitleCase(cost_source)
  ))

  unlist(map(source_variants, ~ cost_file(biome, .x)), use.names = FALSE)
}

read_cost_source <- function(cost_source) {
  paths <- tibble(
    biome_file_name = names(biomes),
    paths_to_try = map(biome_file_name, cost_file_variants, cost_source = cost_source),
    path = map_chr(paths_to_try, ~ .x[file.exists(.x)][1])
  )

  missing_paths <- paths %>% filter(is.na(path))

  if (nrow(missing_paths) > 0) {
    warning(
      "Missing ", cost_source, " result file(s): ",
      paste(map_chr(missing_paths$paths_to_try, paste, collapse = " or "), collapse = "; "),
      call. = FALSE
    )
    paths <- paths %>% filter(!is.na(path))
  }

  if (nrow(paths) == 0) {
    return(tibble())
  }

  paths %>%
    mutate(data = map(path, readRDS)) %>%
    select(biome_file_name, data) %>%
    unnest(data, names_repair = "unique") %>%
    mutate(
      name_biome = if_else(
        is.na(name_biome),
        unname(biomes[biome_file_name]),
        name_biome
      )
    ) %>%
    mutate(cost_source = cost_source)
}

projects_raw <- cost_sources %>%
  set_names() %>%
  map(read_cost_source) %>%
  bind_rows()

if (nrow(projects_raw) == 0) {
  stop("No mitigation-cost result files were found for Wirsenius or Carlos.")
}

if (!"carlos" %in% projects_raw$cost_source) {
  message("Carlos results were not found yet. The figure will include them automatically once the files exist with the same naming style as Wirsenius.")
}

# create ignoring scenario (scale does not matter)
projects_ignoring <- projects_raw %>%
  mutate(
    mitigation_strategy = "ignoring",
    mitigation_costs_reais_50y = 0,
    mitigation_scale = "ignoring",
    approach = "ignoring",
    leakage_scale = "ignoring"
  ) %>%
  distinct()

projects_no_mitigation <- projects_raw %>%
  mutate(
    mitigation_strategy = "no_mitigation",
    mitigation_costs_reais_50y = 0,
    mitigation_scale = "no_mitigation",
    approach = "no_mitigation"
  ) %>%
  distinct()

projects_all <- bind_rows(
  projects_raw,
  projects_ignoring,
  projects_no_mitigation
)

# add column with final carbon balance based on the scenario
projects_all <- projects_all %>%
  mutate(
    final_CO2_balance_corrected =
      case_when(
        approach == "ignoring" ~ net_CO2_corrected_mitigation,
        approach == "no_mitigation" ~ net_CO2_corrected_no_mitigation,
        approach == "intensification" ~ net_CO2_corrected_mitigation
      ),
    final_CO2_balance_non_corrected =
      case_when(
        approach == "ignoring" ~ net_CO2_no_correction_mitigation,
        approach == "no_mitigation" ~ net_CO2_no_correction_no_mitigation,
        approach == "intensification" ~ net_CO2_no_correction_mitigation
      ),
    final_CO2_balance_corrected_minus_COC =
      case_when(
        approach == "ignoring" ~ final_CO2_balance_corrected,
        approach == "no_mitigation" ~ final_CO2_balance_corrected,
        approach == "intensification" ~
          final_CO2_balance_corrected - forgone_CO2_discounted
      ),
    final_CO2_balance_non_corrected_minus_COC =
      case_when(
        approach == "ignoring" ~ final_CO2_balance_non_corrected,
        approach == "no_mitigation" ~ final_CO2_balance_non_corrected,
        approach == "intensification" ~
          final_CO2_balance_non_corrected - forgone_CO2_discounted
      )
  )

# calculating total project costs in USD
IPCA_2021 <- 1.0579
IPCA_2024 <- 1.1583
IPCA_2023 <- 1.0462 * 1.0579
USD_rate_2021 <- 5.3955
USD_BR <- 5

projects_all <- projects_all %>%
  mutate(
    mitigation_costs_USD_50y_2021 = mitigation_costs_reais_50y / USD_BR,
    total_land_value_USD_2021 = total_land_value_reais_ha / IPCA_2023 / USD_rate_2021,
    total_restoration_costs_USD_2021 =
      total_restoration_costs_reais_2023 / IPCA_2024 / USD_rate_2021,
    project_costs_USD =
      total_restoration_costs_USD_2021 +
      total_land_value_USD_2021 +
      mitigation_costs_USD_50y_2021,
    final_CO2_balance_corrected_minus_COC =
      net_CO2_corrected_mitigation - forgone_CO2_discounted,
    CO2_cost_COC = project_costs_USD / final_CO2_balance_corrected_minus_COC
  )

################################################################################
#### plot Wirsenius vs Carlos intensification costs for ABACUS projects
################################################################################

y_label <- expression("Cost per tonne of CO"[2] * "e (USD)")

point_position <- position_jitterdodge(
  dodge.width = 0.8,
  jitter.width = 0.35,
  jitter.height = 0,
  seed = 123
)

box_position <- position_dodge(width = 0.8)

plot_data <- projects_all %>%
  filter(
    mitigation_strategy == "optimize_carbon",
    CO2_cost_COC > 0
  ) %>%
  mutate(
    leakage_group = recode(
      yield_carbon_ratio,
      "max" = "high",
      "min" = "low",
      "high" = "high",
      "low" = "low"
    ),
    name_biome = factor(name_biome, levels = c("Mata Atlântica", "Amazônia")),
    cost_source = factor(cost_source, levels = cost_sources),
    leakage_group = factor(leakage_group, levels = c("high", "low"))
  )

projects <- plot_data %>%
  ggplot(
    aes(
      x = leakage_group,
      y = CO2_cost_COC,
      fill = cost_source,
      colour = cost_source
    )
  ) +
  geom_point(
    shape = 16,
    size = 1.8,
    alpha = 0.28,
    position = point_position
  ) +
  geom_boxplot(
    fill = "white",
    outliers = FALSE,
    alpha = 0.5,
    width = 0.46,
    position = box_position
  ) +
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = scales::comma(round(..y.., 0))),
    position = box_position,
    vjust = -0.6,
    size = 3,
    colour = "black"
  ) +
  scale_y_log10(
    labels = label_comma(),
    breaks = c(10, 25, 50, 100, 1000, 10000)
  ) +
  scale_x_discrete(labels = c("high", "low")) +
  scale_fill_manual(
    name = "Mitigation cost source",
    labels = cost_source_labels,
    values = cost_source_colours
  ) +
  scale_colour_manual(
    name = "Mitigation cost source",
    labels = cost_source_labels,
    values = cost_source_colours
  ) +
  ylab(y_label) +
  xlab("") +
  facet_wrap(
    ~name_biome,
    ncol = 1,
    scales = "free",
    labeller = as_labeller(
      c(
        "Amazônia" = "Amazon",
        "Mata Atlântica" = "Atlantic forest"
      )
    )
  ) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_blank(),
    legend.position = "top"
  )

projects

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = output_file,
  plot = projects,
  width = 15,
  height = 17,
  units = "cm"
)
