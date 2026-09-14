#---- load packages ------------------------------------------------------------

library(dplyr)
library(purrr)
library(tibble)
library(tidyr)

#-------------------------------------------------------------------------------

results_dir <- "results_with_mitigation"
output_dir <- "output_tables"
raw_output_file <- file.path(
  output_dir,
  "supp_table_04_discount_leakage_sensitivity_raw.rds"
)
formatted_output_file <- file.path(
  output_dir,
  "supp_table_04_discount_leakage_sensitivity.csv"
)

mitigation_scale <- "country"
source_params <- "Abacus"
strategy <- "optimize_carbon"
yield_param <- "yield_prj"
round_param <- "round1"
intensification_name <- "wirsenius"

discount_rates <- c("SCC_2" = 2, "SCC_3" = 3, "SCC_4" = 4)
central_discount_rate <- 3

biomes <- c(
  mataatlantica = "Mata Atlântica",
  amazonia = "Amazônia"
)

# Set to FALSE if you want medians to include negative or zero carbon costs.
positive_costs_only <- TRUE

#-------------------------------------------------------------------------------

result_file <- function(biome, SCC_param) {
  file.path(
    results_dir,
    paste0(
      "mitigation_costs_", mitigation_scale,
      "_scale_intensification_", strategy, "_",
      source_params, "_", biome, "_",
      yield_param, "_", SCC_param, "_", round_param, "_",
      intensification_name, ".rds"
    )
  )
}

result_file_variants <- function(biome, SCC_param) {
  source_variants <- unique(c(source_params, tolower(source_params), toupper(source_params)))
  intensification_variants <- unique(c(
    intensification_name,
    tolower(intensification_name),
    tools::toTitleCase(intensification_name)
  ))

  expand_grid(
    source_variant = source_variants,
    intensification_variant = intensification_variants
  ) %>%
    mutate(
      path = file.path(
        results_dir,
        paste0(
          "mitigation_costs_", mitigation_scale,
          "_scale_intensification_", strategy, "_",
          source_variant, "_", biome, "_",
          yield_param, "_", SCC_param, "_", round_param, "_",
          intensification_variant, ".rds"
        )
      )
    ) %>%
    pull(path)
}

read_result_table <- function(path) {
  x <- readRDS(path)
  x_list <- unclass(x)
  sfc_cols <- vapply(x_list, inherits, logical(1), what = "sfc")
  x_list[sfc_cols] <- NULL
  tibble::as_tibble(x_list)
}

result_files <- expand_grid(
  biome_file_name = names(biomes),
  SCC_param = names(discount_rates)
) %>%
  mutate(
    paths_to_try = map2(biome_file_name, SCC_param, result_file_variants),
    path = map_chr(paths_to_try, ~ .x[file.exists(.x)][1]),
    discount_rate = unname(discount_rates[SCC_param])
  )

missing_files <- result_files %>% filter(is.na(path))

if (nrow(missing_files) > 0) {
  stop(
    "Missing mitigation result file(s): ",
    paste(map_chr(missing_files$paths_to_try, paste, collapse = " or "), collapse = "; "),
    call. = FALSE
  )
}

mitigation_projects <- result_files %>%
  mutate(data = map(path, read_result_table)) %>%
  select(biome_file_name, discount_rate, data) %>%
  unnest(data, names_repair = "unique") %>%
  mutate(
    name_biome = if_else(
      is.na(name_biome),
      unname(biomes[biome_file_name]),
      name_biome
    ),
    scenario = "With mitigation",
    mitigation_strategy = "optimize_carbon",
    approach = "intensification"
  )

ignoring_projects <- mitigation_projects %>%
  mutate(
    scenario = "Ignoring leakage",
    mitigation_strategy = "ignoring",
    mitigation_costs_reais_50y = 0,
    mitigation_scale = "ignoring",
    approach = "ignoring",
    leakage_scale = "ignoring"
  )

projects <- bind_rows(mitigation_projects, ignoring_projects) %>%
  mutate(
    final_CO2_balance_corrected =
      case_when(
        approach == "ignoring" ~ net_CO2_corrected_mitigation,
        approach == "intensification" ~ net_CO2_corrected_mitigation
      ),
    final_CO2_balance_corrected_minus_COC =
      case_when(
        approach == "ignoring" ~ final_CO2_balance_corrected,
        approach == "intensification" ~
          final_CO2_balance_corrected - forgone_CO2_discounted
      )
  )

# calculating total project costs in USD
IPCA_2021 <- 1.0579
IPCA_2024 <- 1.1583
IPCA_2023 <- 1.0462 * 1.0579
USD_rate_2021 <- 5.3955
USD_BR <- 5

projects <- projects %>%
  mutate(
    mitigation_costs_USD_50y_2021 = mitigation_costs_reais_50y / USD_BR,
    total_land_value_USD_2021 = total_land_value_reais_ha / IPCA_2023 / USD_rate_2021,
    total_restoration_costs_USD_2021 =
      total_restoration_costs_reais_2023 / IPCA_2024 / USD_rate_2021,
    project_costs_USD =
      total_restoration_costs_USD_2021 +
      total_land_value_USD_2021 +
      mitigation_costs_USD_50y_2021,
    CO2_cost_USD = project_costs_USD / final_CO2_balance_corrected_minus_COC,
    leakage_level = recode(
      yield_carbon_ratio,
      "min" = "Low",
      "max" = "High"
    ),
    leakage_level = factor(leakage_level, levels = c("Low", "High")),
    biome = recode(
      name_biome,
      "Mata Atlântica" = "Atlantic Forest",
      "Amazônia" = "Amazon"
    ),
    biome = factor(biome, levels = c("Atlantic Forest", "Amazon")),
    scenario = factor(scenario, levels = c("Ignoring leakage", "With mitigation"))
  )

if (positive_costs_only) {
  projects <- projects %>%
    filter(CO2_cost_USD > 0)
}

scenario_medians <- projects %>%
  group_by(biome, leakage_level, discount_rate, scenario) %>%
  summarise(
    median_cost = median(CO2_cost_USD, na.rm = TRUE),
    n_projects = n(),
    .groups = "drop"
  )

missing_scenarios <- scenario_medians %>%
  complete(
    biome,
    leakage_level,
    discount_rate,
    scenario = c("Ignoring leakage", "With mitigation")
  ) %>%
  filter(is.na(median_cost))

if (nrow(missing_scenarios) > 0) {
  warning(
    "Some table scenarios have no median cost after filtering. Check: ",
    paste(
      paste(
        missing_scenarios$biome,
        missing_scenarios$leakage_level,
        missing_scenarios$discount_rate,
        missing_scenarios$scenario,
        sep = " / "
      ),
      collapse = "; "
    ),
    call. = FALSE
  )
}

table_raw <- scenario_medians %>%
  select(-n_projects) %>%
  pivot_wider(
    names_from = scenario,
    values_from = median_cost
  ) %>%
  rename(
    ignoring_leakage = `Ignoring leakage`,
    with_mitigation = `With mitigation`
  ) %>%
  mutate(
    cost_increase_pct =
      ((with_mitigation - ignoring_leakage) / ignoring_leakage) * 100
  ) %>%
  group_by(biome, leakage_level) %>%
  mutate(
    central_cost_increase_pct =
      cost_increase_pct[discount_rate == central_discount_rate][1],
    change_vs_r3_pp = cost_increase_pct - central_cost_increase_pct
  ) %>%
  ungroup() %>%
  select(
    biome,
    leakage_level,
    discount_rate,
    ignoring_leakage,
    with_mitigation,
    cost_increase_pct,
    change_vs_r3_pp
  ) %>%
  arrange(biome, leakage_level, discount_rate)

table_formatted <- table_raw %>%
  mutate(
    ignoring_leakage = round(ignoring_leakage, 1),
    with_mitigation = round(with_mitigation, 1),
    cost_increase_pct = round(cost_increase_pct, 0),
    change_vs_r3_pp = round(change_vs_r3_pp, 0)
  ) %>%
  rename(
    `Biome` = biome,
    `Leakage level` = leakage_level,
    `Disc. rate (r)` = discount_rate,
    `Ignoring leakage` = ignoring_leakage,
    `With mitigation` = with_mitigation,
    `Cost increase (%)` = cost_increase_pct,
    `Change vs. ignoring leakage with r=3 (pp)` = change_vs_r3_pp
  )

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

saveRDS(table_raw, raw_output_file)
write.csv(table_formatted, formatted_output_file, row.names = FALSE)

table_formatted
