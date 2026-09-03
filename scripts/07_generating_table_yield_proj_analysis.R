#---- load packages ------------------------------------------------------------

library(dplyr)
library(purrr)
library(tibble)
library(tidyr)

#-------------------------------------------------------------------------------

results_dir <- "results_with_mitigation"
output_dir <- "output_tables"
output_file <- file.path(
  output_dir,
  "abacus_high_low_leakage_projects_country_scale_comparing_yield_model_SCC_3_USD.rds"
)

mitigation_scale <- "country"
source_params <- "Abacus"
strategy <- "optimize_carbon"
SCC_param <- "SCC_3"
round_param <- "round1"
intensification_name <- "wirsenius"

biomes <- c(
  mataatlantica = "Mata Atlântica",
  amazonia = "Amazônia"
)

yield_models <- c(
  yield_nogrow = "nogrow",
  yield_prj = "prj",
  yield_carrying = "carrying"
)

result_file <- function(biome, yield_param) {
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

result_file_variants <- function(biome, yield_param) {
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

make_co2_name <- function(yield_carbon_ratio, name_biome) {
  case_when(
    yield_carbon_ratio == "min" & name_biome == "Amazônia" ~ "co2_min_AM",
    yield_carbon_ratio == "max" & name_biome == "Amazônia" ~ "co2_max_AM",
    yield_carbon_ratio == "min" & name_biome == "Mata Atlântica" ~ "co2_min_AF",
    yield_carbon_ratio == "max" & name_biome == "Mata Atlântica" ~ "co2_max_AF"
  )
}

needs_scc_columns <- function(df) {
  required_cols <- c(
    "net_CO2_corrected_mitigation",
    "net_CO2_corrected_no_mitigation",
    "net_CO2_no_correction_no_mitigation",
    "net_CO2_no_correction_mitigation"
  )

  missing_required <- any(!required_cols %in% names(df))
  missing_required ||
    all(is.na(df$net_CO2_corrected_mitigation)) ||
    all(is.na(df$net_CO2_corrected_no_mitigation))
}

add_scc_columns <- function(df) {
  if (!needs_scc_columns(df)) {
    return(df)
  }

  SCC <- readRDS("input_tables/SCC_rcp45.rds")
  biomass_recovery <- readRDS("input_tables/biomass_recovery.rds")
  recovery_50y <- biomass_recovery %>%
    filter(years == 50) %>%
    pull(recovery)
  SCC_t0 <- 133.0843

  co2_vals <- df %>%
    group_by(yield_carbon_ratio, name_biome, coordinates) %>%
    summarise(CO2_additionality = mean(CO2_additionality, na.rm = TRUE), .groups = "drop") %>%
    mutate(name = make_co2_name(yield_carbon_ratio, name_biome))

  biomass_recovery_source <- biomass_recovery %>%
    crossing(coordinates = unique(df$coordinates)) %>%
    left_join(co2_vals, by = "coordinates") %>%
    mutate(value = (recovery / recovery_50y) * CO2_additionality) %>%
    group_by(coordinates) %>%
    mutate(Year = 2021 + years) %>%
    left_join(filter(SCC, Year <= 2080), by = "Year") %>%
    filter(!is.na(SCC_t))

  leakage_vals <- df %>%
    ungroup() %>%
    select(yield_carbon_ratio, name_biome, leaked_CO2, coordinates) %>%
    mutate(
      name = make_co2_name(yield_carbon_ratio, name_biome),
      years = 1,
      Year = 2022
    )

  biomass_agg <- biomass_recovery_source %>%
    left_join(
      leakage_vals,
      by = c("name", "years", "Year", "coordinates", "name_biome", "yield_carbon_ratio")
    ) %>%
    mutate(leaked_CO2 = if_else(is.na(leaked_CO2), 0, leaked_CO2)) %>%
    arrange(years, name) %>%
    group_by(name, coordinates) %>%
    mutate(
      additionality = value - lag(value),
      additionality = if_else(is.na(additionality), 0, additionality),
      EP = SCC_t / SCC_t0,
      co2_removal_corrected = additionality * EP,
      co2_leaked_corrected = leaked_CO2 * EP
    ) %>%
    filter(years <= 50) %>%
    summarise(
      net_CO2_corrected_mitigation = sum(co2_removal_corrected, na.rm = TRUE),
      net_CO2_corrected_no_mitigation =
        sum(co2_removal_corrected - co2_leaked_corrected, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      yield_carbon_ratio = if_else(name %in% c("co2_min_AM", "co2_min_AF"), "min", "max"),
      name_biome = case_when(
        name %in% c("co2_min_AF", "co2_max_AF") ~ "Mata Atlântica",
        TRUE ~ "Amazônia"
      )
    )

  df %>%
    select(-any_of(c(
      "net_CO2_corrected_mitigation",
      "net_CO2_corrected_no_mitigation",
      "net_CO2_no_correction_no_mitigation",
      "net_CO2_no_correction_mitigation"
    ))) %>%
    left_join(
      select(
        biomass_agg,
        yield_carbon_ratio,
        name_biome,
        coordinates,
        net_CO2_corrected_mitigation,
        net_CO2_corrected_no_mitigation
      ),
      by = c("yield_carbon_ratio", "name_biome", "coordinates")
    ) %>%
    mutate(
      net_CO2_no_correction_no_mitigation = CO2_additionality - leaked_CO2,
      net_CO2_no_correction_mitigation = CO2_additionality
    )
}

read_result_file <- function(path) {
  readRDS(path) %>%
    add_scc_columns()
}

result_files <- expand_grid(
  biome_file_name = names(biomes),
  yield_param = names(yield_models)
) %>%
  mutate(
    paths_to_try = map2(biome_file_name, yield_param, result_file_variants),
    path = map_chr(paths_to_try, ~ .x[file.exists(.x)][1]),
    yield_model = unname(yield_models[yield_param])
  )

missing_files <- result_files %>% filter(is.na(path))

if (nrow(missing_files) > 0) {
  stop(
    "Missing yield-sensitivity mitigation result file(s): ",
    paste(map_chr(missing_files$paths_to_try, paste, collapse = " or "), collapse = "; "),
    call. = FALSE
  )
}

df <- result_files %>%
  mutate(data = map(path, read_result_file)) %>%
  select(biome_file_name, yield_model, data) %>%
  unnest(data, names_repair = "unique") %>%
  mutate(
    name_biome = if_else(
      is.na(name_biome),
      unname(biomes[biome_file_name]),
      name_biome
    ),
    FP_type = factor(yield_model, levels = c("nogrow", "prj", "carrying"))
  )

# calculating total project costs in USD
IPCA_2021 <- 1.0579
IPCA_2024 <- 1.1583
IPCA_2023 <- 1.0462 * 1.0579
USD_rate_2021 <- 5.3955
USD_BR <- 5

df <- df %>%
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

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

saveRDS(df, output_file)
