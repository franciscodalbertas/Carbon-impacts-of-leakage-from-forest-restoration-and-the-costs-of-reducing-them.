# Adjusting carbon balance according to SCC
# Handles leakage timescale vs sequestration with scale and data source flexibility

library(tidyr)
library(dplyr)

drop_sfc_columns <- function(x) {
  x_list <- unclass(x)
  sfc_cols <- vapply(x_list, inherits, logical(1), what = "sfc")
  x_list[sfc_cols] <- NULL
  tibble::as_tibble(x_list)
}

# Allow override from master script
if (!exists("scale")) scale <- c("cluster", "biome", "country") # check here its local, not cluster
if (!exists("source_to_read")) source_to_read <- "both"  # Options: "verra", "abacus", "both"

# Read shared inputs
#SCC <- readRDS("input_tables/SCC_rcp45.rds")
if(!exists("SCC_rate")) {
  SCC <- readRDS("input_tables/SCC_rcp45.rds")
  scc_file_name <- "3"
  SCC_t0 = 133.08431
}else{
  
  SCC <- readRDS("input_tables/SCC_rcp45_sensibility.rds")
  
  if (SCC_rate == 0.02) {
    SCC <- SCC %>% select(Year, SCC_t_2) %>% rename(SCC_t = SCC_t_2)
    scc_file_name <- "2"
    # define value in t0
    SCC_t0 = 636.5159
  } else if (SCC_rate == 0.03) {
    SCC <- SCC %>% select(Year, SCC_t_3) %>% rename(SCC_t = SCC_t_3)
    scc_file_name <- "3"
    # define value in t0
    SCC_t0 = 133.08431
  } else if (SCC_rate == 0.04) {
    SCC <- SCC %>% select(Year, SCC_t_4) %>% rename(SCC_t = SCC_t_4)
    scc_file_name <- "4"
    # define value in t0
    SCC_t0 = 68.22638
  } else {
    stop("Invalid SCC_rate specified. Choose from: 0.02, 0.03, or 0.04.")
  

  }

}
biomass_recovery <- readRDS("input_tables/biomass_recovery.rds")
recovery_50y <- biomass_recovery %>% filter(years == 50) %>% pull(recovery)
biomass_recovery_base <- biomass_recovery

source_options <- c("verra", "abacus")
if (!source_to_read %in% c(source_options, "both")) {
  stop("Invalid source_to_read specified. Choose from: 'verra', 'abacus', or 'both'.")
}

sources_to_run <- if (source_to_read == "both") source_options else source_to_read

source_labels <- c(verra = "Verra", abacus = "Abacus")

make_co2_name <- function(yield_carbon_ratio, name_biome) {
  case_when(
    yield_carbon_ratio == "min" & name_biome == "Amazônia" ~ "co2_min_AM",
    yield_carbon_ratio == "max" & name_biome == "Amazônia" ~ "co2_max_AM",
    yield_carbon_ratio == "min" & name_biome == "Mata Atlântica" ~ "co2_min_AF",
    yield_carbon_ratio == "max" & name_biome == "Mata Atlântica" ~ "co2_max_AF"
  )
}

process_source <- function(source_name, scale_name) {
  source_label <- unname(source_labels[source_name])
  input_path <- paste0("mitigation_inputs/inputs_for_mitigation_leakage_", source_name,
                       "_", scale_name, "_scale_", yield_projection,
                       "_high_low_leakge_round1.rds")
  output_path <- paste0("mitigation_inputs/inputs_for_mitigation_leakage_", source_name,
                        "_", scale_name, "_scale_", yield_projection,
                        "_high_low_leakage_SCC_", scc_file_name, "_round1.rds")
  
  project_data <- drop_sfc_columns(readRDS(input_path))
  project_id <- unique(project_data$coordinates)
  
  co2_vals <- project_data %>%
    group_by(yield_carbon_ratio, name_biome, coordinates) %>%
    summarise(CO2_additionality = mean(CO2_additionality, na.rm = TRUE), .groups = "drop") %>%
    mutate(name = make_co2_name(yield_carbon_ratio, name_biome))
  
  biomass_recovery_source <- biomass_recovery_base %>%
    crossing(coordinates = project_id) %>%
    left_join(co2_vals, by = "coordinates") %>%
    mutate(value = (recovery / recovery_50y) * CO2_additionality) %>%
    group_by(coordinates) %>%
    mutate(Year = 2021 + years) %>%
    left_join(filter(SCC, Year <= 2080), by = "Year") %>%
    filter(!is.na(SCC_t))
  
  leakage_vals <- project_data %>%
    ungroup() %>%
    select(yield_carbon_ratio, name_biome, leaked_CO2, coordinates) %>%
    mutate(
      param = source_label,
      name = make_co2_name(yield_carbon_ratio, name_biome),
      years = 1,
      Year = 2022
    )
  
  biomass_recovery2 <- biomass_recovery_source %>%
    left_join(
      leakage_vals,
      by = c("name", "years", "Year", "coordinates", "name_biome", "yield_carbon_ratio")
    ) %>%
    mutate(
      param = source_label,
      leaked_CO2 = if_else(is.na(leaked_CO2), 0, leaked_CO2)
    ) %>%
    arrange(years, name, param) %>%
    group_by(name, param,coordinates) %>%
    mutate(additionality = value - lag(value),
           additionality = if_else(is.na(additionality), 0, additionality)) %>%
    mutate(
      net_CO2_mitigation = additionality,
      net_CO2_no_mitigation = additionality - leaked_CO2,
      EP = SCC_t / SCC_t0,  # added an object with correct scc at t0!
      co2_removal_corrected = additionality * EP,
      co2_leaked_corrected = leaked_CO2 * EP
    ) %>%
    filter(years <= 50) %>%
    mutate(
      net_CO2_corrected_mitigation = co2_removal_corrected,
      net_CO2_corrected_no_mitigation = co2_removal_corrected - co2_leaked_corrected
    )
  
  biomass_agg <- biomass_recovery2 %>%
    group_by(name, param, coordinates) %>%
    summarise(
      net_CO2_corrected_mitigation = sum(net_CO2_corrected_mitigation, na.rm = TRUE),
      net_CO2_corrected_no_mitigation = sum(net_CO2_corrected_no_mitigation, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      yield_carbon_ratio = if_else(name %in% c("co2_min_AM", "co2_min_AF"), "min", "max"),
      name_biome = case_when(
        name %in% c("co2_min_AF", "co2_max_AF") ~ "Mata Atlântica",
        TRUE ~ "Amazônia"
      )
    )
  
  project_data_scc <- left_join(
    project_data,
    filter(biomass_agg, param == source_label),
    by = c("yield_carbon_ratio", "name_biome", "coordinates")
  ) %>%
    select(-any_of("name")) %>%
    mutate(
      net_CO2_no_correction_no_mitigation = CO2_additionality - leaked_CO2,
      net_CO2_no_correction_mitigation = CO2_additionality
    )
  
  saveRDS(project_data_scc, output_path)
}

for (i in seq_along(scale)) {
  for (source_name in sources_to_run) {
    process_source(source_name, scale[i])
  }
}
