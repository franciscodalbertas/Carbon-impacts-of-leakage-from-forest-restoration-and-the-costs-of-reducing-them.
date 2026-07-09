# Precomputed mitigation candidates for carbon optimization
# ---------------------------------------------------------
# This file supports the optimized mitigation pipeline. It builds reusable
# candidate mitigation tables for:
#   - am_af: Amazonia + Atlantic Forest, used by local/cluster and biome scales
#   - country: national domain, used by country scale
#
# Project scenarios are treated as alternatives, not simultaneous demands. Pool
# trimming therefore uses the maximum single-project FP demand in each pool.

library(dplyr)
library(sf)
library(terra)


normalize_intensification_model <- function(intensification_model = NULL,
                                            intensification_name = NULL) {
  if (identical(tolower(as.character(intensification_model)), "carlos") ||
      identical(tolower(as.character(intensification_name)), "carlos")) {
    "carlos"
  } else {
    "wirsenius"
  }
}


resolve_mitigation_input_path <- function(source_params,
                                          mitigation_scale,
                                          yield_param,
                                          SCC_param,
                                          round) {
  source_lower <- tolower(source_params)
  scale_name <- if (mitigation_scale %in% c("cluster", "local")) "local" else mitigation_scale

  names_to_try <- c(
    paste0("inputs_for_mitigation_leakage_", source_lower, "_", scale_name,
           "_scale_", yield_param, "_high_low_leakage_", SCC_param, "_", round, ".rds"),
    paste0("inputs_for_mitigation_leakage_", source_lower, "_", scale_name,
           "_scale_", yield_param, "_high_low_leakge_", round, ".rds"),
    paste0("inputs_for_mitigation_leakage_", source_lower, "_", scale_name,
           "_scale_", yield_param, "_high_low_leakage_", round, ".rds"),
    paste0("inputs_for_mitigation_leakage_", source_lower, "_", scale_name,
           "_scale_", yield_param, "_high_low_leakge_", SCC_param, "_", round, ".rds")
  )

  paths_to_try <- file.path("mitigation_inputs", names_to_try)

  existing <- paths_to_try[file.exists(paths_to_try)]

  if (length(existing) == 0) {
    stop("Could not find mitigation input for ", source_params, " at ",
         scale_name, " scale. Tried: ", paste(paths_to_try, collapse = "; "))
  }

  existing[1]
}


build_mitigation_candidates <- function(domain = c("am_af", "country"),
                                        intensification_model = "wirsenius",
                                        rate = 0.08,
                                        years = 50,
                                        min_cost = NULL) {
  domain <- match.arg(domain)
  intensification_model <- normalize_intensification_model(intensification_model)

  sr_carrying <- rast("variables/stocking_rate_carrying_capacity_albers_pj.tif")
  current_yield <- rast("variables/beef_yield_kg_ha_2021_albers_1km.tif")
  delta_y_c <- rast("variables/yield_gap_carbon_pnr_ratio_albers_1km.tif")
  delta_carbon <- rast("variables/delta_AGC_BGC_SOC_Walter_22_albers_50y_1km.tif")
  p_n_r <- rast("variables/prob_nat_regeneration_all_vegetation_albers1km_pj.tif")
  immediate_regions_r <- rast("variables/immediate_regions_r_limits_albers1km.tif")

  domain_layer <- if (domain == "am_af") {
    readRDS("input_tables/imediate_regions_am_af.rds")
  } else {
    readRDS("input_tables/br.rds")
  }

  domain_layer <- st_transform(domain_layer, crs = crs(sr_carrying))

  immediate_regions_am_af <- readRDS("input_tables/imediate_regions_am_af.rds") %>%
    st_transform(crs = crs(sr_carrying))

  pasture_share <- sum(
    rast("variables/pasture_vigor_share_lapig_2021_col8albers_pj_1000mres.tif")[[1:2]],
    na.rm = TRUE
  )
  pasture_share <- ifel(pasture_share <= 0.005, NA, pasture_share)
  pasture_area <- pasture_share * 100

  if (identical(intensification_model, "carlos")) {
    intensification_costs <- rast("variables/intensif_cost_reais-kg-ha_1st_year_low_medium_vigor_past_2022_Carlos_et_al_albers_1km.tif")
    maintenance_costs <- rast("variables/intensif_maintenance_cost_reais-kg-ha_low_medium_vigor_past_2022_Carlos_et_al_albers_1km.tif")

    if (is.null(min_cost)) {
      mitigation_costs <- read.csv("input_tables/maintenance_costs.csv")
      min_cost <- min(mitigation_costs$cost_ha_y, na.rm = TRUE)
    }

    intensification_costs <- ifel(intensification_costs < min_cost, NA, intensification_costs)
    discount_factors <- 1 / ((1 + rate) ^ seq_len(years))
    total_cost_ha <- (intensification_costs + maintenance_costs * sum(discount_factors)) * 450
    yield_gain <- NULL
  } else {
    total_cost_ha <- rast("variables/pasture_intensification_costs__to_ext_2_50years_Stefan.tif")
    yield_gain <- rast("variables/pasture_intensification_yield_gains_to_ext_annually_Stefan.tif")
  }

  raster_stack <- c(
    sr_carrying,
    current_yield,
    total_cost_ha,
    pasture_area,
    delta_y_c,
    delta_carbon,
    p_n_r
  )

  names(raster_stack) <- c(
    "sr_carrying",
    "current_yield",
    "cost_reais_ha",
    "area_available_ha",
    "delta_y_c",
    "delta_tons_ha",
    "p_n_r"
  )

  if (!is.null(yield_gain)) {
    raster_stack <- c(raster_stack, yield_gain)
    names(raster_stack)[nlyr(raster_stack)] <- "intens_gain_yield"
  }

  candidate_rasters <- crop(raster_stack, domain_layer)
  candidate_rasters <- mask(candidate_rasters, domain_layer)

  candidates <- as.data.frame(candidate_rasters, xy = TRUE, na.rm = TRUE) %>%
    rename(x_mitig = x, y_mitig = y)

  regions_c <- crop(immediate_regions_r, candidate_rasters[[1]])
  regions_m <- mask(regions_c, candidate_rasters[[1]])
  boundaries <- as.data.frame(regions_m, xy = TRUE, na.rm = TRUE)
  names(boundaries)[3] <- "code_immediate"
  boundaries <- boundaries %>%
    rename(x_mitig = x, y_mitig = y)

  candidates <- candidates %>%
    left_join(boundaries, by = c("x_mitig", "y_mitig")) %>%
    filter(!is.na(code_immediate)) %>%
    mutate(code_immediate = as.character(code_immediate))

  immediate_lookup <- immediate_regions_am_af %>%
    st_drop_geometry() %>%
    select(code_immediate, name_immediate, abbrev_state, name_biome) %>%
    mutate(code_immediate = as.character(code_immediate)) %>%
    distinct()

  animal_performance <- readRDS("input_tables/historical_yield_immediate_regions_2011_2021.rds") %>%
    filter(year == 2021) %>%
    select(cod_rgi, DA_uf) %>%
    rename(code_immediate = cod_rgi) %>%
    mutate(code_immediate = as.character(code_immediate)) %>%
    distinct()

  candidates <- candidates %>%
    left_join(immediate_lookup, by = "code_immediate") %>%
    left_join(animal_performance, by = "code_immediate") %>%
    filter(!is.na(DA_uf)) %>%
    mutate(
      yield_carrying = (sr_carrying * 0.9) * DA_uf,
      production_carrying = yield_carrying * area_available_ha,
      current_production = current_yield * area_available_ha,
      delta_production_kg = production_carrying - current_production
    ) %>%
    filter(delta_production_kg > 0)

  if (identical(intensification_model, "carlos")) {
    candidates <- candidates %>%
      mutate(
        density = current_yield / DA_uf,
        intensified_density = density + 1,
        intensified_yield = pmin(intensified_density * DA_uf, yield_carrying),
        y_gained = intensified_yield - current_yield,
        area_required_yield = intensified_yield,
        production_gain_kg = area_available_ha * (p_n_r * intensified_yield + ((1 - p_n_r) * y_gained))
      )
  } else {
    candidates <- candidates %>%
      mutate(
        y_i = pmin(intens_gain_yield + current_yield, yield_carrying),
        y_gained = y_i - current_yield,
        area_required_yield = y_i,
        production_gain_kg = area_available_ha * (p_n_r * y_i + ((1 - p_n_r) * y_gained))
      )
  }

  candidates %>%
    filter(production_gain_kg > 0)
}


candidate_table_path <- function(domain, round, intensification_name) {
  file.path(
    "mitigation_inputs",
    "precomputed_candidates",
    paste0(domain, "_candidates_", round, "_", intensification_name, ".rds")
  )
}


ensure_mitigation_candidate_tables <- function(round,
                                               intensification_model = NULL,
                                               intensification_name = NULL,
                                               rate = 0.08,
                                               overwrite = FALSE) {
  intensification_model <- normalize_intensification_model(intensification_model, intensification_name)
  intensification_name <- intensification_model
  years <- if (identical(intensification_model, "carlos")) 49 else 50

  out_dir <- file.path("mitigation_inputs", "precomputed_candidates")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  paths <- c(
    am_af = candidate_table_path("am_af", round, intensification_name),
    country = candidate_table_path("country", round, intensification_name)
  )

  for (domain in names(paths)) {
    if (overwrite || !file.exists(paths[[domain]])) {
      message("Building ", domain, " candidate table for ", intensification_name)
      candidates <- build_mitigation_candidates(
        domain = domain,
        intensification_model = intensification_model,
        rate = rate,
        years = years
      )
      saveRDS(candidates, paths[[domain]])
    }
  }

  list(
    am_af = readRDS(paths[["am_af"]]),
    country = readRDS(paths[["country"]]),
    paths = paths
  )
}


estimate_max_project_demand <- function(projects) {
  list(
    local = projects %>%
      group_by(code_immediate) %>%
      summarise(required_FP = max(total_FP_leaked, na.rm = TRUE), .groups = "drop") %>%
      mutate(code_immediate = as.character(code_immediate)),
    biome = projects %>%
      group_by(name_biome) %>%
      summarise(required_FP = max(total_FP_leaked, na.rm = TRUE), .groups = "drop"),
    country = projects %>%
      summarise(required_FP = max(total_FP_leaked, na.rm = TRUE))
  )
}


rank_candidate_pool <- function(candidates) {
  top_delta_y_c <- quantile(candidates$delta_y_c, 0.80, na.rm = TRUE)

  candidates %>%
    filter(delta_y_c <= top_delta_y_c) %>%
    arrange(desc(delta_y_c))
}


trim_ranked_pool_to_demand <- function(ranked_pool, required_FP) {
  ranked_pool <- ranked_pool %>%
    mutate(cumulative_production_kg = cumsum(production_gain_kg))

  last_index <- which(ranked_pool$cumulative_production_kg >= required_FP)[1]

  if (is.na(last_index)) {
    warning("Candidate pool does not meet required_FP.")
    last_index <- nrow(ranked_pool)
  }

  ranked_pool[seq_len(last_index), ]
}


precompute_trimmed_pools <- function(candidates_am_af,
                                     candidates_country,
                                     projects) {
  demand <- estimate_max_project_demand(projects)

  local_pools <- candidates_am_af %>%
    semi_join(demand$local, by = "code_immediate") %>%
    group_by(code_immediate) %>%
    group_split() %>%
    lapply(function(pool) {
      required_FP <- demand$local$required_FP[
        demand$local$code_immediate == pool$code_immediate[1]
      ]

      rank_candidate_pool(pool) %>%
        trim_ranked_pool_to_demand(required_FP = required_FP)
    }) %>%
    bind_rows()

  biome_pools <- candidates_am_af %>%
    semi_join(demand$biome, by = "name_biome") %>%
    group_by(name_biome) %>%
    group_split() %>%
    lapply(function(pool) {
      required_FP <- demand$biome$required_FP[
        demand$biome$name_biome == pool$name_biome[1]
      ]

      rank_candidate_pool(pool) %>%
        trim_ranked_pool_to_demand(required_FP = required_FP)
    }) %>%
    bind_rows()

  country_pool <- rank_candidate_pool(candidates_country) %>%
    trim_ranked_pool_to_demand(required_FP = demand$country$required_FP[1])

  list(
    local = local_pools,
    biome = biome_pools,
    country = country_pool,
    demand = demand
  )
}


target_fp_column <- function(project) {
  if ("total_FP_leaked" %in% names(project)) {
    project$total_FP_leaked
  } else {
    project$FP_leaked
  }
}


select_mitigation_sites_for_project <- function(trimmed_pools,
                                                project,
                                                mitigation_scale) {
  scale_key <- if (mitigation_scale %in% c("cluster", "local")) "local" else mitigation_scale
  target_FP <- target_fp_column(project)

  if (scale_key == "local") {
    pool <- trimmed_pools$local %>%
      filter(code_immediate == as.character(project$code_immediate))
  } else if (scale_key == "biome") {
    pool <- trimmed_pools$biome %>%
      filter(name_biome == project$name_biome)
  } else if (scale_key == "country") {
    pool <- trimmed_pools$country
  } else {
    stop("mitigation_scale must be 'cluster'/'local', 'biome', or 'country'")
  }

  pool <- pool %>%
    mutate(cumulative_production_kg = cumsum(production_gain_kg))

  last_index <- which(pool$cumulative_production_kg >= target_FP)[1]

  if (is.na(last_index)) {
    warning("Candidate pool does not meet target_FP for project ", project$coordinates)
    last_index <- nrow(pool)
  }

  selected <- pool[seq_len(last_index), ]

  selected %>%
    mutate(
      cumulative_production_kg = pmin(cumulative_production_kg, target_FP),
      new_target = if_else(
        row_number() == 1,
        target_FP,
        target_FP - lag(cumulative_production_kg)
      ),
      area_required = new_target / area_required_yield,
      area_required = round(if_else(area_required > area_available_ha, area_available_ha, area_required), 2),
      mitigation_costs_reais_50y = cost_reais_ha * area_required,
      forgone_carbon = delta_tons_ha * area_required,
      forgone_co2 = forgone_carbon * (44 / 12),
      forgone_co2_nat_reg = forgone_co2 * p_n_r
    )
}


summarise_project_mitigation_precomputed <- function(selected_sites,
                                                     project,
                                                     mitigation_scale,
                                                     source_params) {
  project_df <- if (inherits(project, "sf")) {
    st_drop_geometry(project)
  } else {
    as.data.frame(project)
  }

  summary <- selected_sites %>%
    summarise(
      mitigation_costs_reais_50y = sum(mitigation_costs_reais_50y, na.rm = TRUE),
      forgone_co2 = sum(forgone_co2_nat_reg, na.rm = TRUE),
      .groups = "drop"
    )

  bind_cols(
    summary,
    project_df
  ) %>%
    mutate(
      mitigation_strategy = "optimize_carbon",
      mitigation_scale = mitigation_scale,
      approach = "intensification",
      params = source_params
    )
}


run_precomputed_carbon_optimization <- function(projects,
                                                trimmed_pools,
                                                mitigation_scale,
                                                source_params) {
  project_results <- lapply(seq_len(nrow(projects)), function(i) {
    project <- projects[i, ]
    selected_sites <- select_mitigation_sites_for_project(
      trimmed_pools = trimmed_pools,
      project = project,
      mitigation_scale = mitigation_scale
    )

    summarise_project_mitigation_precomputed(
      selected_sites = selected_sites,
      project = project,
      mitigation_scale = mitigation_scale,
      source_params = source_params
    )
  })

  project_mitigation <- bind_rows(project_results)

  if (!exists("SCC_calculator", mode = "function")) {
    source("functions/13_c_SCC_mitigation_sites.R")
  }

  SCC <- SCC_calculator(input_data = project_mitigation)

  left_join(project_mitigation, SCC %>% select(2, 3))
}
