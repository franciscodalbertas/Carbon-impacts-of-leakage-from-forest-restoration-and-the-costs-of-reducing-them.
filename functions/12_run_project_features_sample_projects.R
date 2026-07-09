run_leakage_analysis <- function(source_type = NULL, leakage_scale = NULL, projects = NULL,
                                 FP_type = NULL  ) {
  # Load packages
  library(dplyr)
  library(terra)
  library(sf)
  library(purrr)
  library(furrr)
  
  # Load functions
  source("functions/00_a_calculating_baseline_production_varying_pasture_area.R")
  source("functions/01_calculating_delta_carbon.R")
  source("functions/02_project_cost.R")
  source("functions/03_calculate_leakage_new_land.R")
  source("functions/08_c_spatial_leakage_add_pNat.R")
  
  # Load data
  # Load default projects if not provided
  if (is.null(projects)) {
    projects <- readRDS("project_location/project_location_round8_sampled100projects.rds")
  }
  
  # Add pasture area
  pasture_share <- rast("variables/pasture_coverage_lapig_2021_col8albers_pj_1000mres.tif")
  projects$pasture_area_ha <- terra::extract(pasture_share, projects)[,2] * 100
  
  # Add stocking rate
  src <- rast("variables/stocking_rate_carrying_capacity_albers_pj.tif")
  projects$max_sr <- terra::extract(src, projects)[,2]
  projects$max_sr_capped <- projects$max_sr * 0.9 # Capping at 90% of the maximum stocking rate
  
  # Add animal performance
  ap <- readRDS("input_tables/historical_yield_immediate_regions_2011_2021.rds") %>%
    filter(year == 2021) %>%
    mutate(cod_rgi = as.double(cod_rgi))
  
  projects <- projects %>%
    left_join(ap[, c(1, 21)], by = join_by("code_immediate" == "cod_rgi")) %>%
    mutate(
      max_yield = max_sr_capped * DA_uf,
      max_production = max_yield * pasture_area_ha,
      FP_zero_growth = beef_yield_kg_ha_2021* pasture_area_ha,
    )
  
  # Baseline production
  projects_fp <- calc_baseline_production(
    project = projects,
    project_age = 50,
    H = 1,
    rate = "linear"
  ) %>%
    mutate(capped_FP = if_else(FP > max_production, max_production, FP))
  
  # Carbon delta
  projects_deltaC <- calc_delta_c(projects_fp)
  
  # Project costs
  projects_costs <- calc_project_cost(project = projects_deltaC)
  
  # Get leakage parameters (I can add new values here!)
  get_leakage_params <- function(source) {
    switch(source,
           "verra" = list(IS = 0.75, NL = 0.4),
           "abacus" = list(IS = 1, NL = 1),
           stop("Invalid source type. Use 'verra' or 'abacus'.")
    )
  }
  
  params <- get_leakage_params(source_type)
  
  # Leakage deforestation (for now, this only calculates AGB leaked. I need
  # to add SOC and BGB)...this is also a function i can prob. optimize!
  projects_leakage <- calc_leakage_deforestation(
    project = projects_costs,
    IS = params$IS,
    NL = params$NL,
    FP = FP_type #  "capped_FP" or "FP_carrying_capacity" or "FP_zero_growth"!!
  )
  
  
  nat_reg <- rast("variables/prob_nat_regeneration_all_vegetation_albers1km_pj.tif")
  projects_leakage$prob_nat_regeneration <- terra::extract(nat_reg, projects_leakage)[,2]

  
  run_spatial_leakage <- function(projects_data, scale) {
    spatial_leakage(projects_data, scale = scale)
  }
  
  spatial_leak <- run_spatial_leakage(projects_leakage, leakage_scale)
  
  projects_agg <- projects_leakage %>%
      mutate(
        # Calculate additionality based on delta_tons_ha and pasture_area_ha
        additionality = (delta_tons_ha * 44 / 12) * pasture_area_ha
            ) %>%
      group_by(code_immediate, yield_carbon_ratio, name_biome,
               name_immediate, abbrev_state, name_region,
               leakage_scale, coordinates) %>%
      summarise(
        total_restoration_costs_reais_2023 = sum(total_restoration_costs_reais_2023),
        total_land_value_reais_ha = sum(total_land_value_reais_ha),
        FP_leaked = sum(FP_leaked), 
        CO2_additionality = sum(additionality),
        # add probability of natural regeneration (can use later for yield and carbon)
        mean_prob_nat_reg = mean(prob_nat_regeneration, na.rm = TRUE),
        .groups = "drop"
      )
 # }
  
  # Merge
  final_result <- left_join(
    projects_agg,
    spatial_leak,
    by = c("code_immediate", "yield_carbon_ratio", "name_biome", "name_immediate", "coordinates")
  ) %>%
    st_drop_geometry()
  
  final_result$leakage_scale <- leakage_scale
  final_result$FP_type <- FP_type
  
  # add return instead!
  return(final_result)
  
}
