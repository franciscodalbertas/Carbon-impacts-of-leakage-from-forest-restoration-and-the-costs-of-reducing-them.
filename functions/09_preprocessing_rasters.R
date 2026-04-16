library(terra)
library(dplyr)
library(tidyr)

preprocess_data <- function(path_verra = NULL, path_abacus = NULL) {
  
  use_path_verra <- function(filename) {
    if (is.null(path_verra)) filename else file.path(path_verra)
  }
  
  use_path_abacus <- function(filename) {
    if (is.null(path_abacus)) filename else file.path(path_abacus)
  }
  
  
  # Load necessary rasters
  pot_yield <- rast("variables/stocking_rate_carrying_capacity_albers_pj.tif")
  current_yield <- rast("variables/beef_yield_kg_ha_2021_albers_1km.tif")
  
  ## Calculate yield gap to carbon ratio
  delta_yield_c <- rast("variables/yield_gap_carbon_pnr_ratio_albers_1km.tif")
  
  selected_projects <- readRDS("project_location/project_location_round1_sampled100projects.rds")
  
  # get coordinates
  
  coords_pj <- selected_projects %>%
    sf:::st_drop_geometry() %>%
    mutate(coord_pairs = strsplit(coordinates, ";")) %>%
    unnest(coord_pairs) %>%
    separate(coord_pairs, into = c("x", "y"), sep = ",", convert = TRUE)
  
  
  # extract y and carbon, and then calculate y/c ratio
  
  
  # Extract raster values at point locations (was 24,25)
  values_df <- terra::extract(delta_yield_c, coords_pj[,c(23,24)])
  
  # Join the extracted values back to original df
  selected_projects_df <- bind_cols(coords_pj, values_df[, -1]) 
  
  names(selected_projects_df)[25] <- "delta_yield_c" 
  
  bottom <- min(selected_projects_df$delta_yield_c,na.rm = T)
  top <-  max(selected_projects_df$delta_yield_c,na.rm = T)
  
  delta_yield_c <- ifel(delta_yield_c<bottom,NA,delta_yield_c)
  delta_yield_c <- ifel(delta_yield_c>top,NA,delta_yield_c)
  
  # Load and process pasture coverage: only consider degraded pastures!!
  pasture_share <- sum(rast("variables/pasture_vigor_share_lapig_2021_col8albers_pj_1000mres.tif")[[1:2]], na.rm = TRUE)
  pasture_share <- ifel(pasture_share <= 0.005, NA, pasture_share)
  
  # Load mitigation costs and calculate cost per kg
  mitigation_costs <- read.csv("input_tables/maintenance_costs.csv") 
  
  min_cost <- min(mitigation_costs$cost_ha_y, na.rm = TRUE)
  
  # Load intensification and maintenance costs (here I can add a conditional to choose 
  # which data to use!!). Below Carlos et al., data:
  
  intensification_costs <- rast("variables/intensif_cost_reais-kg-ha_1st_year_low_medium_vigor_past_2022_Carlos_et_al_albers_1km.tif")
  maintenance_costs <- rast("variables/intensif_maintenance_cost_reais-kg-ha_low_medium_vigor_past_2022_Carlos_et_al_albers_1km.tif")
  
  # I can probably just open the 3  option
  
  intensification_costs_stefan <- rast("variables/pasture_intensification_costs__to_ext_2_50years_Stefan.tif")
  
  # Load carbon data and calculate yield-carbon ratio (future optimization)
  delta_carbon <- rast("variables/delta_AGC_BGC_SOC_Walter_22_albers_50y_1km.tif")
  
  # Load project data (this changes with scale but is only used if no project info is provided)
  read_rds_or_null <- function(path) {
    if (file.exists(path)) readRDS(path) else NULL
  }
  
  projects_verra_cluster <- read_rds_or_null(
    use_path_verra("mitigation_inputs/inputs_for_mitigation_leakage_Verra_local_scale_yield_prj_high_low_leakage_SCC3_round1.rds")
  )
  
  projects_abacus_cluster <- read_rds_or_null(
    use_path_abacus("mitigation_inputs/inputs_for_mitigation_leakage_abacus_local_scale_yield_prj_high_low_leakage_SCC3_round1.rds")
  )
  
  projects_verra_biome <- read_rds_or_null(
    use_path_verra("inputs_for_mitigation_leakage_Verra_biome_scale_yield_prj_high_low_leakage_SCC3_round1.rds")
  )
  
  projects_abacus_biome <- read_rds_or_null(
    use_path_abacus("inputs_for_mitigation_leakage_abacus_biome_scale_yield_prj_high_low_leakage_SCC3_round1.rds")
  )
  
  projects_verra_country <- read_rds_or_null(
    use_path_verra("inputs_for_mitigation_leakage_Verra_country_scale_yield_prj_high_low_leakage_SCC3_round1.rds")
  )
  
  projects_abacus_country <- read_rds_or_null(
    use_path_abacus("inputs_for_mitigation_leakage_abacus_country_scale_yield_prj_high_low_leakage_SCC3_round1.rds")
  )
  # Return all preprocessed objects as a named list
  list(
    pot_yield = pot_yield,
    current_yield = current_yield,
    sr_carrying = pot_yield,
    delta_y_c =  delta_yield_c,
    pasture_share = pasture_share,
    mitigation_costs = mitigation_costs,
    intensification_costs_stefan = intensification_costs_stefan,
    min_cost = min_cost,
    intensification_costs = intensification_costs,
    maintenance_costs = maintenance_costs,
    delta_carbon = delta_carbon,
    projects_verra_cluster = projects_verra_cluster,
    projects_abacus_cluster = projects_abacus_cluster,
    projects_verra_biome = projects_verra_biome,
    projects_abacus_biome = projects_abacus_biome,
    projects_verra_country = projects_verra_country,
    projects_abacus_country = projects_abacus_country
  )
}
