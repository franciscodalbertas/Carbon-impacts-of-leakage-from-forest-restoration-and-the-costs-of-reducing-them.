## ======================= USER-DEFINED FLAGS =========================
# These must be set in the parent script BEFORE sourcing this one:
# source_params <- "ABACUS" or "Verra"
# strategy <- "minimize_costs" or "optimize_carbon"

if (!exists("source_params") || !exists("strategy")) {
  stop("You must define `source_params` and `strategy` before sourcing this script.")
}
message(glue::glue("Running with: source = {source_params}, strategy = {strategy}"))

# ======================= LOAD PACKAGES & FUNCTIONS ==================
library(terra)
library(dplyr)
library(sf)
library(purrr)
library(glue)
library(stringi)

source("functions/06_mitigation_scale.R")
source("functions/04_calculat_mitigation_minimizing_costs.R")

# Carlos et al:
if(identical(intensification_model, "carlos")){
source("functions/05_a_calculat_mitigation_carbon_efficiency_with_nat_reg.R")
  }else{
# Wirsenius et al:
source("functions/05_b_calculat_mitigation_carbon_efficiency_with_nat_reg_stefan_model.R")
}
source("functions/09_preprocessing_rasters.R")

# ======================= PREPROCESS DATA ============================

#library(glue)

# Initialize
base_path <- "mitigation_inputs/inputs_for_mitigation_leakage_"
filename <- NULL

# Only proceed if source_params is one of the expected values
if (source_params %in% c("ABACUS", "Verra")) {
  
  # Start filename. Prefer the SCC-tagged name used by the mitigation pipeline,
  # but fall back to the current leakage-input name if the SCC file is not present.
  filename <- paste0(base_path,tolower(source_params),"_local_scale_",yield_param,
                     "_high_low_leakage_",SCC_param,"_",round,".rds")

  fallback_filename <- paste0(base_path,tolower(source_params),"_local_scale_",yield_param,
                              "_high_low_leakge_",round,".rds")

  if (!file.exists(filename) && file.exists(fallback_filename)) {
    filename <- fallback_filename
  }
  
  # Assign path to correct input
  path_verra <- if (source_params == "Verra") filename else NULL
  path_abacus <- if (source_params == "ABACUS") filename else NULL
  
} else {
  stop("source_params must be 'Verra' or 'ABACUS'")
}

# Call the function
data <- preprocess_data(
  path_verra = path_verra,
  path_abacus = path_abacus
)


list2env(data, .GlobalEnv)

rate <- 0.08
years <- if (identical(intensification_model, "carlos")) 49 else 50
# change if there's updates in project location
proj_location_path <- if (file.exists("project_location/project_location_round1_sampled1000projects.rds")) {
  "project_location/project_location_round1_sampled1000projects.rds"
} else {
  "project_location/project_location_round1_sampled100projects.rds"
}

proj_location <- readRDS(proj_location_path)
immediate_regions <- rast("variables/immediate_regions_r_limits_albers1km.tif")

# change cost input

intensification_costs_input <- if (identical(intensification_model, "carlos")) {
  intensification_costs
} else {
  intensification_costs_stefan
}


input_cluster_scale <- project_scale(
  projects = get(glue("projects_{tolower(source_params)}_cluster")),
  scale = "cluster",
  pot_yield = pot_yield,
  intensification_costs = intensification_costs_input, 
  pasture_share = pasture_share,
  maintenance_costs = maintenance_costs,
  rate = rate,
  years = years,
  delta_carbon = delta_carbon,
  min_cost = min_cost,
  current_yield = current_yield,
  delta_y_c = delta_y_c,
  intensification_model = intensification_model
)

sf_object <- st_transform(proj_location, crs = crs(input_cluster_scale[[1]]))
raster_mask <- rasterize(sf_object, input_cluster_scale[[1]], field = 1, background = 0)

input_cluster_scale[c(1, 2, 3, 8)] <- lapply(input_cluster_scale[c(1, 2, 3, 8)], function(obj) {
  if (inherits(obj, "SpatRaster")) mask(obj, raster_mask, maskvalue = 1, updatevalue = NA) else obj
})

immediate_regions_c <- crop(immediate_regions, input_cluster_scale$sr_carry)
immediate_regions_m <- mask(immediate_regions_c, input_cluster_scale$sr_carry)

# ======================= DEFINE PROJECTS =============================
projects_cluster <- get(glue("projects_{tolower(source_params)}_cluster"))
biomes <- c("Amazônia", "Mata Atlântica")

for (biome in biomes) {
  message(glue("Processing biome: {biome}"))
  
  projects_min <- projects_cluster %>%
    filter(yield_carbon_ratio == "min", name_biome == biome)
  projects_max <- projects_cluster %>%
    filter(yield_carbon_ratio == "max", name_biome == biome)
  
  stopifnot(nrow(projects_min) == nrow(projects_max))
  
  mitigation_list <- purrr::map2(
    .x = split(projects_min, seq_len(nrow(projects_min))),
    .y = split(projects_max, seq_len(nrow(projects_max))),
    .f = function(min_proj, max_proj) {
      projects <- bind_rows(min_proj, max_proj)
      
      if (strategy == "minimize_costs") {
        minimize_costs(
          sr_carrying = input_cluster_scale$sr_carry,
          total_cost_ha = input_cluster_scale$total_cost_ha,
          pasture_area = input_cluster_scale$pasture_area,
          mitigation_scale = "cluster",
          source_params = source_params,
          target_FP_1 = max_proj$total_FP_leaked,
          target_FP_2 = min_proj$total_FP_leaked,
          projects = projects,
          scale = immediate_regions_m,
          boundary_1 = max_proj$code_immediate,
          boundary_2 = min_proj$code_immediate,
          biome = biome,
          current_yield = input_cluster_scale$current_yield
        )
      } else if (strategy == "optimize_carbon") {
        carbon_optimization(
          sr_carrying = input_cluster_scale$sr_carry,
          total_cost_ha = input_cluster_scale$total_cost_ha,
          pasture_area = input_cluster_scale$pasture_area,
          mitigation_scale = "cluster",
          source_params = source_params,
          target_FP_1 = max_proj$total_FP_leaked,
          target_FP_2 = min_proj$total_FP_leaked,
          projects = projects,
          scale = immediate_regions_m,
          boundary_1 = max_proj$code_immediate,
          boundary_2 = min_proj$code_immediate,
          biome = biome,
          current_yield = input_cluster_scale$current_yield,
          delta_y_c = input_cluster_scale$delta_y_c,
          p_n_r = input_cluster_scale$p_n_r
        )
      } else {
        stop("Invalid strategy: choose 'minimize_costs' or 'optimize_carbon'")
      }
    }
  )
  
  mitigation_combined <- bind_rows(mitigation_list)
  clean_biome <- stringi::stri_trans_general(biome, "Latin-ASCII") %>%
    tolower() %>%
    gsub(" ", "", .)
  
  save_name <- glue("results_with_mitigation/mitigation_costs_local_scale_intensification_{strategy}_{source_params}_{clean_biome}_{yield_param}_{SCC_param}_{round}_{intensification_name}.rds")
  saveRDS(mitigation_combined, save_name)
  message(glue("Saved: {save_name}"))
}
