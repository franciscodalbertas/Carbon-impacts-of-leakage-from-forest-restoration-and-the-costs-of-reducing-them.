# Load packages
library(dplyr)
library(terra)
library(sf)
library(purrr)
library(furrr)

# calculate leakage consequences for the pool of projects!
source("scripts/results_calculation_single_projects/02_c_run_project_features_sample_projects.R")

#--- high and low projects ---

projects <- readRDS("project_location/project_location_round8_sampled100projects.rds")

# run analysis for country scale
source_type <- "abacus"

result <- run_leakage_analysis(source_type = "abacus", leakage_scale = "biome",projects = projects,
                               FP_type = "capped_FP")


# saving location
save_path <- paste0("mitigation_imputs/imputs_for_mitigation_leakage_",source_type,
                    "_biome_scale_yield_prj_high_low_leakge_round10.rds")

#save
saveRDS(result, save_path)

# run analysis for random located projects ---

# read projects
projects <- readRDS("project_location/project_location_round8_randomsampled100projects.rds")

# add fake yield_carbon_ratio column just to make the function run

projects$yield_carbon_ratio <- "random"
source_type = "abacus"
result2 <- run_leakage_analysis(source_type = source_type,leakage_scale = "biome",
                                projects = projects,FP_type = "capped_FP")

save_path2 <- paste0("mitigation_imputs/imputs_for_mitigation_leakage_",source_type,
                     "_biome_scale_yield_prj_random_sample_round10.rds")

# save

saveRDS(result2, save_path2)