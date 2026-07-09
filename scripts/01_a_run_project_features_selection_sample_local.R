# --- Load packages ---
library(dplyr)
library(terra)
library(sf)
library(purrr)
library(furrr)

# calculate leakage consequences for the pool of projects
source("functions/12_run_project_features_sample_projects.R")

#--- high and low projects ---

# read projects
projects <- readRDS("project_location/project_location_round1_sampled1000projects.rds")

# Explanations on the parameters
# source_type = "abacus(100%)" or "verra(30%)"
# leakage_scale = "country", "biome", or "cluster"
# FP_type = "capped_FP" - projected yield based on historical trends, capped at carrying capacity
# "FP_carrying" - all projects achieve their carrying capacity
# or "FP_zero_growth" - no yield growth, just use current yield for FP

# run analysis for local scale
# abacus

# run leakage analysis for abacus projects at cluster scale
source_type = "verra"

result <- run_leakage_analysis(source_type = source_type, leakage_scale = "cluster",
                     projects = projects,FP_type = "capped_FP")

# saving location
save_path <- paste0("mitigation_inputs/inputs_for_mitigation_leakage_",source_type,
                  "_local_scale_yield_prj_high_low_leakge_round1.rds")


saveRDS(result, save_path)

# --- run analysis for random located projects ----


# read projects
projects <- readRDS("project_location/project_location_round1_randomsampled1000projects.rds")

# add fake yield_carbon_ratio column just to make the function run
projects$yield_carbon_ratio <- "random"
source_type = "verra"
result2 <- run_leakage_analysis(source_type = source_type,leakage_scale = "cluster",
                    projects = projects,FP_type = "capped_FP")

save_path2 <- paste0("mitigation_inputs/inputs_for_mitigation_leakage_",source_type,
                    "_local_scale_yield_prj_random_sample_round1.rds")

# save

saveRDS(result2, save_path2)
