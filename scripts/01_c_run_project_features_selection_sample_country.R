# Load packages
library(dplyr)
library(terra)
library(sf)
library(purrr)
library(furrr)

# calculate leakage consequences for the pool of projects!
source("scripts/results_calculation_single_projects/02_c_run_project_features_sample_projects.R")

#--- high and low projects -----------------------------------------------------

#projects <- readRDS("project_location/project_location_round8_sampled100projects.rds")

# run analysis for country scale
#source_type <- "abacus" # verra or abacus!!

# type of yield projection: 
#capped_FP" =  projecting future trends 
# "FP_carrying_capacity" # all grow
# or "FP_zero_growth"!! # none grow

#FP_type = "capped_FP"; yield_name = "yield_prj"
#FP_type = "FP_zero_growth"; yield_name = "yield_nogrow"
#FP_type = "FP_carrying_capacity"; yield_name = "yield_carrying"


#result <- run_leakage_analysis(source_type = source_type, leakage_scale = "country",projects = projects,
#                     FP_type = FP_type)


# saving location (this need to take into account the yield projection!!)



#save_path <- paste0("mitigation_imputs/imputs_for_mitigation_leakage_",source_type,
#                    "_country_scale_",yield_name,"_high_low_leakge_round10.rds")
#save
#saveRDS(result, save_path)

# run analysis for random located projects -------------------------------------

# read projects
projects <- readRDS("project_location/project_location_round8_randomsampled100projects.rds")
# 
# # add fake yield_carbon_ratio column just to make the function run

projects$yield_carbon_ratio <- "random"
source_type = "verra"
result2 <- run_leakage_analysis(source_type = source_type,leakage_scale = "country",
                                projects = projects,FP_type = "capped_FP")
# 
save_path2 <- paste0("mitigation_imputs/imputs_for_mitigation_leakage_",source_type,
                      "_country_scale_yield_prj_random_sample_round10.rds")
# 
#save

saveRDS(result2, save_path2)