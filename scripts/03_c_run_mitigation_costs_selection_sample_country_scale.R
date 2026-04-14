#-------------------------------------------------------
# run mitigation cost calculation for biome scale
#-------------------------------------------------------

# parameters to choose which data to run!
source_params <- "ABACUS"       # "Abacus" or "Verra"
strategy <- "optimize_carbon"   # "minimize_costs" or "optimize_carbon"
SCC_param <- "SCC_3" # discount rate used to calculate SCC_3 (3%) or "SCC_4" (4%) OR "SCC_2"
yield_param <- "yield_prj" # "yield_nogrow" OR "yield_prj" OR "yield_carrying"

intensification_model <- NULL # default will run Wirsenius model, or "Carlos"

# change how the name is created
round <- "round1"
intensification_name <- "wirsenius" # Wirsenius was used for the central analysis
# sensitivity analysis with "carlos"

# run mitigation cost calculation
# source("functions/14_a_calculating_mitigation_costs_imediate_region_project_sample_modular.R")


