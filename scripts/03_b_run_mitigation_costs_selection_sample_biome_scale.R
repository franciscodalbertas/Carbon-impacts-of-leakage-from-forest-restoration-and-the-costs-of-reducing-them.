#-------------------------------------------------------
# run mitigation cost calculation for biome scale
#-------------------------------------------------------

# parameters to choose which data to run!
source_params <- "ABACUS"       # ABACUS (leakage=100%)or Verra (leakage=30%)
strategy <- "optimize_carbon"   # "minimize_costs" or "optimize_carbon"
SCC_param <- "SCC_3" # discount rate used to calculate SCC (SCC_3, SCC_2, or SCC_4 for 3-4%)
yield_param <- "yield_prj" # yield assumption: forecasted yield increase ("yield_prj"),
# yield at carrying capacity ("yield_carrying"), no yield increase ("yield_nogrow")

intensification_model <- NULL # default will run Wirsenius model, or "Carlos"

# change how the name is created
round <- "round1"
intensification_name <- "wirsenius" # Wirsenius was used for the central analysis
# sensitivity analysis with "carlos"

# run mitigation cost calculation
source("functions/14_b_calculating_mitigation_costs_biome_project_sample_modular.R")

