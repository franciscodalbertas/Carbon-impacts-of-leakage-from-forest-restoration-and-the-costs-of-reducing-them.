
# run mitigation cost calculation for local scale

# parameters to choose which data to run!
source_params <- "ABACUS"       # or "Verra"
strategy <- "optimize_carbon"   # "minimize_costs" or "optimize_carbon"
SCC_param <- "SCC_3" # discount rate used to calculate SCC (3%)
yield_param <- "yield_prj" # yield assumption: forecasted yield increase,
# yield at carrying capacity, no yield increase

# yield at carrying capacity, no yield increase
intensification_model <- NULL # Or carlos if I want to run the old way!

# change how the name is created
round <- "round1"
intensification_name <- "wirsenius" # Wirsenius was used for the central analysis
# sensitivity analysis with "carlos"

# Load necessary script

source("functions/14_a_calculating_mitigation_costs_imediate_region_project_sample_modular.R")



