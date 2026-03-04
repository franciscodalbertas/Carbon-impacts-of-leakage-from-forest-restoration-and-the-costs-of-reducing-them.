
# run mitigation cost calculation for local scale

# parameters to choose which data to run!
source_params <- "ABACUS"       # or "Verra"
strategy <- "optimize_carbon"   # "minimize_costs" or "optimize_carbon"
SCC_param <- "SCC_3" # discount rate used to calculate SCC (3%)
yield_param <- "yield_prj" # yield assumption: forecasted yield increase,
# yield at carrying capacity, no yield increase

# Load necessary script

# run mitigation costs biome scale carbon ----
source("scripts/results_calculation_single_projects/04_c_calculating_mitigation_costs_biome_project_sample_modular.R")

