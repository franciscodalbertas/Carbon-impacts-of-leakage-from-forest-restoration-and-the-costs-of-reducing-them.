# need to run for SCC_2 
# run mitigation cost calculation for country scale

# parameters to choose which data to run!
source_params <- "ABACUS"       # "Abacus" or "Verra"
strategy <- "optimize_carbon"   # "minimize_costs" or "optimize_carbon"
SCC_param <- "SCC_2" # discount rate used to calculate SCC_3 (3%) or "SCC_4" (4%) OR "SCC_2"
yield_param <- "yield_prj" # "yield_nogrow" OR "yield_prj" OR "yield_carrying"


# Load necessary script

# run mitigation costs country scale costs ------------------------------------
source("scripts/results_calculation_single_projects/05_b_calculating_mitigation_costs_country_project_sample_modular.R")

