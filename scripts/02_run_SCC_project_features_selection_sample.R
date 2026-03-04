# falta rodar isso aqui pro l=30%, SCC=3%


# running for cluster scale
scale <- c("local")     # run for multiple scales (options: "local","biome","country")

source_to_read <- "abacus" # "abacus" or "verra" or "both"

# define discount rate for SCC

SCC_rate <- 0.03  # Options: 0.03, 0.02, 0.04 


# define yield projection type
yield_projection <- "yield_prj"  # Options: "yield_nogrow", "yield_prj", "yield_carrying"

# for low and high leakage sites -----------------------------------------------

source("functions/13_a_SCC_sample_projects.R", local = TRUE)

# for random sites -------------------------------------------------------------

source("functions/13_b_SCC_random_projects.R")
