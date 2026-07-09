## ======================= USER-DEFINED FLAGS =========================
# These must be set in the parent script BEFORE sourcing this one:
# source_params <- "ABACUS" or "Verra"
# strategy <- "optimize_carbon"
# mitigation_scale <- "cluster"/"local", "biome", or "country"

if (!exists("source_params") || !exists("strategy") || !exists("mitigation_scale")) {
  stop("You must define `source_params`, `strategy`, and `mitigation_scale` before sourcing this script.")
}

if (!identical(strategy, "optimize_carbon")) {
  stop("The precomputed candidate pipeline currently supports only strategy = 'optimize_carbon'.")
}

library(dplyr)
library(glue)
library(purrr)
library(sf)
library(stringi)

source("functions/15_mitigation_precomputed_candidates.R")
source("functions/13_c_SCC_mitigation_sites.R")

drop_sfc_columns <- function(x) {
  x_list <- unclass(x)
  sfc_cols <- vapply(x_list, inherits, logical(1), what = "sfc")
  x_list[sfc_cols] <- NULL
  tibble::as_tibble(x_list)
}

message(glue("Running precomputed mitigation with: source = {source_params}, scale = {mitigation_scale}, intensification = {intensification_name}"))

input_path <- resolve_mitigation_input_path(
  source_params = source_params,
  mitigation_scale = mitigation_scale,
  yield_param = yield_param,
  SCC_param = SCC_param,
  round = round
)

projects_input <- readRDS(input_path)

candidate_tables <- ensure_mitigation_candidate_tables(
  round = round,
  intensification_model = intensification_model,
  intensification_name = intensification_name
)

trimmed_pools <- precompute_trimmed_pools(
  candidates_am_af = candidate_tables$am_af,
  candidates_country = candidate_tables$country,
  projects = projects_input
)

scale_name <- if (mitigation_scale %in% c("cluster", "local")) "local" else mitigation_scale
biomes <- sort(unique(projects_input$name_biome))

for (biome in biomes) {
  message(glue("Processing biome: {biome}"))

  projects_biome <- projects_input %>%
    filter(name_biome == biome)

  mitigation_combined <- run_precomputed_carbon_optimization(
    projects = projects_biome,
    trimmed_pools = trimmed_pools,
    mitigation_scale = mitigation_scale,
    source_params = source_params
  )

  clean_biome <- stringi::stri_trans_general(biome, "Latin-ASCII") %>%
    tolower() %>%
    gsub(" ", "", .)

  save_name <- glue("results_with_mitigation/mitigation_costs_{scale_name}_scale_intensification_{strategy}_{source_params}_{clean_biome}_{yield_param}_{SCC_param}_{round}_{intensification_name}.rds")
  mitigation_combined <- drop_sfc_columns(mitigation_combined)
  saveRDS(mitigation_combined, save_name)
  message(glue("Saved: {save_name}"))
}
