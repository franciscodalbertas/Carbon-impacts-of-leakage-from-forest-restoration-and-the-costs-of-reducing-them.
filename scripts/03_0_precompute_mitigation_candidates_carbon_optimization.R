#-------------------------------------------------------
# precompute mitigation candidate tables for carbon optimization
#-------------------------------------------------------
# This builds the reusable candidate tables used by the optimized mitigation
# pipeline:
#   - am_af candidates for local/cluster and biome scales
#   - country candidates for national scale
#
# Run this once after raster/input updates. The scale-specific scripts will also
# build missing tables automatically, but this makes the heavy step explicit.

# load packages ----------------------------------------------------------------

library(dplyr)
library(sf)
library(terra)

source("functions/15_mitigation_precomputed_candidates.R")

#-------------------------------------------------------------------------------
round <- "round1"
models <- c("wirsenius", "carlos")

for (model in models) {
  message("Precomputing candidate tables for: ", model)

  ensure_mitigation_candidate_tables(
    round = round,
    intensification_model = model,
    intensification_name = model,
    overwrite = FALSE
  )
}
