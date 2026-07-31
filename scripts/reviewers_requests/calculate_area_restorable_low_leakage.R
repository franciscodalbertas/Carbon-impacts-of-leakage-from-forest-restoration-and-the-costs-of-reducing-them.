# load packages ----------------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)
source("functions/00_a_calculating_baseline_production_varying_pasture_area.R")
  
#-------------------------------------------------------------------------------

# load all projects with bins

projects <- readRDS("input_tables/all_projects_with_deciles.rds")


# select favourable bin and calculate total area restorable. I will then
# need to calculate the forgone production to be mitigated and see if I can
# meet it!!

# this would require me to choose a leakage parameter...do it at 100% as it is
# the most conservative assumption

low_leakage <- projects %>%
  filter(percentile_bin=="[20,30)")

length(unique(low_leakage$coordinates)) # 15mil projects!
# convert into coordinates

# get coordinates

low_leakage_pj <- low_leakage %>%
  mutate(coord_pairs = strsplit(coordinates, ";")) %>%
  unnest(coord_pairs) %>%
  separate(coord_pairs, into = c("x", "y"), sep = ",", convert = TRUE)



# drop project variables!!

low_leakage_pj <- low_leakage_pj %>%
  select(-c(16:20))

# Add pasture area
pasture_share <- rast("variables/pasture_coverage_lapig_2021_col8albers_pj_1000mres.tif")

# convert in sf

low_leakage_pj <- vect(low_leakage_pj,geom = c("x","y"),crs = crs(pasture_share))

low_leakage_pj $pasture_area_ha <- terra::extract(low_leakage_pj, low_leakage_pj)[,2] * 100

# Add stocking rate
src <- rast("variables/stocking_rate_carrying_capacity_albers_pj.tif")
low_leakage_pj$max_sr <- terra::extract(src, low_leakage_pj)[,2]
low_leakage_pj$max_sr_capped <- low_leakage_pj$max_sr * 0.9 # Capping at 90% of the maximum stocking rate

# add beef yield
yield <- rast("variables/beef_yield_kg_ha_2021_albers_1km.tif")
low_leakage_pj$beef_yield_kg_ha_2021 <- terra::extract(yield, low_leakage_pj)[,2]

# Add animal performance
ap <- readRDS("input_tables/historical_yield_immediate_regions_2011_2021.rds") %>%
  filter(year == 2021) %>%
  mutate(cod_rgi = as.double(cod_rgi))

low_leakage_df <- as.data.frame(low_leakage_pj)

low_leakage_df <- low_leakage_df %>%
  left_join(ap[, c(1, 21)], by = join_by("code_immediate" == "cod_rgi")) %>%
  mutate(
    max_yield = max_sr_capped * DA_uf,
    max_production = max_yield * pasture_area_ha,
    FP_zero_growth = beef_yield_kg_ha_2021* pasture_area_ha,
  )

# Baseline production
low_leakage_df2 <- calc_baseline_production(
  project = low_leakage_df ,
  project_age = 50,
  H = 1,
  rate = "linear"
) %>%
  mutate(capped_FP = if_else(FP > max_production, max_production, FP))

# with leakage at 100%, aggregate 100% of FP

fp_aggregate <- low_leakage_df2%>%
  group_by(coordinates,name_biome,name_immediate)%>%
  summarise(FP_projected = sum(capped_FP))


# add to the original projects

low_leakage <- left_join(low_leakage,fp_aggregate)

# # I can now calculate total area available for restoration
# # and total forgone production that would need to be offset!
# 
# low_leakage_agg <- low_leakage %>%
#   group_by(name_biome)%>%
#   summarise(total_restorable_million_ha = sum(restor_area)/10^6,
#             total_FP_pj_tonnes = sum(FP_projected)/1000)


# save

saveRDS(low_leakage,"output_tables/hide/total_restorable_area_bin_20_30.rds")

# i now need to check how many areas I need to select
# to promote intensification

