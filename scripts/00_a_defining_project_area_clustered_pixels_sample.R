# load packages ---------------------------------------------------------------

library(terra)
library(sf)
library(geobr)
library(dplyr)
# built-in function to select projects
source("functions/07c_project_selection_clustered_pixels_sample.R")
# -----------------------------------------------------------------------------

# select high and low leakage projects (max, min or high, low)

# open project distributions:
# AF (entire pool, not capped to 20 - 80)
yield_carbon_ratio_ma <- readRDS("input_tables/clustered_pixels_MA_full_distribution.RDS")
# AM
yield_carbon_ratio_am <- readRDS("input_tables/clustered_pixels_AM_full_distribution.RDS")

# check if there is data for the immediate regions! regarding historical data
# on ap, yield etc...

ap <- readRDS("input_tables/historical_yield_immediate_regions_2011_2021.rds") %>%
  filter(year == 2021) %>%
  mutate(cod_rgi = as.double(cod_rgi))

yield_carbon_ratio_ma_f <- yield_carbon_ratio_ma %>%
  filter(code_immediate %in% ap$cod_rgi)

yield_carbon_ratio_am_f <- yield_carbon_ratio_am %>%
  filter(code_immediate %in% ap$cod_rgi)

# capp the distribution here so the sample only includes the values around
# 20 and 80 quantiles

bottom_lim_ma <- quantile(yield_carbon_ratio_ma_f$summed_value,0.20,na.rm=T)
bottom_lim_am <- quantile(yield_carbon_ratio_am_f$summed_value,0.20,na.rm=T)

top_lim_ma <-  quantile(yield_carbon_ratio_ma_f$summed_value,0.80,na.rm=T)
top_lim_am <-  quantile(yield_carbon_ratio_am_f$summed_value,0.80,na.rm=T)


# capping the values!
yield_carbon_ratio_ma_capped <- yield_carbon_ratio_ma_f %>%
  filter(summed_value>= bottom_lim_ma & summed_value <= top_lim_ma)

yield_carbon_ratio_am_capped <- yield_carbon_ratio_am_f %>%
  filter(summed_value>= bottom_lim_am & summed_value <= top_lim_am)



project_locations_MA <- project_selection(
  yield_carbon_ratio = yield_carbon_ratio_ma_capped
  )

project_locations_MA$name_biome <- "Mata Atlântica"

project_locations_AM <- project_selection(
  yield_carbon_ratio = yield_carbon_ratio_am_capped
)

project_locations_AM$name_biome <- "Amazônia"

# combine results per biome into one file

projects_location <- rbind(project_locations_AM,project_locations_MA)

# add immediate region info.

immediate <- readRDS("input_tables/imediate_regions_am_af.rds")
immediate <- st_drop_geometry(immediate)

# excluding unwanted columns
projects_location_imme <- left_join(projects_location,immediate[,-c(8,9,10)])

# saving results
saveRDS(projects_location_imme,
        "project_location/project_location_round1_sampled100projects.rds"
)
