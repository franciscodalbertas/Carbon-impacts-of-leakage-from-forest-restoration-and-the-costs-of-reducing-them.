# load packages ---------------------------------------------------------------

library(terra)
library(sf)
library(geobr)
library(dplyr)
library(tidyr)

# -----------------------------------------------------------------------------

# select random projects across the entire distribution

# open project distributions:
# AF (entire pool, not capped to 20 - 80)
yield_carbon_ratio_ma <- readRDS("input_tables/clustered_pixels_MA_full_distribution.RDS")
# AM
yield_carbon_ratio_am <- readRDS("input_tables/clustered_pixels_AM_full_distribution.RDS")

# historical data on ap, yield etc...

ap <- readRDS("input_tables/historical_yield_immediate_regions_2011_2021.rds") %>%
  filter(year == 2021) %>%
  mutate(cod_rgi = as.double(cod_rgi))

yield_carbon_ratio_ma_f <- yield_carbon_ratio_ma %>%
  filter(code_immediate %in% ap$cod_rgi)

yield_carbon_ratio_am_f <- yield_carbon_ratio_am %>%
  filter(code_immediate %in% ap$cod_rgi)

# sampling projects

set.seed(123)  # or any other number

project_locations_MA <- yield_carbon_ratio_ma_f %>%
  slice_sample(n = 100)


project_locations_AM <- yield_carbon_ratio_am_f %>%
  slice_sample(n = 100)

project_locations_AM$name_biome <- "Amazônia"
project_locations_MA$name_biome <- "Mata Atlântica"

# combine results per biome into one file
projects_location <- rbind(project_locations_AM,project_locations_MA)

# adding additional variables

# yield

yield_curr <- rast("variables/beef_yield_kg_ha_2021_albers_1km.tif")


# land value 
land_value <- rast("variables/land_value_2023_albers_1km.tif")


#Split and unnest
coords <- projects_location %>%
  mutate(coord_pairs = strsplit(coordinates, ";")) %>%
  unnest(coord_pairs) %>%
  separate(coord_pairs, into = c("x", "y"), sep = ",", convert = TRUE)

coords <- coords %>%
  mutate(mean_y_c = if_else(name_biome=="Amazônia",summed_value/17,summed_value/3))
  

coords <- st_as_sf(coords, coords = c("x", "y"), crs = crs(yield_curr))

# add yield
project_locations2 <- cbind(coords,
                            terra::extract(yield_curr,coords))


# add land_value
project_locations3 <- cbind(project_locations2,
                            terra::extract(land_value,project_locations2))


# add immediate region info.
immediate <- readRDS("input_tables/imediate_regions_am_af.rds")
immediate <- st_drop_geometry(immediate)

projects_location_imme <- left_join(project_locations3,immediate[,-c(8,9,10)])

# this uses clustered pixels
saveRDS(projects_location_imme,
        "project_location/project_location_round1_randomsampled100projects.rds"
)

