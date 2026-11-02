# for the entire distribution of projects, divide them in percentiles
# for the AF and AM and extract the relevant variables defining their features
# to show it would not change much to take that many projects


# load packages ----------------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)

#-------------------------------------------------------------------------------

am <- readRDS("input_tables/all_potential_projects_AM.rds")
af <- readRDS("input_tables/all_potential_projects_AF.rds")


ap <- readRDS("input_tables/historical_yield_immediate_regions_2011_2021.rds") %>%
  filter(year == 2021) %>%
  mutate(cod_rgi = as.double(cod_rgi))

am_f <- am %>%
  filter(code_immediate %in% ap$cod_rgi)

af_f <- af %>%
  filter(code_immediate %in% ap$cod_rgi)

# calculate the percentiles 

am_f <- am_f %>%
  mutate(
    percentile = percent_rank(summed_value) * 100,
    percentile_bin = cut(
      percentile,
      breaks = seq(0, 100, by = 10),
      include.lowest = TRUE,
      right = FALSE
    )
  )

af_f <- af_f %>%
  mutate(
    percentile = percent_rank(summed_value) * 100,
    percentile_bin = cut(
      percentile,
      breaks = seq(0, 100, by = 10),
      include.lowest = TRUE,
      right = FALSE
    )
  )


af_f$name_biome <- "Mata Atlântica"

am_f$name_biome <- "Amazônia"

# combine results per biome into one file

projects<- rbind(af_f,am_f)

# add immediate region info.

immediate <- readRDS("input_tables/imediate_regions_am_af.rds")
immediate <- st_drop_geometry(immediate)

# excluding unwanted columns
projects <- left_join(projects,immediate[,-c(8,9,10)])

# remove old objects

rm(am_f,am,ap,immediate,af,af_f)

# check number of projects on the 80th percentile in each biome

projects %>%
  # filter percentile 80 untill 70 (do the same for [20,30))
  filter(percentile_bin %in% c("[70,80)"))%>%
  group_by(name_biome)%>%
  summarise(
    n_projects = n_distinct(coordinates),
    .groups = "drop"
  )


# check number of projects on the 80th percentile in each biome

projects %>%
  # filter percentile 80 untill 70 (do the same for [20,30))
  filter(percentile_bin %in% c("[20,30)"))%>%
  group_by(name_biome)%>%
  summarise(
    n_projects = n_distinct(coordinates),
    .groups = "drop"
  )


# get coordinates

coords_pj <- projects %>%
  mutate(coord_pairs = strsplit(coordinates, ";")) %>%
  unnest(coord_pairs) %>%
  separate(coord_pairs, into = c("x", "y"), sep = ",", convert = TRUE)


# add the variables needed to calculate variability and also the area available
# for restoration (I will also need to find out how much can be mitigated!)

land_value <- rast("variables/land_value_2023_albers_1km.tif")
rest_cost <- rast("variables/restoration_costs_reais_ha_2023_tropical_forests_albers_regreen_vals_1km_my_model.tif")
pasture_share <- rast("variables/pasture_coverage_lapig_2021_col8albers_pj_1000mres.tif")
carbon <- rast("variables/delta_AGC_BGC_SOC_Walter_22_albers_50y_1km.tif")
yield <- rast("variables/beef_yield_kg_ha_2021_albers_1km.tif")
# Combine the rasters
raster_stack <- c(
  land_value,
  rest_cost,
  pasture_share,
  carbon,
  yield
)

# Give layers useful names
names(raster_stack) <- c(
  "land_value",
  "rest_cost",
  "past_share",
  "carbon",
  "yield"
)

# Convert the coordinates to spatial points
project_points <- vect(
  coords_pj,
  geom = c("x", "y"),
  crs = crs(raster_stack)
)

# Extract raster values
raster_values <- terra::extract(
  raster_stack,
  project_points,
  ID = FALSE
)

coords_pj_with_values <- coords_pj %>%
  bind_cols(raster_values)%>%
  select(c(1,3,4:22))%>%
  rename(land_value = `land_value...18`)

# add cell area

coords_pj_with_values <- coords_pj_with_values %>%
  mutate(cell_area = 100)

# aggregate values per project

coords_pj_with_values_agg <- coords_pj_with_values%>%
  group_by(name_biome,code_immediate,coordinates)%>%
  mutate(restor_area = cell_area*past_share,
         carbon = restor_area * carbon,
         production = yield*restor_area,
         rest_cost = restor_area * rest_cost)%>%
  summarise(sum_land = sum(land_value*cell_area),
            sum_area = sum(cell_area),
            restor_area = sum(restor_area),
            carbon = sum(carbon),
            production = sum(production),
            rest_cost = sum(rest_cost)
            )%>%
  mutate(land_value_ha = sum_land/sum_area,
         carbon_ha = carbon/restor_area,
         yield_ha = production/restor_area,
         rest_cost_ha = rest_cost/restor_area)%>%
  select(-c(sum_land,sum_area,production,carbon,rest_cost))

# merge this to the original table!

projects2 <- projects %>%
  left_join(coords_pj_with_values_agg)

# save it to annalyze later!

saveRDS(projects2,"input_tables/all_projects_with_deciles.rds")

