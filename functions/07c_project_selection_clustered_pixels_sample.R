# load pakages ---------------------------------------------------------------

library(dplyr)
library(tidyr)

#------------------------------------------------------------------------------

project_selection <- function(yield_carbon_ratio) {
  
  # yield
  
  yield_curr <- rast("variables/beef_yield_kg_ha_2021_albers_1km.tif")
  
  
  # biomes
  biomes <- readRDS("input_tables/am_af.rds")
  

  # land value
  land_value <- rast("variables/land_value_2023_albers_1km.tif")
  
  # choose min projects (bottom 100)
  min <- yield_carbon_ratio[order(yield_carbon_ratio[["summed_value"]]), ][1:100, ]
  max <-  yield_carbon_ratio[order(yield_carbon_ratio[["summed_value"]], decreasing = TRUE), ][1:100, ]
  
  # add ranking ID
  min$project_rank <- seq(1:100)
  max$project_rank <- seq(1:100)
  
  #Split and unnest
  coords_min <- min %>%
    mutate(coord_pairs = strsplit(coordinates, ";")) %>%
    unnest(coord_pairs) %>%
    separate(coord_pairs, into = c("x", "y"), sep = ",", convert = TRUE)
  
  # proj_area
  proj_area <- nrow(coords_min%>%filter(project_rank==1))
  
  # coords_min$x <- as.numeric(coords_min$x)
  # coords_min$y <- as.numeric(coords_min$y)
  #coords_min$code_immediate <- min$code_immediate
  coords_min$mean_y_c <- coords_min$summed_value/proj_area
  
  # Extract the coordinates  for max(split them into x, y pairs)
  coords_max <- max %>%
    mutate(coord_pairs = strsplit(coordinates, ";")) %>%
    unnest(coord_pairs) %>%
    separate(coord_pairs, into = c("x", "y"), sep = ",", convert = TRUE)
  
  # calculate mean y/c ratio
  coords_max$mean_y_c <- coords_max$summed_value/proj_area
  
  # Convert the selected pixels for minimum sum into an `sf` object
  selected_points_min <- st_as_sf(coords_min, coords = c("x", "y"), crs = crs(yield_curr))
  
  # add additional info.
  selected_points_min$yield_carbon_ratio <- "min"
  
  # Convert the selected pixels for maximum sum into an `sf` object
  selected_points_max <- st_as_sf(coords_max, coords = c("x", "y"), crs = crs(yield_curr))
  
  # add additional info.
  selected_points_max$yield_carbon_ratio <- "max"
  
  # combine both 
  project_locations <-rbind(selected_points_max,selected_points_min)
  
  # buffer to avoid extracting NA values
  # add info on cluster (need to create this object biomes!)
  project_locations2 <- st_join(project_locations,biomes)
  
  # add yield
  project_locations3 <- cbind(project_locations2,
                              terra::extract(yield_curr,project_locations2))
  
  
  # add land_value
  project_locations4 <- cbind(project_locations3,
                              terra::extract(land_value,project_locations3))
  
  return(project_locations4)

  }
