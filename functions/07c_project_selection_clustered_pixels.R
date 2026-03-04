# load pakages ---------------------------------------------------------------

library(dplyr)

#------------------------------------------------------------------------------

project_selection <- function(yield_carbon_ratio) {
  
  # yield
  yield_curr <- rast("variables/beef_yield_kg_ha_2021_albers_1km.tif")
  
  
  # biomes
  biomes <- readRDS("input_tables/am_af.rds")
  

  # land value
  land_value <- rast("variables/land_value_2023_albers_1km.tif")
  
  # choose min project
  
  min <- filter(yield_carbon_ratio,summed_value==min(summed_value))
  max <- filter(yield_carbon_ratio,summed_value==max(summed_value))
  
  # Extract the coordinates  for min(split them into x, y pairs)
  coords_min <- strsplit(as.character(min$coordinates), ";")[[1]]
  coords_min <- do.call(rbind, strsplit(coords_min, ","))
  coords_min <- as.data.frame(coords_min)
  colnames(coords_min) <- c("x", "y")
  coords_min$x <- as.numeric(coords_min$x)
  coords_min$y <- as.numeric(coords_min$y)
  coords_min$code_immediate <- min$code_immediate
  coords_min$mean_y_c <- min$summed_value/nrow(coords_min)
  
  # Extract the coordinates  for max(split them into x, y pairs)
  coords_max <- strsplit(as.character(max$coordinates), ";")[[1]]
  coords_max <- do.call(rbind, strsplit(coords_max, ","))
  coords_max <- as.data.frame(coords_max)
  colnames(coords_max) <- c("x", "y")
  coords_max$x <- as.numeric(coords_max$x)
  coords_max$y <- as.numeric(coords_max$y)
  coords_max$code_immediate <- max$code_immediate
  coords_max$mean_y_c <- max$summed_value/nrow(coords_max)
  
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
