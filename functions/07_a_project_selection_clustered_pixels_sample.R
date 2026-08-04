# load pakages ---------------------------------------------------------------

library(dplyr)
library(tidyr)

#------------------------------------------------------------------------------

project_selection <- function(yield_carbon_ratio,n_projects) {
  
  select_non_overlapping_projects <- function(projects, n_projects, decreasing = FALSE,
                                              used_coords = character()) {
    ordered_projects <- projects[order(projects[["summed_value"]],
                                       decreasing = decreasing), , drop = FALSE]
    
    selected_rows <- integer(0)
    
    for (project_i in seq_len(nrow(ordered_projects))) {
      project_coords <- trimws(unlist(strsplit(ordered_projects$coordinates[project_i],
                                               ";", fixed = TRUE)))
      project_coords <- unique(gsub("\\s+", "", project_coords))
      project_coords <- project_coords[!is.na(project_coords) & nzchar(project_coords)]
      
      if (length(project_coords) > 0 && !any(project_coords %in% used_coords)) {
        selected_rows <- c(selected_rows, project_i)
        used_coords <- c(used_coords, project_coords)
      }
      
      if (length(selected_rows) == n_projects) {
        break
      }
    }
    
    if (length(selected_rows) < n_projects) {
      stop("Only ", length(selected_rows), " non-overlapping projects were available, ",
           "but ", n_projects, " were requested.")
    }
    
    list(
      projects = ordered_projects[selected_rows, , drop = FALSE],
      used_coords = used_coords
    )
  }
  
  # yield
  
  yield_curr <- rast("variables/beef_yield_kg_ha_2021_albers_1km.tif")
  
  
  # biomes
  biomes <- readRDS("input_tables/am_af.rds")
  

  # land value
  land_value <- rast("variables/land_value_2023_albers_1km.tif")
  
  # choose non-overlapping projects from the bottom and top of the distribution
  min_selection <- select_non_overlapping_projects(yield_carbon_ratio, n_projects)
  min <- min_selection$projects
  
  max_selection <- select_non_overlapping_projects(yield_carbon_ratio, n_projects,
                                                   decreasing = TRUE,
                                                   used_coords = min_selection$used_coords)
  max <- max_selection$projects
  
  # add ranking ID
  min$project_rank <- seq(1:n_projects)
  max$project_rank <- seq(1:n_projects)
  
  #Split and unnest
  coords_min <- min %>%
    mutate(coord_pairs = strsplit(coordinates, ";")) %>%
    unnest(coord_pairs) %>%
    separate(coord_pairs, into = c("x", "y"), sep = ",", convert = TRUE)
  
  # proj_area
  proj_area <- nrow(coords_min%>%filter(project_rank==1))
  
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
