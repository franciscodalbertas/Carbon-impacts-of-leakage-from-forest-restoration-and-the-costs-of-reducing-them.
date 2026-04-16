# load packages ----------------------------------------------------------------
library(terra)
library(sf)
library(spatstat.geom) # For compactness calculations
library(spatstat)

# ------------------------------------------------------------------------------

# Function to find clusters and calculate their sums
find_clusters_and_sum <- function(r, land_value, biome, size) {
  results <- data.frame(summed_value = numeric(0), 
                        land_value = numeric(0),
                        code_immediate = character(0), 
                        coordinates = character(0), 
                        stringsAsFactors = FALSE)
  
  # Filter the biome boundary
  biome_boundary <- biomes %>%
    filter(name_biome == biome)
  
  
  # Function to find clusters and calculate their sums
  # Get regions within the biome
  immediate <- readRDS("input_tables/imediate_regions_am_af.rds")
  
  # define a treshold to exclude regions that have less than 50%(?)of their area
  # within the biome
  immediate <- filter(immediate,proportional_area >=0.501)
  
  # Extract unique regions within the biome
  regions <- unique(immediate$code_immediate[immediate$name_biome == biome])  
  
  for (i in 1:length(regions)) {
    # Clip raster to region
    region <- filter(immediate, code_immediate == regions[i])
    r_clipped <- crop(r, region)
    land_value_clipped <- crop(land_value, region)  # Clip the additional raster too
    r_clipped <- mask(r_clipped, region)
    land_value_clipped <- mask(land_value_clipped, region)
    
    # Convert raster to dataframe (coordinates and values)
    df <- as.data.frame(r_clipped, xy = TRUE, na.rm = TRUE)
    colnames(df) <- c("x", "y", "value")
    
    if (nrow(df) < size) next  # Skip if not enough pixels in the region
    
    # Identify non-NA cells
    non_na_cells <- which(!is.na(values(r_clipped)))
    
    # Convert raster to terra format to get adjacency (4 directions)
    neighbors <- adjacent(r_clipped, cells = non_na_cells, directions = 4, pairs = TRUE)
    
    # Initialize variables for cluster identification
    visited_indices <- integer(0)  # Keep track of the visited indices
    remaining_indices <- non_na_cells  # Indices of remaining unvisited cells
    
    # Find clusters of size 'n'
    while (length(remaining_indices) >= size) {
      # Start from a random unvisited pixel
      start_idx <- sample(remaining_indices, 1)
      visited_indices <- c(start_idx)
      remaining_indices <- remaining_indices[remaining_indices != start_idx]
      
      # Initialize the visited set for the cluster
      visited <- df[df$x == xyFromCell(r_clipped, start_idx)[1] & df$y == xyFromCell(r_clipped, start_idx)[2], , drop = FALSE]
      
      # Expand the cluster until it reaches the desired size
      while (nrow(visited) < size) {
        # Find neighbors of the current visited pixels
        neighbor_ids <- unique(neighbors[neighbors[, 1] %in% visited_indices, 2])
        
        # Get the unvisited neighbors
        unvisited_neighbors <- setdiff(neighbor_ids, visited_indices)
        
        if (length(unvisited_neighbors) == 0) break  # No more valid neighbors
        
        # Add one unvisited neighbor to the cluster
        new_idx <- unvisited_neighbors[1]  # Simply pick the first available neighbor
        visited_indices <- c(visited_indices, new_idx)
        
        # Add the new pixel to the visited dataframe
        new_pixel <- df[df$x == xyFromCell(r_clipped, new_idx)[1] & df$y == xyFromCell(r_clipped, new_idx)[2], , drop = FALSE]
        visited <- rbind(visited, new_pixel)
        
        # Remove the new pixel from remaining indices
        remaining_indices <- remaining_indices[remaining_indices != new_idx]
      }
      
      # If the visited set has reached the desired size, store the result
      if (nrow(visited) == size) {
        # Sum the original raster values and the land_value raster values for the selected pixels
        cluster_sum <- sum(visited$value, na.rm = TRUE)
        
        # Sum the corresponding land_value raster values for the same pixels
        land_value_sum <- sum(values(land_value_clipped)[visited_indices], na.rm = TRUE)
        
        # Concatenate coordinates as a single string
        coordinates_str <- paste(paste(visited$x, visited$y, sep = ","), collapse = ";")
        
        # Append the result to the results dataframe
        results <- rbind(results, data.frame(summed_value = cluster_sum, 
                                             land_value = land_value_sum, 
                                             code_immediate = regions[i], 
                                             coordinates = coordinates_str))
      }
    }
  }
  
  return(results)
}

