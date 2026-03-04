# load packages ----------------------------------------------------------------

library(terra)
library(dplyr)
library(sf)

# ------------------------------------------------------------------------------

calc_delta_c <- function(project)  {
  
  
  projects <- st_as_sf(project)
  
  # load delta raster
  delta <- rast("variables/delta_AGC_BGC_SOC_Walter_22_albers_50y_1km.tif")
  
  # extract values to project
  projects_C <- cbind(projects,terra::extract(delta,projects)[,2])
  
  
  colnames(projects_C)[ncol(projects_C)-1] <- "delta_tons_ha"
  
  #save data
  #saveRDS(projects_C,"clusters/project_CO2.rds")
  return(projects_C)
}