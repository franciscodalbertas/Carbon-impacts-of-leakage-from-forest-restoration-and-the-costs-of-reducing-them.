# load packages ----------------------------------------------------------------

library(dplyr)
library(terra)

#-------------------------------------------------------------------------------

# project cost based on Instituto escolhas report

calc_project_cost <- function(project){
  
  # read restoration costs
  restoration_costs <- rast("variables/restoration_costs_reais_ha_2023_tropical_forests_albers_regreen_vals_1km_my_model.tif")
  # extract restoration cost from raster
  project <- cbind(project,terra::extract(restoration_costs,project)[2])
  
  # rename
  index <- which(names(project)==names(restoration_costs))
  names(project)[index] <- "restoration_cost_reais_ha_2023"
  
  # mutiply by area of the cell covered with pasture
  project$total_restoration_costs_reais_2023 <- project$restoration_cost_reais_ha_2023 * project$pasture_area_ha
  
  # add land cost
  land_value <- rast("variables/land_value_2023_albers_1km.tif")
  
  project <- cbind(project,terra::extract(land_value,project)[2])
  
  # Rename the new column to 'carbon_value'
  colnames(project)[ncol(project)-1] <- "vtn_reais_ha_2023"
  
  project$total_land_value_reais_ha <- project$vtn_reais_ha_2023 *100
  
  return(project)
}