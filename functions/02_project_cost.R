# load packages ----------------------------------------------------------------

library(dplyr)
library(terra)

#-------------------------------------------------------------------------------

# project cost based on Instituto escolhas report

calc_project_cost <- function(project){
  
  # read restoration costs
  # Here I can use the costs from my model or from Brooke Williams paper!
  #restoration_costs <- rast("variables/restoration_costs_reais_ha_2023_tropical_forests_albers_regreen_vals_1km.tif")
  restoration_costs <- rast("variables/restoration_costs_reais_ha_2023_tropical_forests_albers_regreen_vals_1km_my_model.tif")
  # extract restoration cost from raster
  project <- cbind(project,terra::extract(restoration_costs,project)[2])
  
  # rename
  index <- which(names(project)==names(restoration_costs))
  names(project)[index] <- "restoration_cost_reais_ha_2023"
  # name variable = restoration_cost_reais_ha_2023
  #project$restoration_cost_reais_ha_2023 <- restoration_costs_f$custo
  
  # mutiply by area of the cell covered with pasture
  project$total_restoration_costs_reais_2023 <- project$restoration_cost_reais_ha_2023 * project$pasture_area_ha
  
  
  # add land cost
  land_value <- rast("variables/land_value_2023_albers_1km.tif")
  
  project <- cbind(project,terra::extract(land_value,project)[2])
  
  # Rename the new column to 'carbon_value'
  colnames(project)[ncol(project)-1] <- "vtn_reais_ha_2023"
  #names(project)[27] <- "vtn_reais_ha_2023"
  
  project$total_land_value_reais_ha <- project$vtn_reais_ha_2023 *100
  
  #saveRDS(project,"clusters/project_with_costs.rds")
  return(project)
}