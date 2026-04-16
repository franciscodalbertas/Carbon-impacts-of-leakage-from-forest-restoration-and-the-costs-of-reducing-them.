
project_scale <- function(projects,
                          scale,
                          pot_yield,
                          intensification_costs,
                          pasture_share,
                          maintenance_costs,
                          rate,
                          years,
                          delta_carbon,
                          min_cost,
                          # add current yield data
                          current_yield,
                          # add delta_y_c_ratio
                          delta_y_c,
                          intensification_model = NULL
                          ){
  
  # load packages
  library(sf)
  library(dplyr)
  library(terra)
  
  if (scale == "cluster") {
    # Opening cluster boundaries
    mitigation_area <- readRDS("input_tables/imediate_regions_am_af.rds")
    # Filter cluster of interest
    mitigation_area <- filter(mitigation_area, code_immediate %in% projects$code_immediate)
  } else if (scale == "biome") {
    # Opening biome boundaries
    mitigation_area <- readRDS("input_tables/am_af.rds")
  } else {
    # Default to country-level boundaries (will be good if we want to exclude 
    # areas such as PAs)
    mitigation_area <- readRDS("input_tables/br.rds")
  }
  
  
  # making sure extents are the same
  mitigation_area <- st_transform(mitigation_area, crs = crs(pot_yield))
  
  # masking delta_yield
  pot_yield_c <- crop(pot_yield,mitigation_area)
  pot_yield_m <- mask(pot_yield_c,mitigation_area)
  
  # mask to cluster (here it should change depending which data I am using!)
  intensification_costs_c <- (crop(intensification_costs,mitigation_area))
  intensification_costs_m <- (mask(intensification_costs_c,mitigation_area))
 
  if(identical(intensification_model, "carlos")){ 
  # replace 0 for NAs, as 0 are areas which are not degraded so no cost
  intensification_costs_m <- ifel(intensification_costs_m<min_cost,NA,intensification_costs_m)
  }
  # crop pasture share
  pasture_share_c <- (crop(pasture_share,mitigation_area))
  pasture_share_m <- (mask(pasture_share_c,mitigation_area))
  pasture_area <- pasture_share_m *100
  
  # size of forgone production that would need to be mitigated
  # this now depend on the region and yield_carbon
  
  # Amazonia
  target_FP_am_max <-  projects %>%
    filter(yield_carbon_ratio=="max",
           name_biome =="Amazônia")%>%
    pull(FP_leaked)
    
  
  target_FP_am_min <-  projects %>%
    filter(yield_carbon_ratio=="min",
           name_biome =="Amazônia")%>%
    pull(FP_leaked) 
  
  # Atlantic forest
  target_FP_ma_max <-  projects %>%
    filter(yield_carbon_ratio=="max",
           name_biome =="Mata Atlântica")%>%
    pull(FP_leaked)
  
  target_FP_ma_min <-  projects %>%
    filter(yield_carbon_ratio=="min",
           name_biome =="Mata Atlântica")%>%
    pull(FP_leaked)
  
  
  # add maintenance costs (only for Carlos)
  if(identical(intensification_model, "carlos")){
  maintenance_costs_c <- (crop(maintenance_costs,mitigation_area))
  maintenance_costs_m <- (mask(maintenance_costs_c,mitigation_area))
  }
  # carbon
  
  # masking
  delta_carbon_c <- (crop(delta_carbon,mitigation_area))
  delta_carbon_m <- (mask(delta_carbon_c,mitigation_area))
  
  # need to choose areas with pasture. The amount of carbon associated with pasture areas!
  delta_carbon_pasture <- delta_carbon_m * (pasture_share_m/pasture_share_m)
 
  # this is also only necessary for carlos
  if(identical(intensification_model, "carlos")){
    # Create a discount factor
    discount_factors <- 1 / ((1 + rate) ^ (1:years))
    
    # Sum the discount factors to get the NPV factor
    npv_factor <- sum(discount_factors)
    
    # Apply the NPV calculation to the raster
    maintenance_costs_29y <- maintenance_costs_m * npv_factor
    
    # maintenance cost + recovery/reform cost on year 1
    total_cost_kg_ha <- intensification_costs_m + maintenance_costs_29y
    # multiply that to 1 AU = 450 kg
    # this gives the weighted cost... then I can calculate the real cost using 
    # density and animal performance
    total_cost_ha <- total_cost_kg_ha * 450
  }else{ 
    # this has no info. on kg gain!!need to add later!
    
    total_cost_ha <- intensification_costs_m
  }
  
  # delta production.
  #delta_production <- delta_yield_m * 100 * pasture_share_m
  
  # adding currenty yield info:
  
  # masking
  yield_c <- crop(current_yield,mitigation_area)
  yield_m <- mask(yield_c,mitigation_area)
  
  # ADD DELTA_Y_C_RATIO
  delta_y_c_c <-  crop(delta_y_c,mitigation_area)
  delta_y_c_m <-  mask(delta_y_c_c,mitigation_area)
  
  # add prob. nat. reg.
  
  prob_nat_reg <- rast("variables/prob_nat_regeneration_all_vegetation_albers1km_pj.tif")
  prob_nat_reg_c <- crop(prob_nat_reg,mitigation_area)
  prob_nat_reg_m <- mask(prob_nat_reg_c,mitigation_area)
  
  # save objects in a list
  
  results <- list(pot_yield_m,total_cost_ha,pasture_area,target_FP_am_max,
                  target_FP_am_min,target_FP_ma_max,target_FP_ma_min,yield_m,
                  delta_y_c_m,prob_nat_reg_m)
  names(results) <- c("sr_carry","total_cost_ha","pasture_area","target_FP_am_max",
                      "target_FP_am_min","target_FP_ma_max","target_FP_ma_min",
                      "current_yield","delta_y_c","p_n_r")
  
  return(results)
  
}
