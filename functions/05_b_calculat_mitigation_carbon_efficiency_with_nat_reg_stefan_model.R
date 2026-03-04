# function to select pixels to do mitigation by the minimum costs

carbon_optimization <- function(sr_carrying,
                           total_cost_ha,
                           pasture_area,
                           mitigation_scale,
                           source_params,
                           target_FP_1,
                           target_FP_2,
                           projects,
                           scale,
                           boundary_1,
                           boundary_2,
                           biome,
                           # adding current yield
                           current_yield,
                           # adding sr at carryng capacity
                           pot_yield,
                           # adding delta y/c ratio
                          delta_y_c,
                          # add prob. nat. reg
                          p_n_r
                           ){
  
  source("functions/13_c_SCC_mitigation_sites.R")
  
  
  # transform in a matrix (need to add yield to the extraction)
  data <- as.data.frame(c(sr_carrying,
                          current_yield,
                          total_cost_ha,
                          pasture_area,
                          delta_y_c,  # this creates way to many more nas!
                          scale,
                          p_n_r
                          ), xy = TRUE, na.rm = TRUE)
  
  
  colnames(data) <- c("x_mitig",
                      "y_mitig",
                      "sr_carrying",
                      "current_yield",
                      "cost_reais_ha",
                      "area_available_ha",
                      "delta_y_c",
                      "boundary",
                      "p_n_r")
  
  
  if(mitigation_scale=="cluster"){
    beef_yield_municipality_kg_ha_2021 <- readRDS("input_tables/beef_yield_municipality_kg_ha_2021.rds")%>%
    group_by(uf)%>%
    distinct(DA_uf) %>%
    rename(abbrev_state =  uf)
  
  # get code immediate with animal performance (this work for immediate region but needs update for biome!!)
  # would need to include biome!!or at least all UFs involved!
  animal_performance <-   projects %>%
      select(code_immediate,abbrev_state)%>%
      left_join(beef_yield_municipality_kg_ha_2021) %>%
    mutate(code_immediate = as.double(code_immediate))
  }else{
    
    # I need to get animal performance for all immediate regions 
    
    yield_immediate <- readRDS("input_tables/historical_yield_immediate_regions_2011_2021.rds")
    
    animal_performance <-   yield_immediate %>%
      filter(year == 2021) %>%
      select(cod_rgi,abbrev_state,DA_uf)%>%
      rename(code_immediate =  cod_rgi) %>%
      ungroup() %>%
      #left_join(beef_yield_municipality_kg_ha_2021) %>%
      mutate(code_immediate = as.double(code_immediate))
  
    
    }
  
  # add AP to data ( i need info on state for this!)
  
  data <- data %>%
    left_join(select(animal_performance,c(code_immediate,DA_uf)),
              by = join_by("boundary" ==  "code_immediate"))%>%
    # exclude pixels outside the limits
    filter(!is.na(DA_uf))
  
  # calculte maximum achievable yield at carrying capacity (capped to 90%): 
  
  data <- data %>%
    mutate(yield_carrying = (sr_carrying * 0.9) * DA_uf,
           production_carrying = yield_carrying * area_available_ha,
           current_production = current_yield * area_available_ha, 
           delta_production_kg = production_carrying - current_production)%>%
    # filter out areas where delta production is <=0 (no space for intensification)
    filter(delta_production_kg>0)
  
  # add data on delta yield from stefans model..using coordinates!
  # yield gain data
  yield_gain <- rast("variables/pasture_intensification_yield_gains_to_ext_annually_Stefan.tif")
  temp_vec <-  vect(data, geom = c("x_mitig", "y_mitig"), crs = crs(delta_carbon))
  # extract values
  delta_yield <- terra::extract(yield_gain,temp_vec)
  # rename
  delta_yield <- delta_yield %>% rename(intens_gain_yield = pasture_quality_2022)
  # add to data
  data <- data %>% bind_cols(delta_yield %>% select(intens_gain_yield))
  rm(yield_gain,temp_vec,delta_yield)
  
  # split the data into the projects boundaries 
  
  if(mitigation_scale=="cluster"){
  
  data_1 <- data %>%
    filter(boundary == boundary_1) # this is the max yield/c
  
  data_2 <- data %>%
    filter(boundary == boundary_2) # this is the min yield/c
  
  # arrange by delta_y_c, with higher values  first (big yield gap, low carbon)!
  # check the other scrip (cost, as I think the data is not arranged!!)
  
  
  }else{
    
    # data 1 and 2 should be the same!!
    
    data_1 <- data
    
    data_2 <- data
    
  }
  
  data_1 <- data_1 %>%
    arrange(desc(delta_y_c))
  
  data_2 <- data_2 %>%
    arrange(desc(delta_y_c))

  data_1 <- data_1 %>%
    mutate(
      # density by animal performance. 
      density =  current_yield/ DA_uf,
      # Production_current (Pc):
      P_c = current_yield * area_available_ha,
      # intensified yield (Y_i):
      Y_i =  intens_gain_yield + current_yield,
      # Production intensified (P_i):
      P_i = area_available_ha *(p_n_r*Y_i+((1-p_n_r)*intens_gain_yield)),
      # production achievable with intensification
      achievable_production_kg = P_c + P_i,
      # here I replace the production by capping it to the carrying capacity when needed
      achievable_production_kg = if_else(achievable_production_kg<delta_production_kg,achievable_production_kg,delta_production_kg),
      cumulative_production_kg = cumsum(achievable_production_kg),
      achievable_yield = achievable_production_kg/area_available_ha
    ) 
  
  data_2 <- data_2 %>%
    mutate(
      # density by animal performance. 
      density =  current_yield/ DA_uf,
      # Production_current (Pc):
      P_c = current_yield * area_available_ha,
      # intensified yield (Y_i):
      Y_i =  intens_gain_yield + current_yield,
      # Production intensified (P_i):
      P_i = area_available_ha *(p_n_r*Y_i+((1-p_n_r)*intens_gain_yield)),
      # production achievable with intensification
      achievable_production_kg = P_c + P_i,
      # here I replace the production by capping it to the carrying capacity when needed
      achievable_production_kg = if_else(achievable_production_kg<delta_production_kg,achievable_production_kg,delta_production_kg),
      cumulative_production_kg = cumsum(achievable_production_kg),
      achievable_yield = achievable_production_kg/area_available_ha
    )
  
  
  
  # Find the smallest subset of pixels that meets or exceeds the production target
  
  selected_pixels_pj1 <- data_1 %>%
    filter(cumulative_production_kg >= target_FP_1) %>%
    slice(1)
  
  selected_pixels_pj2 <- data_2 %>%
    filter(cumulative_production_kg >= target_FP_2) %>%
    slice(1)
  
  
  # Get the index of the last included pixel
  last_included_index_pj1 <- which(data_1$cumulative_production_kg == selected_pixels_pj1$cumulative_production_kg[1])
  
  last_included_index_pj2 <- which(data_2$cumulative_production_kg == selected_pixels_pj2$cumulative_production_kg[1])
  
  # Select all pixels up to and including the last one needed to exceed the target
  final_selection_pj1 <- data_1[1:last_included_index_pj1, ]
  final_selection_pj2 <- data_2[1:last_included_index_pj2, ]
  
  # calculate the costs
  final_selection_pj1 <-  final_selection_pj1 %>%
    mutate(cumulative_production_kg = if_else(cumulative_production_kg>target_FP_1,target_FP_1,cumulative_production_kg),
           yield = delta_production_kg/area_available_ha,
           new_target = if_else(row_number() == 1,target_FP_1,target_FP_1 - lag(cumulative_production_kg)),
           area_required = new_target/achievable_yield,
           area_required = round(if_else(area_required > area_available_ha,area_available_ha,area_required),2),
           yield_carbon_ratio = "max"
    )
  
  
  final_selection_pj2 <-  final_selection_pj2 %>%
    mutate(cumulative_production_kg = if_else(cumulative_production_kg>target_FP_2,target_FP_2,cumulative_production_kg),
           yield = delta_production_kg/area_available_ha,
           new_target = if_else(row_number() == 1,target_FP_2,target_FP_2 - lag(cumulative_production_kg)),
           area_required = new_target/achievable_yield,
           area_required = round(if_else(area_required > area_available_ha,area_available_ha,area_required),2),
           yield_carbon_ratio = "min"
    )
  
  
  
  # bind to the project dataset (deu ruim, nao bate n columns!)
  
  final_selection <- rbind(final_selection_pj1,final_selection_pj2)
  # adding biome to the cost data
  final_selection$name_biome <- biome
  
  # filter projects in the same biome
  projects <- filter(projects, name_biome == biome)
  
  # adjust names to perform the join (isso ferrou o join)
  names(final_selection)[8] <- "code_immediate"
  
  # if the cost/ha is fixed, here I have to multiply it by the "area" needed, not
  # the FP. The area is given by leakage param.
  final_selection$mitigation_costs_reais_50y <- 
    final_selection$cost_reais_ha * final_selection$area_required
  
  # MODIFY THIS TO HAVE THE SHAPEFILE AVAILABLE!!!!
  
  # save as shapefile to check on qgis ( modify this to save it appended so I 
  # don't need to re-run everytime!!)
  
  # Add the ID of each project here so I can extract data later!
  
  final_selection_shp <- st_as_sf(final_selection, coords = c("x_mitig","y_mitig"))
  path <- paste0("mitigation_locations/mitigation_locations_carbon",source_params,"_",mitigation_scale,"_",biome,"_","round11.shp")
  
  if (file.exists(path)) {
    st_write(final_selection_shp, path, append = TRUE)
  } else {
    st_write(final_selection_shp, path)  # creates the file
  }
  
  
  # Insert here the calculation of carbon additionality discounted by SCC...and
  # by p.nat.reg!
  
  # open c add layer to extract values
  c_add <- rast("variables/delta_AGC_BGC_SOC_Walter_22_albers_50y_1km.tif")
  # transform final_selectio to sf to extract raster values
  final_selection <- st_as_sf(final_selection, coords = c("x_mitig","y_mitig"))
  # extract values to project
  final_selection2 <- cbind(final_selection,terra::extract(c_add,final_selection)[,2])
  # Rename the new column to 'carbon_value'
  colnames(final_selection2)[ncol(final_selection2)-1] <- "delta_tons_ha"
  # forgone carbon
  final_selection2$forgone_carbon <- final_selection2$delta_tons_ha * 
    final_selection2$area_required 
  # forgone co2
  final_selection2$forgone_co2 <- final_selection2$forgone_carbon *  (44/12)
  # read pnr
  pnr <- rast("variables/prob_nat_regeneration_all_vegetation_albers1km_pj.tif")
  # extract pnr
  final_selection2 <- cbind(final_selection2,terra::extract(pnr,final_selection2)[,2])
  # Rename the new column to 'p_n_r'
  colnames(final_selection2)[ncol(final_selection2)-1] <- "p_n_r"
  # multiply pnr by forgone co2
  final_selection2$forgone_co2_nat_reg <- final_selection2$forgone_co2 * 
    (1-final_selection2$p_n_r)
  
  # Now I need to add the logic of SCC!!(by usin another script??)but this can be done
  # on the aggregated data
  
  # aggregate results to make the combination easier (this should not consider immediate regions for aggregate if the scale is biome)
  
  if(mitigation_scale == "cluster"){
  
  final_selection_agg <- final_selection2 %>%
    group_by(code_immediate,yield_carbon_ratio,name_biome)%>%
    summarise(
      mitigation_costs_reais_50y =  sum(mitigation_costs_reais_50y),
      # sum also the forgone carbon opp. cost.
      forgone_co2 = sum(forgone_co2_nat_reg)
      )%>%
    mutate(code_immediate =  as.character(code_immediate))
  
  }else{
    
    final_selection_agg <- final_selection2 %>%
      group_by(yield_carbon_ratio,name_biome)%>%
      summarise(
        mitigation_costs_reais_50y =  sum(mitigation_costs_reais_50y),
        forgone_co2 = sum(forgone_co2_nat_reg)
        )
    
    
  }
  # need to define commom id to join! (this causes probelms as there are many rows!)
  project_mitigation <- left_join(st_drop_geometry(final_selection_agg),projects)
  
  project_mitigation$mitigation_strategy <- "optimize_carbon"
  project_mitigation$mitigation_scale <- mitigation_scale
  project_mitigation$approach <- "intensification"
  project_mitigation$params <- source_params
  
  
  # now discount forgone co2 by SCC

  SCC <- SCC_calculator(input_data = project_mitigation)
  
  # join with the full dataset
  
  project_mitigation2 <- left_join(project_mitigation,SCC%>% select(2,3))
  
  # project_mitigation2 %>% group_by(yield_carbon_ratio)%>%
  #   mutate(disc=100*(1-(forgone_CO2_discounted/ forgone_co2))) %>%
  #   pull(disc) 
  # mit_cost = 17,918,645 # its half of restoration costs!!! quite high!
  # shit this might blow the costs
  # rest_cost = 32,412,978
  # mit cost max = 19,311,972 # 2 million more only
  
  return(project_mitigation2)
}
