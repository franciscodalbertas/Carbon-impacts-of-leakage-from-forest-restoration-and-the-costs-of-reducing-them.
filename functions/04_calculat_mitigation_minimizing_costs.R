# function to select pixels to do mitigation by the minimum costs

minimize_costs <- function(sr_carrying,
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
                           pot_yield){
  
  # transform in a matrix (need to add yield to the extraction)
  data <- as.data.frame(c(sr_carrying,
                          current_yield,
                          total_cost_ha,
                          pasture_area,
                          scale), xy = TRUE, na.rm = TRUE)
  
  
  colnames(data) <- c("x_mitig",
                      "y_mitig",
                      "sr_carrying",
                      "current_yield",
                      "cost_reais_ha",
                      "area_available_ha",
                      "boundary")
  
  
  # do differently for biome!! get all data withing the biome!
  # add animal performance!
  
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
  }else {
    
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
  #need to change the leakage calculation!!
  
  data <- data %>%
    mutate(yield_carrying = (sr_carrying * 0.9) * DA_uf,
           production_carrying = yield_carrying * area_available_ha,
           current_production = current_yield * area_available_ha, 
           delta_production_kg = production_carrying - current_production)%>%
    # filter out areas where delta production is <=0 (no space for intensification)
    filter(delta_production_kg>0)
  
  
  # this is where I choose the method. maybe can have one script per method.
  # as a function that I source here based on a if_else statement. 
  
  # split the data into the projects boundaries (this only works for immediate regions!)
  # for biomes it wont be necessary as it is the same!!!
  
  if(mitigation_scale=="cluster"){
  
  data_1 <- data %>%
    filter(boundary == boundary_1) # this is the max yield/c
  
  data_2 <- data %>%
    filter(boundary == boundary_2) # this is the min yield/c
  
  
  }else{
    
    # data 1 and 2 should be the same!!
    
    data_1 <- data
    
    data_2 <- data
    
  }
  
  # arrange by mitigation costs, with lower mitigation costs first!
  data_1 <- data_1 %>%
    arrange(cost_reais_ha)
  
  data_2 <- data_2 %>%
    arrange(cost_reais_ha)
  
  data_1 <- data_1 %>%
    mutate(
      # density by animal performance. 
      density =  current_yield/ DA_uf,
      intensified_density = density +1,
      intensified_yield =  intensified_density * DA_uf,
      achievable_production_kg = intensified_yield * area_available_ha,
      # here I replace the production by capping it to the carrying capacity when needed
      achievable_production_kg = if_else(achievable_production_kg<delta_production_kg,achievable_production_kg,delta_production_kg),
      cumulative_production_kg = cumsum(achievable_production_kg),
      achievable_yield = achievable_production_kg/area_available_ha
    ) 
  
  data_2 <- data_2 %>%
    mutate(
      density =  current_yield/ DA_uf,
      intensified_density = density +1,
      intensified_yield =  intensified_density * DA_uf,
      #achievable_production_kg = 320*area_available_ha, # I was multiplying by 320 before
      achievable_production_kg = intensified_yield * area_available_ha,
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
  # aqui ta dando erro!!
  final_selection_pj1 <- data_1[1:last_included_index_pj1, ] # aqui da erro?? mas pq roda sem ser no background??
  final_selection_pj2 <- data_2[1:last_included_index_pj2, ]
  
  # calculate the costs (isso aqui ta invertido!!)o max eh min e o min eh max!
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
  
  
  
  # bind to the project dataset
  final_selection <- rbind(final_selection_pj1,final_selection_pj2)
  
  # adding biome to the cost data
  final_selection$name_biome <- biome
  
  # filter projects in the same biome
  projects <- filter(projects, name_biome == biome)
  
  # adjust names to perform the join (isso ferrou o join)
  names(final_selection)[7] <- "code_immediate"
  
  # if the cost/ha is fixed, here I have to multiply it by the "area" needed, not
  # the FP. The area is given by leakage param.
  final_selection$mitigation_costs_reais_50y <- 
    final_selection$cost_reais_ha * final_selection$area_required
  
  # save as shapefile to check on qgis
  #final_selection_shp <- st_as_sf(final_selection, coords = c("x_mitig","y_mitig"))
  #path <- paste0("output_tables/mitigation_locations_",source_params,"_",mitigation_scale,"_",biome,"_","round8.shp")
  #st_write(final_selection_shp,path,append = F)

  # aggregate results to make the combination easier (this should not consider immediate regions for aggregate if the scale is biome)
  
  if(mitigation_scale == "cluster"){
  
  final_selection_agg <- final_selection %>%
    group_by(code_immediate,yield_carbon_ratio,name_biome)%>%
    summarise(mitigation_costs_reais_50y =  sum(mitigation_costs_reais_50y))%>%
    mutate(code_immediate =  as.character(code_immediate))
  
  }else{
    
    final_selection_agg <- final_selection %>%
      group_by(yield_carbon_ratio,name_biome)%>%
      summarise(mitigation_costs_reais_50y =  sum(mitigation_costs_reais_50y))
    
    
  }
  # need to define commom id to join! (this causes probelms as there are many rows!)
  project_mitigation <- left_join(final_selection_agg,projects)
  
  project_mitigation$mitigation_strategy <- "minimize_costs"
  project_mitigation$mitigation_scale <- mitigation_scale
  project_mitigation$approach <- "intensification"
  project_mitigation$params <- source_params
  
  return(project_mitigation)
}