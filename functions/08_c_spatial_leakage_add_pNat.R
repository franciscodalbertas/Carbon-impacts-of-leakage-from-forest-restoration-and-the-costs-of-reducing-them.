# with forgone production calculated, model where leakage is happening and quantify
# consequences in terms of land use (this should vary with spatial scale)
# forgot to incorporate verra or abacus parameters!!! this will change the fp!!
# easy to do!



spatial_leakage <- function(projects, scale, mitigation_type = NULL){
  
  # for each project, at the cluster scale, I need to mask the immediate regions
  # and calculate pixels to leak based on prob. def + land value
  
  # include a conditional argument if the scale is immediate region, biome, or
  # country (start with immediate region only)
  
  # 1: open immediate_regions (replace this with something with average yield,
  # growth yield and expected yield in 50y)
  
  # this works when leakage is LOCAL
  immediate_regions_sp <- readRDS("input_tables/imediate_regions_am_af.rds")
  immediate_regions <- readRDS("input_tables/BP_regions_avg_yield_2021.rds")
  names(immediate_regions)[1] <- "code_immediate"
  # 2: filter immediate regions selected by projects! 
  immediate_regions_f <- filter(immediate_regions,code_immediate %in% as.character(projects$code_immediate))
  #names(projects)[1] <- "code_immediate"
  # summarise fp per immediate region
  immediate_regions_sp$code_immediate <- as.character(immediate_regions_sp$code_immediate)
  immediate_regions_sp$year <- as.character(immediate_regions_sp$year)
  immediate_regions_sp$code_state <- as.character(immediate_regions_sp$code_state)
  # add spatial part (no spatial incorporated!)
  # immediate_regions_f <- st_as_sf(immediate_regions_f)
  # immediate_regions_f <- st_join(immediate_regions_f,immediate_regions_sp)[,-c(26:30)]
  # 
  # Join the data, keeping geometry from immediate_region_sp
  # Join the data, keeping geometry from immediate_region_sp
  immediate_region_f_with_geom <- immediate_regions_f %>%
    left_join(immediate_regions_sp , by = "code_immediate") %>%
    st_as_sf()
  
  total_fp <- projects %>%
    group_by(yield_carbon_ratio,name_biome,code_immediate,name_immediate)%>%
    mutate(area = 100) %>%
    summarise(
      # FP leaked == FP_capped
      total_FP = sum(FP),
      total_FP_leaked = sum(FP_leaked),
      total_area_restored = if (!is.null(mitigation_type) && mitigation_type == "within_project") {
                sum(area_restoration)
              } else {
                sum(pasture_area_ha)
              },
              total_area = sum(area)) 
  
  # raster with forest cover 
  fc <- rast("variables/forest_cover_albers_1km_2021.tif")
  
  # raster with prob. deforestation (diff.resolution, I need to degrade it)
  def <- rast("variables/Williams_pred_to_increase_pasture_albers_1km_pj.tif")
  
  # raster with land value
  landvalue <- rast("variables/land_value_2023_albers_1km.tif")
  
  # here I need to modify, for each immediate region
  
  immediate_regions_ls <- unique(total_fp$code_immediate)
  
  rslt_list <- list()
  
  # I am loosing the info of biome, yield_carbon ratio
  
  for(i in seq_along(immediate_regions_ls)){
    
    
    if(scale == "cluster"){
      # get immediate region boundary
      target <- immediate_region_f_with_geom %>%
      filter(code_immediate == immediate_regions_ls[i])%>%
      st_transform(crs = crs(fc))
      }else if(scale == "biome"){
      # get biome boundaries!  
      target <- readRDS("input_tables/am_af.rds")
      target <- filter(target, name_biome == total_fp$name_biome[i])
    }else{
        
      target <- rast("variables/delta_AGC_BGC_SOC_Walter_22_albers_50y_1km.tif")
          
          }
    
    fp <- total_fp %>%
      filter(code_immediate ==immediate_regions_ls[i])
      
    FP_leaked <- fp$total_FP_leaked
    
    
    #yield <- target$yield
    # get average yield from moving-window!
    yield <- rast("variables/mean_yield_10km_moving_window.tif")
    yield_c <- crop(yield,target)
    yield_m <- mask(yield_c,target)
    # get forest cover
    fc_c <- crop(fc,target)
    fc_m <- mask(fc_c,target)
    # round values
    fc_m <- round(fc_m,2)
    # tranform fc ==0 in NA
    fc_mask <- ifel(fc_m==0,NA,1)
    # get prob. deforestation
    def_c <- crop(def,target)
    def_m <- mask(def_c,target)
    # mask def to areas with forest
    def_m <- def_m*fc_mask
    
    # get land value
    landvalue_c <- crop(landvalue,target)
    landvalue_m <- mask(landvalue_c,target)
    
    # mask def to areas with forest
    landvalue_m <- landvalue_m*fc_mask
    
    # calculate forest area (assuming the average yield, I can get cleared land
    # based on the forest area)
    forest_area <- fc_m *100
    
    # current carbon
    current <- rast("variables/AGC_current_Walter_22_layer_albers_1km_res.tif")
    current_c <- crop(current,target)
    current_m <- mask(current_c,target)
    
    # prob. nat. regeneration (added just in case. should not be used here probably)
    nat_reg <- rast("variables/prob_nat_regeneration_all_vegetation_albers1km_pj.tif")
    nat_reg_c <- crop(nat_reg,target)
    nat_reg_m <- mask(nat_reg_c,target)
    # Combine rasters into a SpatRaster collection (add yield mean!)
    rasters <- c(landvalue_m, def_m, forest_area,current_m,yield_m,nat_reg_m)
    
    # Extract values from all rasters
    values_df <- as.data.frame(rasters, na.rm = T,xy=T)
    
    # Rename the columns for clarity
    colnames(values_df) <- c("x",
                             "y",
                             "land_value",
                             "prob_clearance",
                             "forest_area",
                             "carbon_ton_ha",
                             "yield_kg_ha",
                             "prob_nat_reg")
    
    # add production - this is how much each forested area could produce
    # should I use the yield considering the end of the period of 50 years
    # or the average current yield of the immediate region?
    # think using the average yield of the region is more accurate as deforestation
    # is more likely shortly after the project is undertaken!
    
    values_df$beef_pot_prod_kg <- values_df$forest_area * values_df$yield_kg_ha 
    
    
    
    # arrange in decreasing order of prob. of conversion and increasing order of
    # land value
    
    arranged_df <- values_df %>%
      arrange(desc(prob_clearance), (land_value))%>%
      mutate(
          cumulative_production_kg = cumsum(beef_pot_prod_kg)
      )
    
    # select pixels untill the production is met. I need to account for partial
    # clearance too. So, a new column of how much land was cleared will be needed
    
    # Find the smallest subset of pixels that meets or exceeds the production target
    selected_pixels <- arranged_df %>%
      filter(cumulative_production_kg >= FP_leaked) %>%
      slice(1)
    
    # Get the index of the last included pixel
    last_included_index<- which(arranged_df$cumulative_production_kg == selected_pixels$cumulative_production_kg[1])
    
    # Select all pixels up to and including the last one needed to exceed the target
    final_selection <- arranged_df[1:last_included_index, ]
    
    # calculate the difference between the target and the production achieved
    
    final_selection$difference <- FP_leaked - final_selection$cumulative_production_kg 
    
    # calculate the required area deforested!
    # check if this is correct!!!! 
    final_selection <- final_selection %>%
      mutate(
        # proportion of the area forested that needs to be deforested
        production_ratio = if_else(cumulative_production_kg<=FP_leaked,
                                   1,
                                   abs(difference/beef_pot_prod_kg)),
        area_deforested = production_ratio*forest_area,
        leaked_carbon = carbon_ton_ha * area_deforested,
        leaked_CO2 = leaked_carbon * (44/12))
    
    
    final_selection$yield_carbon_ratio <- fp$yield_carbon_ratio
    final_selection$name_biome <- fp$name_biome
    final_selection$code_immediate <- fp$code_immediate
    final_selection$name_immediate <- fp$name_immediate
    
    rslt_list[[i]] <- final_selection
    
  
    }
  
  updated_projects <- do.call(rbind,rslt_list)
  
  # combine the above project with project info.!
  # do I care about the location? no, aggregate to combine
  # think about how to combine aggregated values with non-aggregate
  # project locations!
  updated_projects_agg <- updated_projects %>%
    group_by(yield_carbon_ratio,name_biome,code_immediate)%>%
    summarise(area_deforested =  sum(area_deforested),
              leaked_carbon=sum(leaked_carbon),
              leaked_CO2 = sum(leaked_CO2))
  
  to_bind <- immediate_region_f_with_geom %>% dplyr::select(code_immediate,P_UF,DA_uf,yield)
  # add data on project current yield and average immediate_region_yield
  projects$code_immediate <- as.character(projects$code_immediate)
  updated_projects_agg <- updated_projects_agg %>%
    mutate(code_immediate = as.character(code_immediate)) %>%
    left_join(to_bind,by = "code_immediate")
    
  updated_projects_agg <- st_drop_geometry(updated_projects_agg)
  updated_projects_agg <- updated_projects_agg[,-10]
  # organizing names to make it clearer!
  updated_projects_agg <- updated_projects_agg %>%
    rename(current_avg_yield =  yield)
  
  
  # add area reforested
  
  updated_projects_agg2 <- left_join(updated_projects_agg,total_fp)
  
  return(updated_projects_agg2)
}
