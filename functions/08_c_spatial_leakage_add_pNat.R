# *** note *** 
# with forgone production calculated, this script model where leakage is happening and quantify
# consequences in terms of land use



spatial_leakage <- function(projects, scale, mitigation_type = NULL){
  
  # for each project, at the cluster scale, mask the immediate regions
  # and calculate pixels to leak based on prob. def + land value
  
  
  # this works when leakage is LOCAL
  immediate_regions_sp <- readRDS("input_tables/imediate_regions_am_af.rds")
  immediate_regions <- readRDS("input_tables/BP_regions_avg_yield_2021.rds")
  names(immediate_regions)[1] <- "code_immediate"
  # filter immediate regions selected by projects! 
  immediate_regions_f <- filter(immediate_regions,code_immediate %in% as.character(projects$code_immediate))
  # summarise fp per immediate region
  immediate_regions_sp$code_immediate <- as.character(immediate_regions_sp$code_immediate)
  immediate_regions_sp$year <- as.character(immediate_regions_sp$year)
  immediate_regions_sp$code_state <- as.character(immediate_regions_sp$code_state)
  # Join the data, keeping geometry from immediate_region_sp
  immediate_region_f_with_geom <- immediate_regions_f %>%
    left_join(immediate_regions_sp , by = "code_immediate") %>%
    st_as_sf()
  
  total_fp <- projects %>%
    group_by(yield_carbon_ratio,name_biome,code_immediate,name_immediate,coordinates)%>%
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
              total_area = sum(area),
      .groups = "drop") 
  
  # raster with forest cover 
  fc <- rast("variables/forest_cover_albers_1km_2021.tif")
  
  # raster with prob. deforestation
  def <- rast("variables/Williams_pred_to_increase_pasture_albers_1km_pj.tif")
  
  # raster with land value
  landvalue <- rast("variables/land_value_2023_albers_1km.tif")

  # raster with average yield attributable to pastures from moving-window
  yield <- rast("variables/mean_yield_10km_moving_window.tif")

  # current carbon (only AGB)
  current <- rast("variables/AGC_current_Walter_22_layer_albers_1km_res.tif")

  # prob. nat. regeneration
  nat_reg <- rast("variables/prob_nat_regeneration_all_vegetation_albers1km_pj.tif")

  biome_regions <- NULL
  country_target <- NULL
  
  immediate_regions_ls <- unique(total_fp$code_immediate)
  
  rslt_list <- list()
  candidate_cache <- list()
  
  for(i in seq_along(immediate_regions_ls)){
    
    
    fp <- total_fp %>%
      filter(code_immediate ==immediate_regions_ls[i])

    if(scale == "cluster"){
      # get immediate region boundary
      target <- immediate_region_f_with_geom %>%
      filter(code_immediate == immediate_regions_ls[i])%>%
      st_transform(crs = crs(fc))
      cache_key <- paste0("cluster_", immediate_regions_ls[i])
    }else if(scale == "biome"){
      # get biome boundaries!  
      if (is.null(biome_regions)) {
        biome_regions <- readRDS("input_tables/am_af.rds")
      }
      target <- filter(biome_regions, name_biome == fp$name_biome[1])
      cache_key <- paste0("biome_", fp$name_biome[1])
    }else{
      if (is.null(country_target)) {
        country_target <- rast("variables/delta_AGC_BGC_SOC_Walter_22_albers_50y_1km.tif")
      }
      target <- country_target
      cache_key <- "country"
          
          }

    if (is.null(candidate_cache[[cache_key]])) {
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
      
      current_c <- crop(current,target)
      current_m <- mask(current_c,target)
      
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
      
      # updated correction suggested by r#3 ------------------------------------
      # ****  add BGB to the leakage calculation ****
      # 0.24 was obtained by calculating the delta diff between BGB in pastures
      # and forests and converged in both AM and AF
      #-------------------------------------------------------------------------
      values_df <- values_df %>%
        mutate(
          carbon_ton_ha_agb = carbon_ton_ha,
          carbon_ton_ha = carbon_ton_ha * 1.24
        )
      
      #-------------------------------------------------------------------------
      # add production - this is how much each forested area could produce
      
      values_df$beef_pot_prod_kg <- values_df$forest_area * values_df$yield_kg_ha 
      
      # arrange in decreasing order of prob. of conversion and increasing order of
      # land value
      
      arranged_df <- values_df %>%
        arrange(desc(prob_clearance), (land_value))%>%
        mutate(
            cumulative_production_kg = cumsum(beef_pot_prod_kg)
        )

      candidate_cache[[cache_key]] <- list(
        arranged_df = arranged_df,
        cumulative_area = cumsum(arranged_df$forest_area),
        cumulative_carbon = cumsum(arranged_df$carbon_ton_ha * arranged_df$forest_area)
      )
    }

    arranged_df <- candidate_cache[[cache_key]]$arranged_df
    cumulative_area <- candidate_cache[[cache_key]]$cumulative_area
    cumulative_carbon <- candidate_cache[[cache_key]]$cumulative_carbon
    cumulative_co2 <- cumulative_carbon * (44/12)

    project_selection <- vector("list", nrow(fp))

    for (j in seq_len(nrow(fp))) {
      FP_leaked <- fp$total_FP_leaked[j]

      last_included_index <- which(arranged_df$cumulative_production_kg >= FP_leaked)[1]

      previous_index <- last_included_index - 1L
      previous_production <- if (previous_index > 0) arranged_df$cumulative_production_kg[previous_index] else 0
      previous_area <- if (previous_index > 0) cumulative_area[previous_index] else 0
      previous_carbon <- if (previous_index > 0) cumulative_carbon[previous_index] else 0
      previous_co2 <- if (previous_index > 0) cumulative_co2[previous_index] else 0

      last_pixel <- arranged_df[last_included_index, ]
      production_ratio <- if (last_pixel$cumulative_production_kg <= FP_leaked) {
        1
      } else {
        (FP_leaked - previous_production) / last_pixel$beef_pot_prod_kg
      }

      area_deforested <- previous_area + (production_ratio * last_pixel$forest_area)
      leaked_carbon <- previous_carbon + (last_pixel$carbon_ton_ha * production_ratio * last_pixel$forest_area)
      leaked_CO2 <- previous_co2 + (last_pixel$carbon_ton_ha * production_ratio * last_pixel$forest_area * (44/12))

      project_selection[[j]] <- data.frame(
        yield_carbon_ratio = fp$yield_carbon_ratio[j],
        name_biome = fp$name_biome[j],
        code_immediate = fp$code_immediate[j],
        name_immediate = fp$name_immediate[j],
        coordinates = fp$coordinates[j],
        area_deforested = area_deforested,
        leaked_carbon = leaked_carbon,
        leaked_CO2 = leaked_CO2
      )
    }

    rslt_list[[i]] <- do.call(rbind,project_selection)
    
  
    }
  
  updated_projects <- do.call(rbind,rslt_list)
  
  # combine the above project with project info.!
  
  updated_projects_agg <- updated_projects %>%
    group_by(yield_carbon_ratio,name_biome,code_immediate,coordinates)%>%
    summarise(area_deforested =  sum(area_deforested),
              leaked_carbon=sum(leaked_carbon),
              leaked_CO2 = sum(leaked_CO2),
              .groups = "drop")
  
  to_bind <- immediate_region_f_with_geom %>% dplyr::select(code_immediate,P_UF,DA_uf,yield)
  # add data on project current yield and average immediate_region_yield
  projects$code_immediate <- as.character(projects$code_immediate)
  updated_projects_agg <- updated_projects_agg %>%
    mutate(code_immediate = as.character(code_immediate)) %>%
    left_join(to_bind,by = "code_immediate")
    
  updated_projects_agg <- st_drop_geometry(updated_projects_agg)
  # organizing names to make it clearer!
  updated_projects_agg <- updated_projects_agg %>%
    rename(current_avg_yield =  yield)
  
  
  # add area reforested
  
  updated_projects_agg2 <- left_join(
    updated_projects_agg,
    total_fp,
    by = c("yield_carbon_ratio", "name_biome", "code_immediate", "coordinates")
  )
  
  return(updated_projects_agg2)
}
