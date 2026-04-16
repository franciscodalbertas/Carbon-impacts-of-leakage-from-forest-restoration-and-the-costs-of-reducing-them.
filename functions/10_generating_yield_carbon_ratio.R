# load packages ---------------------------------------------------------------

library(terra)
library(sf)
library(geobr)
library(dplyr)

#-------------------------------------------------------------------------------


generate_yield_carbon <- function(){
  
  # raster stack of different pasture quality areas. Sum them
  pasture_coverage <- rast("rasters/pasture_vigor_share_lapig_2021_col8albers_pj_1000mres.tif")
  pasture_coverage_sum <- sum(pasture_coverage)
  
  
  # only use areas within the AF and AM
  biomes <- readRDS("input_tables/am_af.rds")
  
  # feature to mask the pasture cover ( biome)
  pasture_coverage_c <- crop(pasture_coverage_sum,biomes)
  pasture_coverage_m <- mask(pasture_coverage_c,biomes)
  
  # get yield data (current yield or potential?)
  # using current yield:
  yield_curr <- rast("variables/beef_yield_kg_ha_2021_albers_1km.tif")
  
  # this needs capping based on the quantiles of stocking rates 
  yield_df <- readRDS("input_tables/beef_yield_municipality_kg_ha_2021.rds")%>%
    filter(yield>0)
  
  # use first quantile from the original dataset:
  quantile_5 <- quantile(yield_df$density,na.rm=T,probs=0.05)
  quantile_95 <- quantile(yield_df$density,na.rm=T,probs=0.95)
  
  # low boundary
  low <- min(yield_df$yield[yield_df$density>=quantile_5])
  high <- max(yield_df$yield[yield_df$density<=quantile_95])
  
  # transform 0 in nas
  yield_curr <- ifel(yield_curr==0,NA,yield_curr)
  # cap yield values
  yield_curr <- ifel(yield_curr<low,NA,yield_curr)
  yield_curr <- ifel(yield_curr>high,NA,yield_curr)
  
  # mask for biomes
  yield_c <- crop(yield_curr,biomes)
  yield_m <- mask(yield_c,biomes)
  
  # get delta carbon data as proxy for additionality - maximum achieved
  # Using Walter delta considering Poorter biomass gain!
  delta_carbon <- rast("variables/delta_AGC_BGC_SOC_Walter_22_albers_50y_1km.tif")
  
  # mask
  delta_carbon_c <- crop(delta_carbon,biomes)
  delta_carbon_m <- mask(delta_carbon_c,biomes)
  
  # filter out pixels with less than 50 tons ha of carbon additionality
  # why?? because this results in non-viable projects (add that to the methods)
  # should I do it? with the capping, no need, just cap <0
  #delta_carbon_m <- ifel(delta_carbon_m < 50, NA, delta_carbon_m)
  delta_carbon_m <- ifel(delta_carbon_m <= 0, NA, delta_carbon_m)
  
  # calculate the yield/c_add ratio
  yield_carbon_ratio <- yield_m/(delta_carbon_m)
  
  # masks out pixels with less than 50% pasture coverage (review that!)
  pasture_mask <- ifel(pasture_coverage_m < 0.5, NA, 1)
  
  yield_carbon_ratio_m <- yield_carbon_ratio * pasture_mask
  
  # exclude negative values (where the deltacarbon is negative)
  yield_carbon_ratio_m <- ifel(yield_carbon_ratio_m < 0, NA, yield_carbon_ratio_m)
  
  # mask pixels with NA at the restoration cost layer!
  restoration_costs <- rast("variables/restoration_costs_reais_ha_2023_tropical_forests_albers_regreen_vals_1km.tif")
  
  # adjust extent
  restoration_costs <- project(restoration_costs,yield_carbon_ratio_m)
  
  # Apply the mask
  yield_carbon_ratio_m <- mask(yield_carbon_ratio_m, restoration_costs)
  
  # load land value
  #land_value_raster <- rast("variables/land_value_2023_albers_1km.tif")
  
  # choose pixels for project location
  # include a rule so that all pixels are in the same cluster!!
  
  # pixels in the amazon
  yield_carbon_ratio_AM <- crop(yield_carbon_ratio_m,filter(biomes,name_biome=="Amazônia"))
  yield_carbon_ratio_MA <- crop(yield_carbon_ratio_m,filter(biomes,name_biome=="Mata Atlântica"))
  
  
  
  # Return all preprocessed objects as a named list
  list(
    yield_carbon_ratio_AM = yield_carbon_ratio_AM,
    yield_carbon_ratio_MA = yield_carbon_ratio_MA,
    biomes = biomes
  )
  
}