# based on VERRA VMD0054

# BP =  summation(production/duration of historical data)*(1+rj)^t
# BP = baseline production
# rj =  annual growth rate of yield.
# t =  time elapsed since the project started

# Forgone production
# difference between baseline production and the monitored production (MP) of that same comootity in the project area. 
 
# FP = BP -  MP


# load packages ----------------------------------------------------------------

library(terra)
library(dplyr)
library(sf)

# ------------------------------------------------------------------------------

# calculate BP - function to calculate baseline production

calculate_bp_exp <- function(p,H,t,rj,a){
  
  bp <-( (sum(p)/H)*(1+rj)^t)*a
  return(bp)
}

calculate_bp_linear <- function(p,t,rj,a){
  
  if(rj == 0){
    
    bp <- p  * a } else{
     
      bp <- (p + (rj * t)) * a 
      
    } 
  
  
  
  return(bp)
}



# add option to calculate yield using exponential or
# linear yield growth

calc_baseline_production <- function(project,project_age,H, rate ,rj = NULL) {
  
  projects <- project

  # production increase rate

  if (is.null(rj)){
  
    rj <- readRDS("input_tables/rj_immediate_regions_2010_2019.rds")
    # set negative growth rates to zero
    rj <-  rj %>%
      mutate(across(c(growth_rate, growth_unit_linear), ~ if_else(. < 0, 0, .)))
    
    
    rj$growth_rate <- rj$growth_rate/100

  # combine project and rj
  projects <- projects %>%mutate(code_immediate = as.character(code_immediate))
  projects <- left_join(projects,rj[,c(1,3,4,5)],by = join_by("code_immediate"=="cod_rgi"))
  
  }else {
    
    projects$growth_rate <- rj
  }


  projects$project_age <- project_age

  # this calculates BP as exponential
  if (rate == "exponential"){
  projects <- projects %>%
    rowwise() %>%
    mutate(BP = calculate_bp_exp(p = beef_yield_kg_ha_2021, 
                             H = 1,
                             t = project_age,
                             rj = growth_rate,
                             a= pasture_area_ha))
  }# this calculates BP as exponential 
  else{
  
    projects <- projects %>%
      rowwise() %>%
      mutate(BP = calculate_bp_linear(p = beef_yield_kg_ha_2021,
                               t = project_age,
                               rj =   growth_unit_linear,
                               a= pasture_area_ha))
    
    
}

  # because we assumed the project occupy the entire pixel, MP = 0

  projects$MP <- 0
  projects$FP <- projects$BP - projects$MP

  #return the result
  return(projects)
}