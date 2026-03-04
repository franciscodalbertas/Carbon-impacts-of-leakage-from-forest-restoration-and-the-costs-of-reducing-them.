# based on VERRA VMD0054

# BP =  summation(production/duration of historical data)*(1+rj)^t
# BP = baseline production
# rj =  annual growth rate of yield.
# t =  time elapsed since the project started

# Forgone production
# difference between baseline production and the monitored production (MP) of that same comootity in the project area. 
 
# FP = BP -  MP

# monitored production is done up to 5 years after the project start date.
# Because the project will be implemented, I need to calculate MP just like BP but discouting the forgone production -- which can change depending on the action.

# Definition of project area - the entire pixel
# 
# add a term so I can choose to use the default rj or the customized!!
# then I can run the scenarios without worring about all the yield data I dont have!!

# load packages ----------------------------------------------------------------

library(terra)
library(dplyr)
library(sf)

# ------------------------------------------------------------------------------

# calculate BP - function to calculate baseline production
# need to change based on area of the project dedicated to pasture!

calculate_bp_exp <- function(p,H,t,rj,a){
  
  bp <-( (sum(p)/H)*(1+rj)^t)*100
  return(bp)
}

calculate_bp_linear <- function(p,H,t,rj){
  
  bp <-( (sum(p)/H)*(1+rj)^t)*100
  
  return(bp)
}



# add option to calculate yield using exponential or
# linear yield growth

calc_baseline_production <- function(project,project_age,H, rj = NULL) {
  
  projects <- project

  # production increase rate

  if (is.null(rj)){
  
  rj <- readRDS("input_tables/rj_immediate_regions_2010_2019.rds")

  rj$growth_rate <- rj$growth_rate/100

  # combine project and rj

  #projects <- merge(projects,rj)
  projects <- left_join(projects,rj)
  
  }else {
    
    projects$growth_rate <- rj
  }


  projects$project_age <- project_age



  projects <- projects %>%
    rowwise() %>%
    mutate(BP = calculate_bp(p = beef_yield_kg_ha_2021, H = 1, t = project_age, rj =   growth_rate))


  # because we assumed the project occupy the entire pixel, MP = 0

  projects$MP <- 0
  projects$FP <- projects$BP - projects$MP


  # that's the yearly FP if leakage was 100%

  #return the result
  return(projects)
}