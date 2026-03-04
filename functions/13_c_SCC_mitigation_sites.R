# Adjusting carbon balance according to SCC
# Handles leakage timescale vs sequestration with scale and data source flexibility

library(tidyr)
library(dplyr)

# function

SCC_calculator <- function(input_data =NULL){


# Read  inputs
SCC <- readRDS("input_tables/SCC_rcp45.rds")
biomass_recovery <- readRDS("input_tables/biomass_recovery.rds")
recovery_50y <- biomass_recovery %>% filter(years == 50) %>% pull(recovery)

project_id <- unique(input_data$coordinates)
    

  
#expand biomass_recovery
biomass_recovery <- biomass_recovery %>%
  crossing(coordinates = project_id) 
  
  
  
co2_vals <- input_data %>%
  group_by(yield_carbon_ratio, name_biome,coordinates) %>%
  summarise(forgone_co2 = mean(forgone_co2, na.rm = TRUE), .groups = "drop") %>%
  mutate(name = case_when(
    yield_carbon_ratio == "min" & name_biome == "Amazônia" ~ "co2_min_AM",
    yield_carbon_ratio == "max" & name_biome == "Amazônia" ~ "co2_max_AM",
    yield_carbon_ratio == "min" & name_biome == "Mata Atlântica" ~ "co2_min_AF",
    yield_carbon_ratio == "max" & name_biome == "Mata Atlântica" ~ "co2_max_AF"
      ))
    
    
   
  
  
  
  
# combine values with biomass recovery
  
biomass_recovery <- biomass_recovery %>%
  left_join(co2_vals)
  
  
biomass_recovery <- biomass_recovery %>%
  mutate(
    forgone_co2_yearly = (recovery / recovery_50y) * forgone_co2,
      
  )

# there is something wrong in the way it is done as there should be a increase
# in biomass following the growth and currently it is being treated as the 
# leakage effect!!
  
biomass_recovery <- biomass_recovery %>%
  group_by(coordinates) %>%
  mutate(Year = seq(2021, 2021 + 100, 1)) %>%
  left_join(filter(SCC, Year <= 2080), by = "Year") %>%
  filter(!is.na(SCC_t))
  
 

biomass_recovery2 <- biomass_recovery %>%
  arrange(years, name) %>%
  group_by(name, coordinates) %>%
  mutate(
    EP = SCC_t / 133.0843,
    forgone_co2_lag = forgone_co2_yearly - lag(forgone_co2_yearly, default = 0),
    forgone_CO2_discounted = forgone_co2_lag * EP
  ) %>%
  filter(years <= 50) 

# Summarize per group and project! sum the yearly data!
# something wrong as this is actually generating a much higher value#
# I think I actually needed the lagged, or what is being added, not 
# the cummulative sum!!

biomass_recovery2 %>%
  group_by(yield_carbon_ratio)%>%
  summarise(sum(forgone_co2_lag))


biomass_agg <- biomass_recovery2 %>%
  group_by(name,  coordinates) %>%
  summarise(
    forgone_CO2_discounted = sum(forgone_CO2_discounted),
    .groups = "drop")
    
# how come the ratio betwen forgone carbon discounted and not is exactly the 
# the same for both max and min? I think it is because the only thing opperating
# is the effect of the SCC discounting...


# checking total discount!
# #Total discount
# total_discount <-   sum(biomass_recovery2$forgone_co2_lag) -
#   sum(biomass_recovery2$forgone_CO2_discounted) 
# 
# # Relative discount (percentage)
# discount_pct <- 100 * (1 - sum(biomass_recovery2$forgone_CO2_discounted)  / sum(biomass_recovery2$forgone_co2_lag))

# return the results

return(biomass_agg)


}