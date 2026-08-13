# load packages ----------------------------------------------------------------

library(dplyr)
library(tidyr)

#-------------------------------------------------------------------------------

# results for random projects
all <- list.files(path = "mitigation_inputs/",pattern = "random_sample_SCC",full.names = T)

local_scale <- grep(pattern = "local",x = all,value = T)
biome_scale <- grep(pattern = "biome",x = all,value = T)
country_scale <- grep(pattern = "country",x = all,value = T)

# read files
local_l <- lapply(local_scale,readRDS)
biome_l <- lapply(biome_scale,readRDS) # mitigation scale is wrong!
country_l <- lapply(country_scale,readRDS) # mitigation scale is wrong!

# combine into a single df
local_df <- do.call(rbind,local_l)
biome_df <- do.call(rbind,biome_l)
country_df <- do.call(rbind,country_l)

# create ignoring scenario (scale does not matter)

projects_ignoring <- local_df %>%
  mutate(
    # define the strategy
    mitigation_strategy = "ignoring",
    # no mitigation so the cost is zero
    mitigation_costs_reais_50y = 0,
    # change scale
    mitigation_scale = "ignoring",
    approach = "ignoring",
    leakage_scale ="ignoring") %>%
  distinct()


# here scale matter (as there is leakage!)
projects_no_mitigation_local <- local_df %>%
  mutate(
    # leakage is accounted but not mitigated
    leakage_scale = "local",
    mitigation_strategy = "no_mitigation",
    mitigation_costs_reais_50y = 0,
    mitigation_scale = "no_mitigation",
    approach = "no_mitigation") %>%
  distinct()

projects_no_mitigation_biome <- biome_df %>%
  mutate(
    # leakage is accounted but not mitigated
    mitigation_strategy = "no_mitigation",
    mitigation_costs_reais_50y = 0,
    mitigation_scale = "no_mitigation",
    approach = "no_mitigation") %>%
  distinct()

projects_no_mitigation_country <- country_df %>%
  mutate(
    # leakage is accounted but not mitigated
    mitigation_strategy = "no_mitigation",
    mitigation_costs_reais_50y = 0,
    mitigation_scale = "no_mitigation",
    approach = "no_mitigation") %>%
  distinct()


# combine 
projects_local <- rbind(projects_ignoring,projects_no_mitigation_local) 
projects_biome <- projects_no_mitigation_biome
projects_country <- projects_no_mitigation_country

# combine local and biome!

projects <- rbind(projects_local,projects_biome,projects_country)


projects <- projects %>%
  mutate(
    # with carbon corrected for the SCC!
    final_CO2_balance_corrected =
      case_when(
        # if ignoring, then there is no mitigation but no discounting
        # so it should have the same balance as mitigation
         approach == "ignoring"~net_CO2_corrected_mitigation,
        # if no mitigation too
        approach == "no_mitigation"~net_CO2_corrected_no_mitigation,
        # there is mitigation in this case!
        approach == "intensification"~net_CO2_corrected_mitigation),
    # no correction for the SCC 
    final_CO2_balance_non_corrected =  
        case_when(
          approach == "ignoring"~net_CO2_no_correction_mitigation,
          approach == "no_mitigation"~net_CO2_no_correction_no_mitigation,
          approach == "intensification"~net_CO2_no_correction_mitigation),
    )


# calculate total project costs

projects<- projects%>%
  mutate(project_costs = total_restoration_costs_reais_2023+
           total_land_value_reais_ha+
           mitigation_costs_reais_50y,
         CO2_cost =  project_costs/final_CO2_balance_corrected)%>%
  ungroup()

# save
saveRDS(projects,"output_tables/abacus_verra_random_projects_local_biome_country_scale_prj_yield_SCC_3.rds")



