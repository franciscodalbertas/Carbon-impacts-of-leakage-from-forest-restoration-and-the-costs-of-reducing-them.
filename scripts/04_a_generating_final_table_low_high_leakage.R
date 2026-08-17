# load packages ----------------------------------------------------------------

library(dplyr)
library(tidyr)

#-------------------------------------------------------------------------------

# define funtion to read tables
read_result_table <- function(path) {
  x <- readRDS(path)
  x_list <- unclass(x)
  sfc_cols <- vapply(x_list, inherits, logical(1), what = "sfc")
  x_list[sfc_cols] <- NULL
  tibble::as_tibble(x_list)
}

# results for high and low projects
all <- list.files(path = "results_with_mitigation/",pattern = "round1",full.names = T)

# get it split by verra or abacus
verra <- grep(pattern = "Verra",x = all,value = T)

# abacus or ABACUS
abacus <- grep(pattern = "abacus", x = all, value = TRUE, ignore.case = TRUE)

# --- filter scale ---
# this is for abacus!!
local_scale <- grep(pattern = "local_scale",x = abacus,value = T)
biome_scale <- grep(pattern = "biome_scale",x = abacus,value = T)
country_scale <- grep(pattern = "country_scale",x = abacus,value = T)

# --- generate single table for abacus ---

local_scale_scc3 <- grep(pattern = "SCC_3",x = local_scale,value = T)
biome_scale_scc3 <- grep(pattern = "SCC_3",x = biome_scale,value = T)
country_scale_scc3 <- grep(pattern = "SCC_3",x = country_scale,value = T)


# read files
local_l <- lapply(local_scale_scc3, read_result_table)
biome_l <- lapply(biome_scale_scc3, read_result_table) 
country_l <- lapply(country_scale_scc3, read_result_table) 

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
# this include the ignoring -- as scale does not matter
projects_local <- rbind(local_df,projects_ignoring,projects_no_mitigation_local) 
projects_biome <- rbind(biome_df,projects_no_mitigation_biome) 
projects_country <- rbind(country_df,projects_no_mitigation_country) 

# combine local and biome!

projects <- rbind(projects_local,projects_biome,projects_country)

# add column with final carbon balance based on the scenario
projects <- projects %>%
  mutate(
    # with carbon corrected for the SCC!
    final_CO2_balance_corrected =
      case_when(
        # if ignoring, then there is no mitigation but no discounting
        # so it should have the same balance as mitigation
        # (but the cost aspect is different!)
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
   final_CO2_balance_corrected_minus_COC =
      case_when(
        approach == "ignoring"~final_CO2_balance_corrected,
        approach == "no_mitigation"~final_CO2_balance_corrected,
        approach == "intensification"~final_CO2_balance_corrected - forgone_CO2_discounted),
   final_CO2_balance_non_corrected_minus_COC =
     case_when(
       approach == "ignoring"~final_CO2_balance_non_corrected,
       approach == "no_mitigation"~final_CO2_balance_non_corrected,
       approach == "intensification"~final_CO2_balance_non_corrected - forgone_CO2_discounted)
   
   )



projects<- projects%>%
  mutate(project_costs = total_restoration_costs_reais_2023+
           total_land_value_reais_ha+
           mitigation_costs_reais_50y,
         CO2_cost_noCOC =  project_costs/final_CO2_balance_corrected,
         CO2_cost_COC =  project_costs/final_CO2_balance_corrected_minus_COC)%>%
  ungroup()




# save
saveRDS(projects,"output_tables/abacus_high_low_leakage_projects_r1_local_biome_country_scale_prj_yield_SCC_3.rds")

# --- generating table for verra ---

local_scale_verra <- grep(pattern = "local_scale",x = verra,value = T)
biome_scale_verra <- grep(pattern = "biome_scale",x = verra,value = T)
country_scale_verra <- grep(pattern = "country_scale",x = verra,value = T)

# filter for 3%
country_scale_verra_scc3 <- grep(pattern = "SCC_3",x = country_scale_verra,value = T)

# read files
local_l_verra <- lapply(local_scale_verra, read_result_table)
biome_l_verra <- lapply(biome_scale_verra, read_result_table)
country_l_verra <- lapply(country_scale_verra_scc3, read_result_table) 

# combine into a single df
local_df_verra <- do.call(rbind,local_l_verra)
biome_df_verra <- do.call(rbind,biome_l_verra)
country_df_verra <- do.call(rbind,country_l_verra)

# create ignoring scenario (scale does not matter)

projects_ignoring_verra <- local_df_verra %>%
  mutate(
    mitigation_strategy = "ignoring",
    mitigation_costs_reais_50y = 0,
    mitigation_scale = "ignoring",
    approach = "ignoring",
    leakage_scale ="ignoring"
  )%>%
  distinct()


projects_no_mitigation_local_verra <- local_df_verra %>%
  mutate(
    # leakage is accounted but not mitigated
    mitigation_strategy = "no_mitigation",
    mitigation_costs_reais_50y = 0,
    mitigation_scale = "no_mitigation",
    approach = "no_mitigation") %>%
  distinct()

projects_no_mitigation_biome_verra <- biome_df_verra %>%
  mutate(
    # leakage is accounted but not mitigated
    mitigation_strategy = "no_mitigation",
    mitigation_costs_reais_50y = 0,
    mitigation_scale = "no_mitigation",
    approach = "no_mitigation") %>%
  distinct()

projects_no_mitigation_country_verra <- country_df_verra %>%
  mutate(
    # leakage is accounted but not mitigated
    mitigation_strategy = "no_mitigation",
    mitigation_costs_reais_50y = 0,
    mitigation_scale = "no_mitigation",
    approach = "no_mitigation") %>%
  distinct()



# combine 
# this include the ignoring -- as scale does not matter
projects_local_verra <- rbind(local_df_verra,projects_ignoring_verra,projects_no_mitigation_local_verra) 
projects_biome_verra <- rbind(biome_df_verra,projects_no_mitigation_biome_verra) 
projects_country_verra <- rbind(country_df_verra,projects_no_mitigation_country_verra) 


# add column with final carbon balance based on the scenario
projects_verra <- rbind(projects_local_verra,projects_biome_verra,projects_country_verra) %>%
  mutate(
    # with carbon corrected for the SCC!
    final_CO2_balance_corrected =
      case_when(
        # if ignoring, then there is no mitigation but no discounting
        # so it should have the same balance as mitigation
        # (but the cost aspect is different!)
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
    # add the carbon balance minus the carbon opportunity costs of restoration
    # from mitigation areas (for the rest there is no forgone OC!)
    final_CO2_balance_corrected_minus_COC =
      case_when(
        approach == "ignoring"~final_CO2_balance_corrected,
        approach == "no_mitigation"~final_CO2_balance_corrected,
        approach == "intensification"~final_CO2_balance_corrected - forgone_CO2_discounted),
    final_CO2_balance_non_corrected_minus_COC =
      case_when(
        approach == "ignoring"~final_CO2_balance_non_corrected,
        approach == "no_mitigation"~final_CO2_balance_non_corrected,
        approach == "intensification"~final_CO2_balance_non_corrected - forgone_CO2_discounted)
    
  )



projects_verra<- projects_verra%>%
  mutate(project_costs = total_restoration_costs_reais_2023+
           total_land_value_reais_ha+
           mitigation_costs_reais_50y,
         CO2_cost_noCOC =  project_costs/final_CO2_balance_corrected,
         CO2_cost_COC =  project_costs/final_CO2_balance_corrected_minus_COC)%>%
  ungroup()

# save
saveRDS(projects_verra,"output_tables/verra_high_low_leakage_projects_r1_local_biome_country_scale_prj_yield_SCC_3.rds")

