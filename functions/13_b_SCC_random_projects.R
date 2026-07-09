# Adjusting carbon balance according to SCC
# Handles leakage timescale vs sequestration with scale and data source flexibility

library(tidyr)
library(dplyr)

drop_sfc_columns <- function(x) {
  x_list <- unclass(x)
  sfc_cols <- vapply(x_list, inherits, logical(1), what = "sfc")
  x_list[sfc_cols] <- NULL
  tibble::as_tibble(x_list)
}

# Allow override from master script
if (!exists("scale")) scale <- c("cluster", "biome", "country")
if (!exists("source_to_read")) source_to_read <- "both"  # Options: "verra", "abacus", "both"

# Read shared inputs
#SCC <- readRDS("input_tables/SCC_rcp45.rds")
if(!exists("SCC_rate")) {
  SCC <- readRDS("input_tables/SCC_rcp45.rds")
}else{
  
  SCC <- readRDS("input_tables/SCC_rcp45_sensibility.rds")
  
  if (SCC_rate == 0.02) {
    SCC <- SCC %>% select(Year, SCC_t_2) %>% rename(SCC_t = SCC_t_2)
    scc_file_name <- "2"
    # define value in t0
    SCC_t0 = 636.5159
  } else if (SCC_rate == 0.03) {
    SCC <- SCC %>% select(Year, SCC_t_3) %>% rename(SCC_t = SCC_t_3)
    scc_file_name <- "3"
    # define value in t0
    SCC_t0 = 133.08431
  } else if (SCC_rate == 0.04) {
    SCC <- SCC %>% select(Year, SCC_t_4) %>% rename(SCC_t = SCC_t_4)
    scc_file_name <- "4"
    # define value in t0
    SCC_t0 = 68.22638
  } else {
    stop("Invalid SCC_rate specified. Choose from: 0.02, 0.03, or 0.04.")
    
    
  }
  
}

biomass_recovery <- readRDS("input_tables/biomass_recovery.rds")
recovery_50y <- biomass_recovery %>% filter(years == 50) %>% pull(recovery)
biomass_recovery_base <- biomass_recovery

for (i in seq_along(scale)) {
  
  # Flags for conditional processing
  read_verra <- source_to_read %in% c("verra", "both")
  read_abacus <- source_to_read %in% c("abacus", "both")
  
  if (read_verra) {
    
    # precisa rodar esse cenario aqui, de random com l=.3
    # e SCC eu aplico depois, mas eh 3%
    verra <- drop_sfc_columns(readRDS(paste0("mitigation_inputs/inputs_for_mitigation_leakage_verra_",
                                             scale[i],"_scale_yield_prj_random_sample_round1.rds")))
  
    project_id <- unique(verra$coordinates)
    
    }
  if (read_abacus) {
    abacus <- drop_sfc_columns(readRDS(paste0("mitigation_inputs/inputs_for_mitigation_leakage_abacus_",
                                              scale[i],"_scale_yield_prj_random_sample_round1.rds")))
    
    project_id <- unique(abacus$coordinates)
    
    }
  
  #expand biomass_recovery
  
  biomass_recovery <- biomass_recovery_base %>%
    crossing(coordinates = project_id) 
  
  
  # === Only compute shared biomass recovery once if Verra is being used ===
  if (read_verra) {
    
    co2_vals <- verra %>%
      group_by(yield_carbon_ratio, name_biome,coordinates) %>%
      summarise(CO2_additionality = mean(CO2_additionality, na.rm = TRUE), .groups = "drop") %>%
      mutate(name = case_when(
        yield_carbon_ratio == "random" & name_biome == "Amazônia" ~ "co2_random_AM",
        yield_carbon_ratio == "random" & name_biome == "Mata Atlântica" ~ "co2_random_MA"
      ))
    
    
    
    # co2_min_AF <- verra %>% filter(yield_carbon_ratio == "min", name_biome == "Mata Atlântica") %>% pull(CO2_additionality)
    # co2_max_AF <- verra %>% filter(yield_carbon_ratio == "max", name_biome == "Mata Atlântica") %>% pull(CO2_additionality)
    # co2_min_AM <- verra %>% filter(yield_carbon_ratio == "min", name_biome == "Amazônia") %>% pull(CO2_additionality)
    # co2_max_AM <- verra %>% filter(yield_carbon_ratio == "max", name_biome == "Amazônia") %>% pull(CO2_additionality)
    
    
    
  }else if(read_abacus){
    
    co2_vals <- abacus %>%
      group_by(yield_carbon_ratio, name_biome,coordinates) %>%
      summarise(CO2_additionality = mean(CO2_additionality, na.rm = TRUE), .groups = "drop") %>%
      mutate(name = case_when(
        yield_carbon_ratio == "random" & name_biome == "Amazônia" ~ "co2_random_AM",
        yield_carbon_ratio == "random" & name_biome == "Mata Atlântica" ~ "co2_random_MA"
      ))
    
  }
  
  
  # combine values with biomass recovery
  
  biomass_recovery <- biomass_recovery %>%
    left_join(co2_vals)
  
  
  biomass_recovery <- biomass_recovery %>%
    mutate(
      value = (recovery / recovery_50y) * CO2_additionality,
      
    )
  
  
  biomass_recovery <- biomass_recovery %>%
    group_by(coordinates) %>%
    mutate(Year = seq(2021, 2021 + 100, 1)) %>%
    left_join(filter(SCC, Year <= 2080), by = "Year") %>%
    filter(!is.na(SCC_t))
  
  #biomass_recovery_l <- biomass_recovery %>% pivot_longer(3:6)
  
  if (read_verra) {
    
    leakage_verra <- verra %>%
      ungroup() %>%
      select(yield_carbon_ratio, name_biome, leaked_CO2,coordinates) %>%
      mutate(param = "Verra",
             name = case_when(
               yield_carbon_ratio == "random" & name_biome == "Amazônia" ~ "co2_random_AM",
               yield_carbon_ratio == "random" & name_biome == "Mata Atlântica" ~ "co2_random_MA"
             ),
             years = 1,
             Year = 2022)
    
    biomass_recovery_verra <- biomass_recovery %>%
      left_join(leakage_verra, by = c("name", "years", "Year","coordinates","name_biome","yield_carbon_ratio")) %>%
      #select(-c(yield_carbon_ratio, name_biome)) %>%
      mutate(param = "Verra",
             leaked_CO2 = if_else(is.na(leaked_CO2), 0, leaked_CO2))
  }
  
  if (read_abacus) {
    leakage_abacus <- abacus %>%
      ungroup() %>%
      select(yield_carbon_ratio, name_biome, leaked_CO2,coordinates) %>%
      mutate(param = "Abacus",
             name = case_when(
               yield_carbon_ratio == "random" & name_biome == "Amazônia" ~ "co2_random_AM",
               yield_carbon_ratio == "random" & name_biome == "Mata Atlântica" ~ "co2_random_MA"
             ),
             years = 1,
             Year = 2022)
    
    biomass_recovery_abacus <- biomass_recovery %>%
      left_join(leakage_abacus, by = c("name", "years", "Year","coordinates","name_biome","yield_carbon_ratio")) %>%
      #select(-c(yield_carbon_ratio, name_biome)) %>%
      mutate(param = "Abacus",
             leaked_CO2 = if_else(is.na(leaked_CO2), 0, leaked_CO2))
  
    
    }
  
  # Combine datasets to run unified processing
  biomass_recovery2 <- bind_rows(
    if (read_verra) biomass_recovery_verra else NULL,
    if (read_abacus) biomass_recovery_abacus else NULL
  )
  
  biomass_recovery2 <- biomass_recovery2 %>%
    arrange(years, name, param) %>%
    group_by(name, param,coordinates) %>%
    mutate(additionality = value - lag(value),
           additionality = if_else(is.na(additionality), 0, additionality)) %>%
    mutate(
      net_CO2_mitigation = additionality,
      net_CO2_no_mitigation = additionality - leaked_CO2,
      EP = SCC_t / SCC_t0,  
      co2_removal_corrected = additionality * EP,
      co2_leaked_corrected = leaked_CO2 * EP
    ) %>%
    filter(years <= 50) %>%
    mutate(
      net_CO2_corrected_mitigation = co2_removal_corrected,
      net_CO2_corrected_no_mitigation = co2_removal_corrected - co2_leaked_corrected
    )
  
  # Summarize per group and project! sum the yearly data!
  
  biomass_agg <- biomass_recovery2 %>%
    group_by(name, param, coordinates) %>%
    summarise(
      net_CO2_corrected_mitigation = sum(net_CO2_corrected_mitigation, na.rm = TRUE),
      net_CO2_corrected_no_mitigation = sum(net_CO2_corrected_no_mitigation, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      yield_carbon_ratio = "random",
      name_biome = case_when(
        name %in% c("co2_random_MA") ~ "Mata Atlântica",
        TRUE ~ "Amazônia"
      )
    )
  
  if (read_verra) {
    verra_j <- left_join(verra, filter(biomass_agg, param == "Verra"), by = c("yield_carbon_ratio", "name_biome","coordinates")) %>%
      select(-name) %>%
      mutate(
        net_CO2_no_correction_no_mitigation = CO2_additionality - leaked_CO2,
        net_CO2_no_correction_mitigation = CO2_additionality
      )
    
    saveRDS(verra_j, paste0("mitigation_inputs/inputs_for_mitigation_leakage_verra_",
                             scale[i],"_scale_yield_prj_random_sample_SCC_",scc_file_name,"_round1.rds"))
  }
  
  if (read_abacus) {
    abacus_j <- left_join(abacus, filter(biomass_agg, param == "Abacus"), by = c("yield_carbon_ratio", "name_biome","coordinates")) %>%
      select(-name) %>%
      mutate(
        net_CO2_no_correction_no_mitigation = CO2_additionality - leaked_CO2,
        net_CO2_no_correction_mitigation = CO2_additionality,
        leakage_scale = scale[i]
      )
    
    saveRDS(abacus_j, paste0("mitigation_inputs/inputs_for_mitigation_leakage_abacus_",
                      scale[i],"_scale_yield_prj_random_sample_SCC_",scc_file_name,"_round1.rds"))
  }
}

