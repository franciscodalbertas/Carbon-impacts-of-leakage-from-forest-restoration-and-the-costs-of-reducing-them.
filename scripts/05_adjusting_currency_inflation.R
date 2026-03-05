# read results high and low projects
#df <- readRDS("output_tables/aggregated_results_sample_projects_round9_local_biome_country_scale.rds")
df <- readRDS("output_tables/stefan_results/abacus_high_low_leakage_projects_r11_local_biome_country_scale_prj_yield_SCC_3.rds")


df_random <- readRDS("output_tables/abacus_random_projects_local_biome_country_scale_prj_yield_SCC_3.rds")

df_verra <- readRDS("output_tables/stefan_results/verra_high_low_leakage_projects_r11_country_scale_prj_yield_SCC_3.rds")


# adjust currencies for USD in 2021!
# 1st, get inflation for each date!
# mitigation values are from year 2020  Ajusted by "Índice Nacional de Preços 
##ao Consumidor Amplo (IPCA)" acumulated from jan 2021 to may 2022. 

# Wirseniius model used 1BRL=0.2 USD conversion rate..apply that to int. cost stuff!

# Restoration costs from re.green in 2024, so assume this
# Deflate 2022 BRL to 2021 BRL using Brazilian inflation
# Use the Índice Nacional de Preços ao Consumidor Amplo (IPCA), Brazil's official inflation index.
# 
# IPCA 2021 (Dec 2021): 10.06% (cumulative for 2021)
# 
# IPCA 2022 (Dec 2022): 5.79% (cumulative for 2022)
# cost_2021 =  Cost_2022/1+IPCA_2022 = Cost_2022/1.0579

IPCA_2021 <- 1.0579
IPCA_2024 <- 1.1583
IPCA_2023 <- 1.0462*1.0579
# 2. Convert BRL (2021 values) to USD (2021) using average 2021 exchange rate
# Use the annual average BRL/USD rate for 2021, published by BCB.

# Average BRL/USD exchange rate in 2021: 5.3955
# (i.e., 1 USD = 5.3955 BRL)
#cost_2021_USD = cost_2021/5.3955
USD_rate_2021 = 5.3955

USD_BRL_Stefan = 5


df <- df %>%
  mutate(
    
    # deflate
    # restoration costs
    total_land_value_USD_2021 = total_land_value_reais_ha/IPCA_2023, 
    total_restoration_costs_USD_2021 = total_restoration_costs_reais_2023/IPCA_2024,
    # convert to USD
    mitigation_costs_USD_50y_2021 =  mitigation_costs_reais_50y/USD_BRL_Stefan,
    total_restoration_costs_USD_2021 = total_restoration_costs_USD_2021/USD_rate_2021,
    total_land_value_USD_2021 = total_land_value_USD_2021/USD_rate_2021
    )

df<- df %>%
  mutate(project_costs_USD = total_restoration_costs_USD_2021+
           total_land_value_USD_2021+
           mitigation_costs_USD_50y_2021,
         CO2_cost_COC_USD =  project_costs_USD/final_CO2_balance_corrected_minus_COC,
         CO2_cost_NOCOC_USD =  project_costs_USD/final_CO2_balance_corrected)%>%
  ungroup()



df_random <- df_random %>%
  mutate(
    
    # deflate
     # restoration costs
    total_land_value_USD_2021 = total_land_value_reais_ha/IPCA_2023, 
    total_restoration_costs_USD_2021 = total_restoration_costs_reais_2023/IPCA_2024,
    # convert to USD
    mitigation_costs_USD_50y_2021 =  mitigation_costs_reais_50y/USD_rate_2021,
    total_restoration_costs_USD_2021 = total_restoration_costs_USD_2021/USD_rate_2021,
    total_land_value_USD_2021 = total_land_value_USD_2021/USD_rate_2021
  )

df_random <- df_random %>%
  mutate(project_costs_USD = total_restoration_costs_USD_2021+
           total_land_value_USD_2021+
           mitigation_costs_USD_50y_2021,
         CO2_cost_USD =  project_costs_USD/final_CO2_balance_corrected)%>%
  ungroup()

# verra

df_verra <- df_verra %>%
  mutate(
    
    # deflate
    # restoration costs
    total_land_value_USD_2021 = total_land_value_reais_ha/IPCA_2023, 
    total_restoration_costs_USD_2021 = total_restoration_costs_reais_2023/IPCA_2024,
    # convert to USD
    mitigation_costs_USD_50y_2021 =  mitigation_costs_reais_50y/USD_BRL_Stefan,
    total_restoration_costs_USD_2021 = total_restoration_costs_USD_2021/USD_rate_2021,
    total_land_value_USD_2021 = total_land_value_USD_2021/USD_rate_2021
  )

df_verra <- df_verra %>%
  mutate(project_costs_USD = total_restoration_costs_USD_2021+
           total_land_value_USD_2021+
           mitigation_costs_USD_50y_2021,
         CO2_cost_COC_USD =  project_costs_USD/final_CO2_balance_corrected_minus_COC,
         CO2_cost_NOCOC_USD =  project_costs_USD/final_CO2_balance_corrected)%>%
  ungroup()





# save
saveRDS(df,"output_tables/stefan_results/abacus_high_low_leakage_projects_r11_local_biome_country_scale_prj_yield_SCC_3_USD.rds")
saveRDS(df_random,"output_tables/abacus_random_projects_local_biome_country_scale_prj_yield_SCC_3_USD.rds")
saveRDS(df_verra,"output_tables/stefan_results/verra_high_low_leakage_projects_r11_country_scale_prj_yield_SCC_3_USD.rds")
