# this figure should bring about the costs comparisons of not mitigating vs
# mitigating across scc discount and extensifications

# ---- load packages -----------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(ggpubr)

#-------------------------------------------------------------------------------

# reading average cost data 
# this is correct, but now I need one avg cost per pannel. it needs to change
# based on SCC and l!

# average cost for r=3% and l=c(30%,100%)# this has verra and abacus
avg_cost_3 <- readRDS("output_tables/abacus_verra_random_projects_local_biome_country_scale_prj_yield_SCC_3_USD.rds") 

# filtering the data
avg_cost_3 <- avg_cost_3 %>%
  filter(mitigation_strategy=="no_mitigation",
         leakage_scale == "country")


#table(avg_cost_3$param)# ok

# reading low high leakage data (again abacus and verra)
high_low_3_abacus <- readRDS("output_tables/abacus_high_low_leakage_projects_r1_local_biome_country_scale_prj_yield_SCC_3_USD.rds")

table(high_low_3_abacus$param)

# check why verra has more rows!!
high_low_3_verra <- readRDS("output_tables/verra_high_low_leakage_projects_r1_local_biome_country_scale_prj_yield_SCC_3_USD.rds")
table(high_low_3_verra$param) 
# combine
high_low_3 <- bind_rows(high_low_3_abacus,high_low_3_verra)

# filter the data

high_low_3 <- high_low_3 %>%
  filter(
    mitigation_strategy!="ignoring",
    leakage_scale == "country"
  )


#  define the common variables and columns to keep

keep <- c("yield_carbon_ratio",
          "name_biome",
          "mitigation_strategy",
          "CO2_cost_USD",
          "CO2_cost_COC_USD",
          "param")

df1 <- avg_cost_3 %>% select(any_of(keep))
df2 <- high_low_3 %>% select(any_of(keep))

# combine datasets (will need to include different randoms for different SCC!)

df <-rbind(df1,df2 %>% rename(CO2_cost_USD = "CO2_cost_COC_USD" ))

# add columns (the columns need to be added before as this varies now!)

df <- df %>%
  mutate(disc = "3%",
         leakage = if_else(param == "Abacus","100%","30%"))

#-------------------------------------------------------------------------------
# get data for 2% (and 4%) average cost (to be used on the horizontal line
#-------------------------------------------------------------------------------
# the random analysis has no mitigation, so leakage matters!

avg_cost_2_100 <-  readRDS("mitigation_inputs/inputs_for_mitigation_leakage_abacus_country_scale_yield_prj_random_sample_SCC_2_round1.rds")
avg_cost_4_100 <-  readRDS("mitigation_inputs/inputs_for_mitigation_leakage_abacus_country_scale_yield_prj_random_sample_SCC_4_round1.rds")

# add columns to unique ID them, then do the additional calculations! the leakage part should not matter
avg_cost_2_100 <- avg_cost_2_100%>%
  mutate(disc = "2%",
         leakage = "100%")

avg_cost_4_100 <- avg_cost_4_100%>%
  mutate(disc = "4%",
         leakage = "100%")


# falta ainda random com l=30%!
# this is included in df!
#avg_cost_3_30 <- readRDS("mitigation_inputs/inputs_for_mitigation_leakage_verra_country_scale_yield_prj_random_sample_SCC_3_round1.rds")

avg_cost_2_30 <- readRDS("mitigation_inputs/inputs_for_mitigation_leakage_verra_country_scale_yield_prj_random_sample_SCC_2_round1.rds")

avg_cost_4_30 <- readRDS("mitigation_inputs/inputs_for_mitigation_leakage_verra_country_scale_yield_prj_random_sample_SCC_4_round1.rds")


# combine all the additional scenarios!! (avg_cost_2,avg_cost_4,avg_cost_verra)

# avg_cost_3_30 <- avg_cost_3_30 %>%
#   mutate(disc = "3%",
#          leakage = "30%")

avg_cost_2_30 <- avg_cost_2_30 %>%
  mutate(disc = "2%",
         leakage = "30%")

avg_cost_4_30 <- avg_cost_4_30 %>%
  mutate(disc = "4%",
         leakage = "30%")


# exclupe FP_type, as it is the same for all!
#colum_to_exclude <- names(avg_cost_2_30)[which(!names(avg_cost_2_30) %in% names(avg_cost_3_30))]

# combine avg cost verra
avg_cost_verra <- avg_cost_2_30 %>%
  #bind_rows(avg_cost_2_30)%>%
  bind_rows(avg_cost_4_30)

# combine abacus and verra!
combined_new_df_avg_values <- avg_cost_2_100%>%
  bind_rows(avg_cost_4_100)%>%
  #select(-colum_to_exclude)%>%
  bind_rows(avg_cost_verra)

# calculate the final columns to be selected!
#  a scenario of no mitigation --
# keep that one as the avg ref lines dont include a mitigation scenario!

avg_values_no_mitigation <- combined_new_df_avg_values %>%
  mutate(
    # leakage is accounted but not mitigated
    mitigation_strategy = "no_mitigation",
    mitigation_costs_reais_50y = 0,
    mitigation_scale = "no_mitigation",
    approach = "no_mitigation") %>%
  distinct()

# add column with final carbon balance based on the scenario
## NAO TEM A COLUNA "forgone_CO2_discounted"!! da onde ela vem, oq significa!!
## eh o coc da mitigacao, excluir, nao precisa!!

avg_values_no_mitigation <- avg_values_no_mitigation %>%
  mutate(
    # with carbon corrected for the SCC!
    final_CO2_balance_corrected =
      case_when(
        # if ignoring, then there is no mitigation but no discounting
        # so it should have the same balance as mitigation
        # (but the cost aspect is different!)
        #approach == "ignoring"~net_CO2_corrected_mitigation, 
        # if no mitigation too
        approach == "no_mitigation"~net_CO2_corrected_no_mitigation,
        # there is mitigation in this case!
        #approach == "intensification"~net_CO2_corrected_mitigation
        ),
    # no correction for the SCC 
    final_CO2_balance_non_corrected =  
      case_when(
        #approach == "ignoring"~net_CO2_no_correction_mitigation,
        approach == "no_mitigation"~net_CO2_no_correction_no_mitigation,
        #approach == "intensification"~net_CO2_no_correction_mitigation
        ),
    # add the carbon balance minus the carbon opportunity costs of restoration
    # from mitigation areas (for the rest there is no forgone OC!)
    # actually, there should be some forgone OC for the no_mitigation too? for where
    # it leaked?? think about it!
    final_CO2_balance_corrected_minus_COC =
      case_when(
        #approach == "ignoring"~final_CO2_balance_corrected,
        approach == "no_mitigation"~final_CO2_balance_corrected,
        #approach == "intensification"~final_CO2_balance_corrected - forgone_CO2_discounted
        ),
    final_CO2_balance_non_corrected_minus_COC =
      case_when(
        #approach == "ignoring"~final_CO2_balance_non_corrected,
        approach == "no_mitigation"~final_CO2_balance_non_corrected,
        #approach == "intensification"~final_CO2_balance_non_corrected - forgone_CO2_discounted
        )
    
  )

# calculating total project costs and final carbon balance!
avg_values_no_mitigation<- avg_values_no_mitigation%>%
  mutate(project_costs = total_restoration_costs_reais_2023+
           total_land_value_reais_ha+
           mitigation_costs_reais_50y,
         CO2_cost_noCOC =  project_costs/final_CO2_balance_corrected,
         CO2_cost_COC =  project_costs/final_CO2_balance_corrected_minus_COC)%>%
  ungroup()

# adjusting currency and inflation!

IPCA_2021 <- 1.0579
IPCA_2024 <- 1.1583
IPCA_2023 <- 1.0462*1.0579
USD_rate_2021 = 5.3955
USD_BR =5 # stefan

avg_values_no_mitigation <- avg_values_no_mitigation %>%
  mutate(
    
    # deflate
    # mitigation costs
    mitigation_costs_USD_50y_2021 = mitigation_costs_reais_50y,
    # restoration costs
    total_land_value_USD_2021 = total_land_value_reais_ha/IPCA_2023, 
    total_restoration_costs_USD_2021 = total_restoration_costs_reais_2023/IPCA_2024,
    # convert to USD
    mitigation_costs_USD_50y_2021 =  mitigation_costs_USD_50y_2021/USD_BR,
    total_restoration_costs_USD_2021 = total_restoration_costs_USD_2021/USD_rate_2021,
    total_land_value_USD_2021 = total_land_value_USD_2021/USD_rate_2021
  )

# total costs and cost/tonne in USD
avg_values_no_mitigation<- avg_values_no_mitigation %>%
  mutate(project_costs_USD = total_restoration_costs_USD_2021+
           total_land_value_USD_2021+
           mitigation_costs_USD_50y_2021,
         CO2_cost_COC_USD =  project_costs_USD/final_CO2_balance_corrected_minus_COC,
         CO2_cost_NOCOC_USD =  project_costs_USD/final_CO2_balance_corrected)%>%
  ungroup()

# now I have the columns to keep (but need to include some extra!!)

avg_values_no_mitigation_tokeep <- avg_values_no_mitigation %>% select(any_of(c(keep,"disc","leakage")))

# remove temp dfs

rm(df1,df2,avg_values_no_mitigation,high_low_3,combined_new_df,avg_cost_2,avg_cost_4,
   avg_cost_3,avg_cost_verra,combined_new_df_avg_values,avg_cost_4_30,avg_cost_4_100,
   avg_cost_2_30,avg_cost_2_100,high_low_3_verra,high_low_3_abacus,avg_values_no_mitigation_tokeep)


# I have left df -- the values with high, low and random at 3%, 100% leakage
# avg_values_no_mitigation_tokeep -- the values with random with 2-4%, and also
# with l=30%

# combine them into df (df has high and low + avg for verra)

df <- rbind(df,avg_values_no_mitigation_tokeep%>%rename(CO2_cost_USD = CO2_cost_COC_USD))


# check if its all good (verra still have more check why!)

#table(df$disc,df$param,df$name_biome,df$yield_carbon_ratio,df$mitigation_strategy)


#-------------------------------------------------------------------------------
# high and low with different rates 
#-------------------------------------------------------------------------------

# abacus
#2%
SCC2_AM <- readRDS("results_with_mitigation/mitigation_costs_country_scale_intensification_optimize_carbon_ABACUS_amazonia_yield_prj_SCC_2_round1_wirsenius.rds")
SCC2_AF <- readRDS("results_with_mitigation/mitigation_costs_country_scale_intensification_optimize_carbon_ABACUS_mataatlantica_yield_prj_SCC_2_round1_wirsenius.rds")
# 4%
SCC4_AM <- readRDS("results_with_mitigation/mitigation_costs_country_scale_intensification_optimize_carbon_ABACUS_amazonia_yield_prj_SCC_4_round1_wirsenius.rds")
SCC4_AF <- readRDS("results_with_mitigation/mitigation_costs_country_scale_intensification_optimize_carbon_ABACUS_mataatlantica_yield_prj_SCC_4_round1_wirsenius.rds")

# verra
# 2% 
SCC2_AM_verra <- readRDS("results_with_mitigation/mitigation_costs_country_scale_intensification_optimize_carbon_Verra_amazonia_yield_prj_SCC_3_round1_wirsenius.rds")
SCC2_AF_verra <- readRDS("results_with_mitigation/mitigation_costs_country_scale_intensification_optimize_carbon_Verra_mataatlantica_yield_prj_SCC_3_round1_wirsenius.rds")

# 4% (check as this is not created yet!)
SCC4_AM_verra <- readRDS("results_with_mitigation/mitigation_costs_country_scale_intensification_optimize_carbon_Verra_amazonia_yield_prj_SCC_4_round1_wirsenius.rds")
SCC4_AF_verra <- readRDS("results_with_mitigation/mitigation_costs_country_scale_intensification_optimize_carbon_Verra_mataatlantica_yield_prj_SCC_4_round1_wirsenius.rds")

# adding columns
# abacus 2
SCC2_AM <- SCC2_AM %>% mutate(disc = "2%",leakage = "100%")
SCC2_AF <- SCC2_AF %>% mutate(disc = "2%",leakage = "100%")
# abacus 4
SCC4_AM <- SCC4_AM %>% mutate(disc = "4%",leakage = "100%")
SCC4_AF <- SCC4_AF %>% mutate(disc = "4%",leakage = "100%")

# verra 2
SCC2_AM_verra <- SCC2_AM_verra %>% mutate(disc = "2%",leakage = "30%")
SCC2_AF_verra <- SCC2_AF_verra %>% mutate(disc = "2%",leakage = "30%")

# verra 4
SCC4_AM_verra <- SCC4_AM_verra %>% mutate(disc = "4%",leakage = "30%")
SCC4_AF_verra <- SCC4_AF_verra %>% mutate(disc = "4%",leakage = "30%")


# pile everything
df3 <- rbind(
  SCC2_AM, # abacus 2 AM
  SCC2_AF, # abacus 2 AF
  SCC4_AM, # abacus 4 AM
  SCC4_AF, # abacus 4 AF
  SCC2_AM_verra, # verra 2 AM
  SCC2_AF_verra, # verra 2 AF
  SCC4_AM_verra, # verra 4 AM
  SCC4_AF_verra  # verra 4 AF
  )


unique(df3$mitigation_strategy) # no no action scenario yet!

# check
#table(df3$param,df3$name_biome,df3$yield_carbon_ratio,df3$disc)

# calculate final c costs

projects_no_mitigation_country <- df3 %>%
  mutate(
    # leakage is accounted but not mitigated
    mitigation_strategy = "no_mitigation",
    mitigation_costs_reais_50y = 0,
    mitigation_scale = "no_mitigation",
    approach = "no_mitigation") %>%
  distinct()

# add this to df3 (adding no mitigation)

df4 <- rbind(df3,projects_no_mitigation_country)

# add column with final carbon balance based on the scenario
df5 <- df4 %>%
  mutate(
    # with carbon corrected for the SCC!
    final_CO2_balance_corrected =
      case_when(
        # if ignoring, then there is no mitigation but no discounting
        # so it should have the same balance as mitigation
        # (but the cost aspect is different!)
        #approach == "ignoring"~net_CO2_corrected_mitigation, 
        # if no mitigation too
        approach == "no_mitigation"~net_CO2_corrected_no_mitigation,
        # there is mitigation in this case!
        approach == "intensification"~net_CO2_corrected_mitigation),
    # no correction for the SCC 
    final_CO2_balance_non_corrected =  
      case_when(
        #approach == "ignoring"~net_CO2_no_correction_mitigation,
        approach == "no_mitigation"~net_CO2_no_correction_no_mitigation,
        approach == "intensification"~net_CO2_no_correction_mitigation),
    # add the carbon balance minus the carbon opportunity costs of restoration
    # from mitigation areas (for the rest there is no forgone OC!)
    # actually, there should be some forgone OC for the no_mitigation too? for where
    # it leaked?? think about it!
    final_CO2_balance_corrected_minus_COC =
      case_when(
        #approach == "ignoring"~final_CO2_balance_corrected,
        approach == "no_mitigation"~final_CO2_balance_corrected,
        approach == "intensification"~final_CO2_balance_corrected - forgone_CO2_discounted),
    final_CO2_balance_non_corrected_minus_COC =
      case_when(
        #approach == "ignoring"~final_CO2_balance_non_corrected,
        approach == "no_mitigation"~final_CO2_balance_non_corrected,
        approach == "intensification"~final_CO2_balance_non_corrected - forgone_CO2_discounted)
    
  )



df5<- df5%>%
  mutate(project_costs = total_restoration_costs_reais_2023+
           total_land_value_reais_ha+
           mitigation_costs_reais_50y,
         CO2_cost_noCOC =  project_costs/final_CO2_balance_corrected,
         CO2_cost_COC =  project_costs/final_CO2_balance_corrected_minus_COC)%>%
  ungroup()


df5 <- df5 %>%
  mutate(
    
    # deflate
    # mitigation costs
    mitigation_costs_USD_50y_2021 = mitigation_costs_reais_50y,
    # restoration costs
    total_land_value_USD_2021 = total_land_value_reais_ha/IPCA_2023, 
    total_restoration_costs_USD_2021 = total_restoration_costs_reais_2023/IPCA_2024,
    # convert to USD
    mitigation_costs_USD_50y_2021 =  mitigation_costs_USD_50y_2021/USD_BR,
    total_restoration_costs_USD_2021 = total_restoration_costs_USD_2021/USD_rate_2021,
    total_land_value_USD_2021 = total_land_value_USD_2021/USD_rate_2021
  )

df5<- df5 %>%
  mutate(project_costs_USD = total_restoration_costs_USD_2021+
           total_land_value_USD_2021+
           mitigation_costs_USD_50y_2021,
         CO2_cost_COC_USD =  project_costs_USD/final_CO2_balance_corrected_minus_COC,
         CO2_cost_NOCOC_USD =  project_costs_USD/final_CO2_balance_corrected)%>%
  ungroup()

df5_filt <- df5 %>% select(any_of(c(keep,"disc","leakage")))

#-------------------------------------------------------------------------------
# aqui junta tudo!!!
#-------------------------------------------------------------------------------

merged_df <- rbind(df5_filt%>%rename(CO2_cost_USD = "CO2_cost_COC_USD" ),df)

# check
#table(merged_df$leakage,merged_df$disc,merged_df$name_biome,merged_df$yield_carbon_ratio,merged_df$mitigation_strategy)


saveRDS(merged_df,"output_tables/testing_scc_leakage_sensitivity.rds")



# generate the values to build a table!!
# merged_df %>%
#   group_by(yield_carbon_ratio,name_biome,disc,mitigation_strategy)%>%
#   summarise(cost_co2 = median(CO2_cost_USD,na.rm=T))

# median value low leakage SCC 2%

# merged_df %>%
#   filter(
#     yield_carbon_ratio =="max",
#     #disc =="2%",
#     mitigation_strategy=="no_mitigation",
#     CO2_cost_USD>0,
#     leakage == "100%"
#          )%>%
#   group_by(yield_carbon_ratio,name_biome,disc)%>%
#   summarise(median(CO2_cost_USD,na.rm = T))


