# load packages ----------------------------------------------------------------

library(tidyverse)

#-------------------------------------------------------------------------------

# get AGB and BGB estimates and calculate delta BGB emission share so I can add
# the the leakage calculation!

carbon_avgs <- readRDS("output_tables/reviewers_request/carbon_share_AGB_BGB_SOC_by_biome_cover.rds")

# rearange the data 

carbon_w <- carbon_avgs %>%
  select(biome, cover_class, carbon_pool, mean) %>%
  pivot_wider(
    names_from = c(carbon_pool, cover_class),
    values_from = mean
  )

# calculate deltas
carbon_w <- carbon_w %>%
  mutate(
    AGB_loss = AGB_forest - AGB_pasture,
    BGB_loss = BGB_forest - BGB_pasture,
    BGB_AGB_ratio = round((BGB_loss / AGB_loss),2),
    AGB_BGB_multiplier = 1 + BGB_AGB_ratio
  )%>%
  select(-c(SOC_forest,SOC_pasture))

# save table

saveRDS(carbon_w,"output_tables/reviewers_request/BGB_multiplier.rds")

