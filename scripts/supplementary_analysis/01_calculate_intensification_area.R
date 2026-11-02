# load packages ----------------------------------------------------------------

library(tidyverse)

#-------------------------------------------------------------------------------

# load mitigation sites across the country

mitigation <- readRDS("mitigation_inputs/precomputed_candidates/country_candidates_round1_wirsenius.rds")

mitigation <- mitigation %>%
  mutate(
    percentile = percent_rank(delta_y_c) * 100,
    percentile_bin = cut(
      percentile,
      breaks = seq(0, 100, by = 10),
      include.lowest = TRUE,
      right = FALSE
    )
  )

# select quantile bins.

top_mitigation <- mitigation%>%
  filter(percentile_bin%in%
           c("[70,80)"
             #"[60,70)"
             #"[50,60)",
             #"[40,50)",
             #"[30,40)",
             #"[20,30)"
             )
         )

# order the data and calculate cumulative production gain

top_mitigation1 <- top_mitigation%>%
  arrange(desc(delta_y_c))%>%
  mutate(cum_production_gain=cumsum(production_gain_kg))

# get total mitigation capacity

total_mitigation <- max(top_mitigation1$cum_production_gain)


# read the total fp data

fp <- readRDS("output_tables/hide/total_restorable_area_bin_20_30.rds")


# order and calculate cumsum

fp2 <- fp %>%
  arrange(summed_value)%>%
  mutate(cum_fp=cumsum(capped_FP))

# select projects until the total mitigation is achieved

fp_mitigation_possible <- fp2%>%
  filter(cum_fp <=total_mitigation)

# just AF
fp_mitigation_possible_af <- fp2%>%
  filter(
    name_biome=="Mata Atlântica",
    cum_fp <=total_mitigation
    )


# calculate total area restorable

total_restorable <- fp_mitigation_possible %>%
  group_by(name_biome)%>%
  summarise(total_restorable_million_ha = sum(pasture_area_ha)/10^6,
            total_FP_pj_tonnes = sum(capped_FP)/1000)



# save the results

saveRDS(total_restorable,"output_tables/reviewers_request/total_restorable_with_mitigation.rds")

