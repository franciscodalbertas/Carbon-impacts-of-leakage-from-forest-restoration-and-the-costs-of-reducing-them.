# load packages ----------------------------------------------------------------

library(tidyverse)

#-------------------------------------------------------------------------------

# i can open all mitigation sites , rank then based on quantiles and get
# the good places using the top of the distribution...it should be something like
# production_gained_to_carbon_forgone...this should be the same as selecting the
# top of the distribution: high yield gaps, low carbon capacity. 

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

# select quantile bins...i would probably need a higher level
# of intensification...i can estimate that!
# should I calculate all or be more restricted!

top_mitigation <- mitigation%>%
  filter(percentile_bin%in%
           c("[70,80)",
             "[60,70)"
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

# pra fazer separado, tem q calcular o cumsum separado tb!!

# get total mitigation capacity

total_mitigation <- max(top_mitigation1$cum_production_gain)


# read the total fp data

fp <- readRDS("output_tables/hide/total_restorable_area_bin_20_30.rds")

# order and calculate cumsum...but how do deal with both biomes??
# here I am not ranking them separately and it selected more projects
# in the AF as the distribution is more skewed there!
# i should try to make a balance and aim to:
# how much could be done in each, separately, or in both maximize
# the area restored (or aim for similar n of projecs in both?)
# maybe the way i did is already good...get the lower possible y/c
# but also show how much could be done if I was to do each separately

fp2 <- fp %>%
  arrange(summed_value)%>%
  mutate(cum_fp=cumsum(FP_projected))

# select projects until the total mitigation is achieved

fp_mitigation_possible <- fp2%>%
  filter(cum_fp <=total_mitigation)

# just AF
fp_mitigation_possible_af <- fp2%>%
  filter(
    name_biome=="Mata Atlântica",
    cum_fp <=total_mitigation
    )

# just AM

fp_mitigation_possible_am <- fp2%>%
  filter(
    name_biome!="Mata Atlântica",
    cum_fp <=total_mitigation
  )
# calculate total area restorable

total_restorable <- fp_mitigation_possible %>%
  group_by(name_biome)%>%
  summarise(total_restorable_million_ha = sum(restor_area)/10^6,
            total_FP_pj_tonnes = sum(FP_projected)/1000)


total_restorable_af <- fp_mitigation_possible_af %>%
  #group_by(name_biome)%>%
  summarise(total_restorable_million_ha = sum(restor_area)/10^6,
            total_FP_pj_tonnes = sum(FP_projected)/1000)

total_restorable_am <- fp_mitigation_possible_am %>%
  #group_by(name_biome)%>%
  summarise(total_restorable_million_ha = sum(restor_area)/10^6,
            total_FP_pj_tonnes = sum(FP_projected)/1000)


# save the results...very cool!
# no costs associated! perhaps it would be worth getting some cost estimates

saveRDS(total_restorable,"output_tables/hide/total_restorable_with_mitigation.rds")

