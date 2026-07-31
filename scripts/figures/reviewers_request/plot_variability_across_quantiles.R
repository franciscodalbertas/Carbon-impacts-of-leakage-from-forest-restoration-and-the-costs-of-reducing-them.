# load packages ----------------------------------------------------------------

library(tidyverse)
library(purrr)
library(ggplot2)
library(ggpubr)

#-------------------------------------------------------------------------------

# load all projects

projects <- readRDS("input_tables/all_projects_with_deciles.rds")
projects$yield_carbon <- projects$yield_ha/projects$carbon_ha
names(projects)
# how to show variability ??

variables <- c(
  "yield_ha",
  "carbon_ha",
  "yield_carbon",
  "rest_cost_ha", 
  "land_value_ha"
)

# add a variable with yield_to_carbon and then show
# for each biome, the variance!

# I could add this as supplementary ? why not?
# this is for high leakage. I also need for low leakage
projects_long_high <- projects %>%
  filter(percentile_bin == "[70,80)") %>%
  select(coordinates, name_biome, all_of(variables)) %>%
  pivot_longer(
    cols = all_of(variables),
    names_to = "variable",
    values_to = "value"
  ) %>%
  filter(!is.na(value))

projects_long_low <- projects %>%
  filter(percentile_bin == "[20,30)") %>%
  select(coordinates, name_biome, all_of(variables)) %>%
  pivot_longer(
    cols = all_of(variables),
    names_to = "variable",
    values_to = "value"
  ) %>%
  filter(!is.na(value))

projects_long_average <- projects %>%
  select(coordinates, name_biome, all_of(variables)) %>%
  pivot_longer(
    cols = all_of(variables),
    names_to = "variable",
    values_to = "value"
  ) %>%
  filter(!is.na(value))

#Function for repeated subsampling:

get_project_pixels <- function(project_id) {
  project_pixels <- project_id %>%
    strsplit(";", fixed = TRUE) %>%
    unlist() %>%
    trimws() %>%
    gsub("\\s+", "", .) %>%
    unique()
  
  project_pixels[!is.na(project_pixels) & nzchar(project_pixels)]
}

select_non_overlapping_project_ids <- function(project_ids) {
  used_pixels <- character()
  selected_projects <- character()
  
  for (project_id in sample(project_ids)) {
    project_pixels <- get_project_pixels(project_id)
    
    if (length(project_pixels) > 0 && !any(project_pixels %in% used_pixels)) {
      selected_projects <- c(selected_projects, project_id)
      used_pixels <- c(used_pixels, project_pixels)
    }
  }
  
  selected_projects
}

sample_stability <- function(data, n_repetitions = 1000) {
  
  project_ids <- unique(data$coordinates)
  non_overlapping_project_ids <- select_non_overlapping_project_ids(project_ids)
  n_total <- length(non_overlapping_project_ids)
  
  sample_sizes <- c(
    10, 100, 500, 1000, 2000, 3000, 4000, 5000, n_total
  ) %>%
    unique() %>%
    sort()
  
  # Remove sample sizes larger than the available number of projects
  sample_sizes <- sample_sizes[sample_sizes <= n_total]
  
  non_overlapping_data <- data %>%
    filter(coordinates %in% non_overlapping_project_ids)
  
  full_estimates <- non_overlapping_data %>%
    summarise(
      median = median(value, na.rm = TRUE),
      mean = mean(value, na.rm = TRUE)
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = "statistic",
      values_to = "full_estimate"
    )
  
  map_dfr(sample_sizes, function(n_sample) {
    
    # Only one possible sample when all projects are included
    n_reps <- if (n_sample == n_total) 1 else n_repetitions
    
    map_dfr(seq_len(n_reps), function(iteration) {
      
      sampled_projects <- sample(
        non_overlapping_project_ids,
        size = n_sample,
        replace = FALSE
      )
      
      sample_estimates <- non_overlapping_data %>%
        filter(coordinates %in% sampled_projects) %>%
        summarise(
          median = median(value, na.rm = TRUE),
          mean = mean(value, na.rm = TRUE)
        ) %>%
        pivot_longer(
          cols = everything(),
          names_to = "statistic",
          values_to = "estimate"
        )
      
      sample_estimates %>%
        left_join(full_estimates, by = "statistic") %>%
        mutate(
          sample_size = n_sample,
          iteration = iteration,
          relative_difference = 100 *
            (estimate - full_estimate) /
            abs(full_estimate)
        ) %>%
        select(
          sample_size,
          iteration,
          statistic,
          estimate,
          full_estimate,
          relative_difference
        )
    })
  })
}
sampling_simulation_path <- "input_tables/sampling_simulation.rds"

if (file.exists(sampling_simulation_path)) {
  stability_results <- readRDS(sampling_simulation_path)
} else {
  # Apply it separately to each biome and variable:
  set.seed(123)
  
  stability_results_high <- projects_long_high %>%
    group_by(name_biome, variable) %>%
    group_modify(
      ~ sample_stability(.x, n_repetitions = 500)
    ) %>%
    ungroup()
  
  set.seed(123)
  
  stability_results_low <- projects_long_low %>%
    group_by(name_biome, variable) %>%
    group_modify(
      ~ sample_stability(.x, n_repetitions = 500)
    ) %>%
    ungroup()
  
  # add column with leakage scenario and combine
  
  stability_results_high$leakage_risk <- "high"
  stability_results_low$leakage_risk <- "low"
  
  stability_results <- rbind(stability_results_high,stability_results_low)
  
  saveRDS(stability_results,sampling_simulation_path)
}

if (!("average" %in% stability_results$leakage_risk)) {
  set.seed(123)
  
  stability_results_average <- projects_long_average %>%
    group_by(name_biome, variable) %>%
    group_modify(
      ~ sample_stability(.x, n_repetitions = 500)
    ) %>%
    ungroup()
  
  stability_results_average$leakage_risk <- "average"
  
  stability_results <- rbind(stability_results, stability_results_average)
  
  saveRDS(stability_results,sampling_simulation_path)
}

# stability_results%>%
#   group_by(leakage_risk)%>%
#   summarise(mean(relative_difference),
#             median(relative_difference),
#             quantile(relative_difference,0.2))



#Summarise the simulations and plot!!!:

stability_summary <- stability_results %>%
  group_by(
    name_biome,
    #percentile_bin,
    leakage_risk,
    variable,
    statistic,
    sample_size
  ) %>%
  summarise(
    central_difference = median(
      relative_difference,
      na.rm = TRUE
    ),
    lower = quantile(
      relative_difference,
      0.025,
      na.rm = TRUE
    ),
    upper = quantile(
      relative_difference,
      0.975,
      na.rm = TRUE
    ),
    .groups = "drop"
  )
# plot mean
plot_data <- stability_summary %>%
  filter(
    statistic == "mean",
    variable %in% c(
      "yield_carbon",
      "land_value_ha",
      "rest_cost_ha"
    )
  ) %>%
  mutate(
    variable = factor(
      variable,
      levels = c(
        "yield_carbon",
        "land_value_ha",
        "rest_cost_ha"
      )
    ),
    name_biome = if_else(name_biome=="Amazônia","Amazon",
                           "Atlantic Forest")
  )

plot_data$name_biome <- factor(plot_data$name_biome,levels = c("Atlantic Forest","Amazon"))

facet_labels <- c(
  yield_carbon = "Forgone-production-to-carbon-gained",
  land_value_ha = "Land value",
  rest_cost_ha = "Restoration costs"
)

# average
average <- ggplot(
  plot_data %>% filter(leakage_risk=="average"),
  aes(
    x = sample_size,
    y = central_difference,
    colour = name_biome,
    fill = name_biome
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "grey40"
  ) +
  geom_hline(
    yintercept = c(-5, 5),
    linetype = "dashed",
    colour = "grey40"
  ) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.15,
    colour = NA
  ) +
  facet_wrap(
    ~ variable,
    scales = "free_y",
    labeller = as_labeller(facet_labels)
  ) +
  scale_x_log10(
    breaks = c(10, 100, 1000, 10000),
    labels = scales::comma
  ) +
  labs(
    x = "Number of projects sampled",
    y = paste0(
      "Relative difference from the full-set mean\n",
      "across the entire distribution (%)"
    ),
    colour = "Biome",
    fill = "Biome"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 7),
    strip.background = element_blank(),
    strip.text = element_text(face = "plain"),
    panel.grid.minor = element_blank()
  )+
  ggtitle("Across the entire distribution")

# high
high <- ggplot(
  plot_data %>% filter(leakage_risk=="high"),
  aes(
    x = sample_size,
    y = central_difference,
    colour = name_biome,
    fill = name_biome
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "grey40"
  ) +
  geom_hline(
    yintercept = c(-5, 5),
    linetype = "dashed",
    colour = "grey40"
  ) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.15,
    colour = NA
  ) +
  facet_wrap(
    ~ variable,
    scales = "free_y",
    labeller = as_labeller(facet_labels)
  ) +
  scale_x_log10(
    breaks = c(10, 100, 1000, 10000),
    labels = scales::comma
  ) +
  labs(
    x = "Number of projects sampled",
    y = paste0(
      "Relative difference from the full-set mean\n",
      "within the 70–80th percentile bin (%)"
    ),
    colour = "Biome",
    fill = "Biome"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 7),
    strip.background = element_blank(),
    strip.text = element_text(face = "plain"),
    panel.grid.minor = element_blank()
  )+
  ggtitle("High leakage")

#low

low <- ggplot(
  plot_data %>% filter(leakage_risk=="low"),
  aes(
    x = sample_size,
    y = central_difference,
    colour = name_biome,
    fill = name_biome
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "grey40"
  ) +
  geom_hline(
    yintercept = c(-5, 5),
    linetype = "dashed",
    colour = "grey40"
  ) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.15,
    colour = NA
  ) +
  facet_wrap(
    ~ variable,
    scales = "free_y",
    labeller = as_labeller(facet_labels)
  ) +
  scale_x_log10(
    breaks = c(10, 100, 1000, 10000),
    labels = scales::comma
  ) +
  labs(
    x = "Number of projects sampled",
    y = paste0(
      "Relative difference from the full-set mean\n",
      "within the 20–30th percentile bin (%)"
    ),
    colour = "Biome",
    fill = "Biome"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 7),
    strip.background = element_blank(),
    strip.text = element_text(face = "plain"),
    panel.grid.minor = element_blank()
  )+
  ggtitle("low leakage")

#low


panel <- ggarrange(average,low,high,nrow = 3,common.legend = T)


ggsave(plot = panel,"figures/response_reviewers/sensitivity_sample_size.png",
       height = 18, width = 16, units = "cm")
