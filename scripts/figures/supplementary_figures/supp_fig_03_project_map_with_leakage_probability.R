# Integrated map for the reviewers' request:
# B) all potential projects, C) sampled projects, D) local leakage pixels over
# probability of conversion, and E) example local mitigation.

library(tidyverse)
library(sf)
library(terra)
library(grid)
library(ggpubr)
source("functions/15_mitigation_precomputed_candidates.R")

#-------------------------------------------------------------------------------

selected_code_immediate <- "290009"
selected_region_label <- "Eunapolis - Porto Seguro, Bahia"
selected_local_domain_title <- "Local domain: Eunapolis - Porto Seguro"
pixel_size <- 1000

local_panel_levels <- c(
  "A) Local domain",
  "B) All potential projects",
  "C) Sampled projects",
  "D) Leakage pixels",
  "E) Example local mitigation"
)

line_legend_levels <- c(
  "All potential projects",
  "Sampled projects",
  "Leakage pixels",
  "Selected mitigation pixels"
)

line_legend_colours <- c(
  "All potential projects" = "#4E79A7",
  "Sampled projects" = "black",
  "Leakage pixels" = "#D73027",
  "Selected mitigation pixels" = "#009E73"
)

resolve_existing_path <- function(paths) {
  existing <- paths[file.exists(paths)]

  if (length(existing) == 0) {
    stop("Could not find any of these files: ", paste(paths, collapse = "; "))
  }

  existing[1]
}

project_crs <- terra::crs(
  terra::rast("variables/beef_yield_kg_ha_2021_albers_1km.tif")
)

project_points_from_table <- function(project_table, project_set, crs = project_crs) {
  project_table <- as.data.frame(project_table)

  if (all(c("x", "y") %in% names(project_table))) {
    return(
      project_table %>%
        transmute(
          project_id = coordinates,
          project_set = project_set,
          x = as.numeric(x),
          y = as.numeric(y)
        ) %>%
        filter(!is.na(x), !is.na(y))
    )
  }

  geometry_col <- names(project_table)[map_lgl(project_table, inherits, what = "sfc")]

  if (length(geometry_col) > 0) {
    project_sf <- st_as_sf(project_table, sf_column_name = geometry_col[1], crs = crs)
    xy <- st_coordinates(project_sf)

    return(
      project_sf %>%
        st_drop_geometry() %>%
        mutate(
          project_set = project_set,
          x = xy[, "X"],
          y = xy[, "Y"]
        ) %>%
        transmute(
          project_id = coordinates,
          project_set,
          x,
          y
        ) %>%
        filter(!is.na(x), !is.na(y))
    )
  }

  project_table %>%
    mutate(coord_pairs = strsplit(coordinates, ";")) %>%
    unnest(coord_pairs) %>%
    separate(coord_pairs, into = c("x", "y"), sep = ",", convert = TRUE) %>%
    transmute(
      project_id = coordinates,
      project_set = project_set,
      x = as.numeric(x),
      y = as.numeric(y)
    ) %>%
    filter(!is.na(x), !is.na(y))
}

pixel_points_to_outlines <- function(pixel_points,
                                     x_col = "x",
                                     y_col = "y",
                                     crs = project_crs,
                                     pixel_size = 1000,
                                     group_cols = NULL) {
  half_pixel <- pixel_size / 2

  pixel_geometry <- map2(
    pixel_points[[x_col]],
    pixel_points[[y_col]],
    \(x, y) {
      st_polygon(list(matrix(
        c(
          x - half_pixel, y - half_pixel,
          x + half_pixel, y - half_pixel,
          x + half_pixel, y + half_pixel,
          x - half_pixel, y + half_pixel,
          x - half_pixel, y - half_pixel
        ),
        ncol = 2,
        byrow = TRUE
      )))
    }
  )

  pixel_sf <- st_sf(
    pixel_points,
    geometry = st_sfc(pixel_geometry, crs = crs)
  )

  if (!is.null(group_cols)) {
    pixel_sf <- pixel_sf %>%
      group_by(across(all_of(group_cols))) %>%
      summarise(.groups = "drop")
  }

  pixel_sf
}

project_points_to_outlines <- function(project_points,
                                       crs = project_crs,
                                       pixel_size = 1000) {
  pixel_points_to_outlines(
    pixel_points = project_points,
    crs = crs,
    pixel_size = pixel_size,
    group_cols = c("project_id", "project_set")
  )
}

mitigation_pixels_to_sf <- function(mitigation_pixels,
                                    crs = project_crs,
                                    pixel_size = 1000) {
  mitigation_pixels %>%
    transmute(
      project_id = paste(x_mitig, y_mitig, sep = "_"),
      project_set = factor("Selected mitigation pixels", levels = line_legend_levels),
      x = as.numeric(x_mitig),
      y = as.numeric(y_mitig)
    ) %>%
    project_points_to_outlines(crs = crs, pixel_size = pixel_size)
}

target_fp_column <- function(project) {
  if ("total_FP_leaked" %in% names(project)) {
    project$total_FP_leaked
  } else {
    project$FP_leaked
  }
}

build_local_leakage_selection <- function(project,
                                          selected_immediate_region) {
  fc <- rast("variables/forest_cover_albers_1km_2021.tif")
  conversion_probability <- rast("variables/Williams_pred_to_increase_pasture_albers_1km_pj.tif")
  landvalue <- rast("variables/land_value_2023_albers_1km.tif")
  yield <- rast("variables/mean_yield_10km_moving_window.tif")

  target <- selected_immediate_region %>%
    st_transform(crs = crs(fc))

  fc_m <- fc %>%
    crop(target) %>%
    mask(target) %>%
    round(2)

  fc_mask <- ifel(fc_m == 0, NA, 1)
  forest_area <- fc_m * 100

  conversion_m <- conversion_probability %>%
    crop(target) %>%
    mask(target)

  landvalue_m <- landvalue %>%
    crop(target) %>%
    mask(target)

  yield_m <- yield %>%
    crop(target) %>%
    mask(target)

  candidate_rasters <- c(
    landvalue_m * fc_mask,
    conversion_m * fc_mask,
    forest_area,
    yield_m
  )

  values_df <- as.data.frame(candidate_rasters, na.rm = TRUE, xy = TRUE)
  colnames(values_df) <- c(
    "x",
    "y",
    "land_value",
    "prob_clearance",
    "forest_area",
    "yield_kg_ha"
  )

  arranged_df <- values_df %>%
    mutate(beef_pot_prod_kg = forest_area * yield_kg_ha) %>%
    filter(
      !is.na(prob_clearance),
      !is.na(land_value),
      !is.na(beef_pot_prod_kg),
      beef_pot_prod_kg > 0
    ) %>%
    arrange(desc(prob_clearance), land_value) %>%
    mutate(cumulative_production_kg = cumsum(beef_pot_prod_kg))

  if (nrow(arranged_df) == 0) {
    stop("No candidate leakage pixels were found in the selected local domain.")
  }

  target_fp <- target_fp_column(project)
  last_included_index <- which(arranged_df$cumulative_production_kg >= target_fp)[1]

  if (is.na(last_included_index)) {
    warning("Local candidate pool does not meet target FP for project ", project$coordinates)
    last_included_index <- nrow(arranged_df)
  }

  selected <- arranged_df[seq_len(last_included_index), ] %>%
    mutate(
      previous_production_kg = lag(cumulative_production_kg, default = 0),
      pixel_fraction = if_else(
        row_number() == n() & cumulative_production_kg > target_fp,
        pmax((target_fp - previous_production_kg) / beef_pot_prod_kg, 0),
        1
      ),
      area_deforested_ha = forest_area * pixel_fraction,
      project_id = project$coordinates,
      project_set = factor("Leakage pixels", levels = line_legend_levels)
    )

  probability_df <- values_df %>%
    mutate(beef_pot_prod_kg = forest_area * yield_kg_ha) %>%
    filter(
      !is.na(prob_clearance),
      !is.na(land_value),
      !is.na(beef_pot_prod_kg),
      beef_pot_prod_kg > 0
    ) %>%
    select(x, y, prob_clearance)

  list(
    probability = probability_df %>%
      mutate(panel = factor("D) Leakage pixels", levels = local_panel_levels)),
    selected = selected,
    target_fp = target_fp,
    selected_area_ha = sum(selected$area_deforested_ha, na.rm = TRUE)
  )
}

#-------------------------------------------------------------------------------

projects <- readRDS("project_location/project_location_round1_sampled1000projects.rds")

projects_AF <- as.data.frame(projects) %>%
  filter(as.character(code_immediate) == selected_code_immediate)

if (nrow(projects_AF) == 0) {
  stop("No sampled projects were found for code_immediate ", selected_code_immediate, ".")
}

all_projects <- readRDS("input_tables/all_potential_projects_AF.rds")
all_projects_eunapolis <- all_projects %>%
  filter(as.character(code_immediate) == selected_code_immediate)

immediate_regions <- readRDS("input_tables/imediate_regions_am_af.rds") %>%
  st_transform(crs = project_crs)

selected_immediate_region <- immediate_regions %>%
  filter(as.character(code_immediate) == selected_code_immediate)

if (nrow(selected_immediate_region) == 0) {
  stop("Could not find code_immediate ", selected_code_immediate,
       " in input_tables/imediate_regions_am_af.rds.")
}

selected_immediate_region_panels <- map_dfr(
  local_panel_levels,
  \(panel_name) selected_immediate_region %>% mutate(panel = panel_name)
) %>%
  mutate(panel = factor(panel, levels = local_panel_levels))

brazil_outline <- readRDS("input_tables/br.rds") %>%
  st_transform(crs = 4674)

atlantic_forest_outline <- readRDS("input_tables/am_af.rds") %>%
  filter(str_detect(name_biome, "Atl")) %>%
  st_transform(crs = 4674)

selected_immediate_region_locator <- selected_immediate_region %>%
  st_transform(crs = 4674)

all_project_outlines <- all_projects_eunapolis %>%
  project_points_from_table("All potential projects") %>%
  mutate(project_set = factor(project_set, levels = line_legend_levels)) %>%
  project_points_to_outlines()

sampled_project_outlines <- projects_AF %>%
  project_points_from_table("Sampled projects") %>%
  mutate(project_set = factor(project_set, levels = line_legend_levels)) %>%
  project_points_to_outlines()

all_panel <- all_project_outlines %>%
  mutate(panel = factor("B) All potential projects", levels = local_panel_levels))

sampled_context_panel <- all_project_outlines %>%
  mutate(panel = factor("C) Sampled projects", levels = local_panel_levels))

sampled_panel <- sampled_project_outlines %>%
  mutate(panel = factor("C) Sampled projects", levels = local_panel_levels))

set.seed(123)

abacus_local_path <- resolve_existing_path(c(
  "mitigation_inputs/inputs_for_mitigation_leakage_abacus_local_scale_yield_prj_high_low_leakage_SCC_3_round1.rds",
  "mitigation_inputs/hide/inputs_for_mitigation_leakage_abacus_local_scale_yield_prj_high_low_leakage_SCC_3_round1.rds"
))

abacus_local_scc3 <- readRDS(abacus_local_path)

abacus_local_scc3_eunapolis <- abacus_local_scc3 %>%
  filter(as.character(code_immediate) == selected_code_immediate) %>%
  distinct(coordinates, .keep_all = TRUE)

if (nrow(abacus_local_scc3_eunapolis) == 0) {
  stop(
    "No projects were found for code_immediate ", selected_code_immediate,
    " in ", abacus_local_path, "."
  )
}

example_project <- abacus_local_scc3_eunapolis %>%
  slice_sample(n = 1)

local_leakage <- build_local_leakage_selection(
  project = example_project,
  selected_immediate_region = selected_immediate_region
)

candidate_tables <- ensure_mitigation_candidate_tables(
  round = "round1",
  intensification_model = NULL,
  intensification_name = "wirsenius"
)

trimmed_pools <- precompute_trimmed_pools(
  candidates_am_af = candidate_tables$am_af,
  candidates_country = candidate_tables$country,
  projects = abacus_local_scc3
)

example_mitigation_pixels <- select_mitigation_sites_for_project(
  trimmed_pools = trimmed_pools,
  project = example_project,
  mitigation_scale = "local"
)

example_project_outline <- example_project %>%
  project_points_from_table("Sampled projects") %>%
  mutate(project_set = factor(project_set, levels = line_legend_levels)) %>%
  project_points_to_outlines()

example_project_leakage_panel <- example_project_outline %>%
  mutate(panel = factor("D) Leakage pixels", levels = local_panel_levels))

example_project_mitigation_panel <- example_project_outline %>%
  mutate(panel = factor("E) Example local mitigation", levels = local_panel_levels))

sampled_context_leakage_panel <- sampled_project_outlines %>%
  mutate(panel = factor("D) Leakage pixels", levels = local_panel_levels))

selected_leakage_outlines <- local_leakage$selected %>%
  pixel_points_to_outlines(
    x_col = "x",
    y_col = "y",
    crs = project_crs,
    pixel_size = pixel_size
  ) %>%
  mutate(
    project_set = factor(project_set, levels = line_legend_levels),
    panel = factor("D) Leakage pixels", levels = local_panel_levels)
  )

example_mitigation_outlines <- example_mitigation_pixels %>%
  mitigation_pixels_to_sf() %>%
  mutate(panel = factor("E) Example local mitigation", levels = local_panel_levels))

leakage_summary_table <- example_project %>%
  transmute(
    project_id = coordinates,
    total_FP_leaked_kg = total_FP_leaked,
    leakage_pixels = nrow(local_leakage$selected),
    leakage_area_ha = local_leakage$selected_area_ha,
    mitigation_pixels = nrow(example_mitigation_pixels),
    mitigation_area_ha = sum(example_mitigation_pixels$area_required, na.rm = TRUE)
  )

dir.create("figures/response_reviewers", recursive = TRUE, showWarnings = FALSE)

write_csv(
  leakage_summary_table,
  "figures/response_reviewers/project_map_with_leakage_probability_summary.csv"
)

saveRDS(
  leakage_summary_table,
  "figures/response_reviewers/project_map_with_leakage_probability_summary.rds"
)

locator_map_eunapolis <- ggplot() +
  geom_sf(
    data = brazil_outline,
    fill = "#F2F2F0",
    colour = "#6F6F6F",
    linewidth = 0.25
  ) +
  geom_sf(
    data = atlantic_forest_outline,
    fill = "#8BCF8F",
    colour = "#208A45",
    linewidth = 0.3,
    alpha = 0.35
  ) +
  geom_sf(
    data = selected_immediate_region_locator,
    fill = NA,
    colour = "#303030",
    linewidth = 0.45,
    alpha = 1
  ) +
  coord_sf(expand = FALSE) +
  labs(
    title = paste0("A) ", selected_local_domain_title)
  ) +
  theme_void(base_size = 9) +
  theme(
    plot.title = element_text(face = "bold", size = 10, hjust = 0.5, margin = margin(b = 4)),
    plot.margin = margin(2, 2, 2, 2)
  )

panel_data <- function(data, panel_name) {
  data %>%
    filter(panel == panel_name)
}

make_project_panel <- function(panel_name, show_legend = FALSE) {
  ggplot() +
    geom_raster(
      data = panel_data(local_leakage$probability, panel_name),
      aes(x = x, y = y, fill = prob_clearance)
    ) +
    geom_sf(
      data = panel_data(selected_immediate_region_panels, panel_name),
      fill = NA,
      colour = "#303030",
      linewidth = 0.45,
      inherit.aes = FALSE
    ) +
    geom_sf(
      data = panel_data(all_panel, panel_name),
      aes(colour = project_set),
      fill = NA,
      linewidth = 0.15,
      alpha = 0.55
    ) +
    geom_sf(
      data = panel_data(sampled_context_panel, panel_name),
      colour = "grey82",
      fill = NA,
      linewidth = 0.12,
      alpha = 0.45
    ) +
    geom_sf(
      data = panel_data(sampled_panel, panel_name),
      aes(colour = project_set),
      fill = NA,
      linewidth = 0.35,
      alpha = 0.95
    ) +
    geom_sf(
      data = panel_data(sampled_context_leakage_panel, panel_name),
      colour = "grey82",
      fill = NA,
      linewidth = 0.12,
      alpha = 0.45
    ) +
    geom_sf(
      data = panel_data(selected_leakage_outlines, panel_name),
      aes(colour = project_set),
      fill = NA,
      linewidth = 0.85,
      alpha = 0.95
    ) +
    geom_sf(
      data = panel_data(example_project_leakage_panel, panel_name),
      aes(colour = project_set),
      fill = NA,
      linewidth = 1.1,
      alpha = 0.95
    ) +
    geom_sf(
      data = panel_data(example_project_mitigation_panel, panel_name),
      aes(colour = project_set),
      fill = NA,
      linewidth = 0.85,
      alpha = 0.95
    ) +
    geom_sf(
      data = panel_data(example_mitigation_outlines, panel_name),
      aes(colour = project_set),
      fill = NA,
      linewidth = 0.85,
      alpha = 0.75
    ) +
    scale_fill_viridis_c(
      option = "viridis",
      name = "Probability of conversion",
      labels = scales::label_number(accuracy = 0.01)
    ) +
    scale_colour_manual(
      values = line_legend_colours,
      breaks = line_legend_levels,
      limits = line_legend_levels,
      drop = FALSE
    ) +
    coord_sf(expand = FALSE) +
    labs(
      title = panel_name,
      colour = NULL,
      x = NULL,
      y = NULL
    ) +
    guides(
      colour = guide_legend(
        order = 1,
        nrow = 2,
        byrow = TRUE,
        override.aes = list(linewidth = 0.8, alpha = 1)
      ),
      fill = guide_colourbar(
        order = 2,
        barwidth = unit(4.2, "cm"),
        barheight = unit(0.4, "cm"),
        title.position = "top"
      )
    ) +
    theme_minimal(base_size = 9) +
    theme(
      legend.position = if (show_legend) "bottom" else "none",
      legend.box = "vertical",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", size = 10, hjust = 0.5, margin = margin(b = 4)),
      plot.margin = margin(2, 2, 2, 2)
    )
}

legend_panel <- ggpubr::as_ggplot(
  ggpubr::get_legend(make_project_panel("D) Leakage pixels", show_legend = TRUE))
)

panel <- ggarrange(
  locator_map_eunapolis,
  make_project_panel("B) All potential projects"),
  make_project_panel("C) Sampled projects"),
  make_project_panel("D) Leakage pixels"),
  make_project_panel("E) Example local mitigation"),
  legend_panel,
  nrow = 3,
  ncol = 2
)

ggsave(
  filename = "figures/supp_figures/supp_fig_03.png",
  plot = panel,
  width = 16,
  height = 22,
  units = "cm",
  dpi = 300,
  bg = "white"
)

leakage_summary_table
