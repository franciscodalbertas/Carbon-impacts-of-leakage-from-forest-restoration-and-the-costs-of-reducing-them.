# Map the local leakage selection for the same Eunapolis - Porto Seguro region
# used in project_map.R, using conversion probability as the background layer.

library(tidyverse)
library(sf)
library(terra)
library(ggpubr)

#-------------------------------------------------------------------------------

selected_code_immediate <- "290009"
selected_region_label <- "Eunapolis - Porto Seguro, Bahia"
pixel_size <- 1000

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

target_fp_column <- function(project) {
  if ("total_FP_leaked" %in% names(project)) {
    project$total_FP_leaked
  } else {
    project$FP_leaked
  }
}

build_local_leakage_selection <- function(project,
                                          selected_immediate_region,
                                          pixel_size = 1000) {
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
      project_set = "Selected leakage pixels"
    )

  probability_df <- as.data.frame(conversion_m * fc_mask, na.rm = TRUE, xy = TRUE)
  colnames(probability_df) <- c("x", "y", "prob_clearance")

  list(
    probability = probability_df,
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

immediate_regions <- readRDS("input_tables/imediate_regions_am_af.rds") %>%
  st_transform(crs = project_crs)

selected_immediate_region <- immediate_regions %>%
  filter(as.character(code_immediate) == selected_code_immediate)

if (nrow(selected_immediate_region) == 0) {
  stop("Could not find code_immediate ", selected_code_immediate,
       " in input_tables/imediate_regions_am_af.rds.")
}

brazil_outline <- readRDS("input_tables/br.rds") %>%
  st_transform(crs = 4674)

atlantic_forest_outline <- readRDS("input_tables/am_af.rds") %>%
  filter(str_detect(name_biome, "Atl")) %>%
  st_transform(crs = 4674)

selected_immediate_region_locator <- selected_immediate_region %>%
  st_transform(crs = 4674)

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

example_project_for_leakage <- abacus_local_scc3_eunapolis %>%
  slice_sample(n = 1)

local_leakage <- build_local_leakage_selection(
  project = example_project_for_leakage,
  selected_immediate_region = selected_immediate_region,
  pixel_size = pixel_size
)

sampled_project_outlines <- projects_AF %>%
  project_points_from_table("Sampled projects") %>%
  pixel_points_to_outlines(
    crs = project_crs,
    pixel_size = pixel_size,
    group_cols = c("project_id", "project_set")
  )

example_project_outline <- example_project_for_leakage %>%
  project_points_from_table("Example sampled project") %>%
  pixel_points_to_outlines(
    crs = project_crs,
    pixel_size = pixel_size,
    group_cols = c("project_id", "project_set")
  )

selected_leakage_outlines <- local_leakage$selected %>%
  pixel_points_to_outlines(
    x_col = "x",
    y_col = "y",
    crs = project_crs,
    pixel_size = pixel_size
  )

leakage_summary_label <- sprintf(
  "Target leaked FP: %s kg | selected leakage area: %s ha | pixels: %s",
  scales::comma(round(local_leakage$target_fp, 0)),
  scales::comma(round(local_leakage$selected_area_ha, 1)),
  scales::comma(nrow(local_leakage$selected))
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
    fill = "#D55E00",
    colour = "#7A2E00",
    linewidth = 0.45,
    alpha = 0.85
  ) +
  coord_sf(expand = FALSE) +
  labs(
    title = paste0("A) ", selected_region_label, " within the Atlantic Forest")
  ) +
  theme_void(base_size = 8) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 4)),
    plot.margin = margin(5, 5, 0, 5)
  )

leakage_probability_map <- ggplot() +
  geom_raster(
    data = local_leakage$probability,
    aes(x = x, y = y, fill = prob_clearance)
  ) +
  geom_sf(
    data = selected_immediate_region,
    fill = NA,
    colour = "#303030",
    linewidth = 0.45,
    inherit.aes = FALSE
  ) +
  geom_sf(
    data = sampled_project_outlines,
    colour = "grey82",
    fill = NA,
    linewidth = 0.12,
    alpha = 0.45,
    inherit.aes = FALSE
  ) +
  geom_sf(
    data = selected_leakage_outlines,
    aes(colour = project_set),
    fill = NA,
    linewidth = 0.45,
    alpha = 0.95,
    inherit.aes = FALSE
  ) +
  geom_sf(
    data = example_project_outline,
    aes(colour = project_set),
    fill = NA,
    linewidth = 0.5,
    alpha = 0.95,
    inherit.aes = FALSE
  ) +
  scale_fill_viridis_c(
    option = "magma",
    direction = -1,
    name = "Probability of conversion",
    labels = scales::label_number(accuracy = 0.01)
  ) +
  scale_colour_manual(
    values = c(
      "Example sampled project" = "#0072B2",
      "Selected leakage pixels" = "#D55E00"
    ),
    name = NULL
  ) +
  coord_sf(expand = FALSE) +
  labs(
    title = "D) Local-domain pixels selected to suffer leakage",
    subtitle = leakage_summary_label,
    x = NULL,
    y = NULL
  ) +
  guides(
    fill = guide_colourbar(
      barwidth = unit(3.2, "cm"),
      barheight = unit(0.35, "cm"),
      title.position = "top"
    ),
    colour = guide_legend(override.aes = list(linewidth = 0.8))
  ) +
  theme_minimal(base_size = 8) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid.major = element_line(colour = "grey90", linewidth = 0.2),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 7),
    plot.margin = margin(2, 5, 5, 5)
  )

dir.create("figures/response_reviewers", recursive = TRUE, showWarnings = FALSE)

panel <- ggarrange(
  locator_map_eunapolis,
  leakage_probability_map,
  nrow = 2,
  heights = c(1, 2)
)

ggsave(
  filename = "figures/response_reviewers/project_leakage_probability_map_eunapolis.png",
  plot = panel,
  width = 16,
  height = 14,
  units = "cm",
  dpi = 300,
  bg = "white"
)

local_leakage_label <- example_project_for_leakage %>%
  transmute(
    project_id = coordinates,
    total_FP_leaked = total_FP_leaked,
    selected_pixels = nrow(local_leakage$selected),
    selected_leakage_area_ha = local_leakage$selected_area_ha
  )

local_leakage_label
