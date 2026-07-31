# Map a subregion and then show all projects, the sampled projects and 
# the areas chosen for intensification!

# i would also need to choose here which leakage rate I should choose.
# do it with 100%
#---- load packages ------------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)
library(grid)
source("functions/15_mitigation_precomputed_candidates.R")
#-------------------------------------------------------------------------------

# open projects in the AF and select one immediate region

projects <- readRDS("project_location/project_location_round1_sampled1000projects.rds")

projects_AF <- as.data.frame(projects) %>%
  filter(name_biome == "Mata Atlântica",
         name_state=="Bahia",
         name_immediate=="Eunápolis - Porto Seguro")

length(unique(projects_AF$coordinates)) # projects

# now get all of the potential projets in this area!!

all_projects <- readRDS("input_tables/all_potential_projects_AF.rds")
all_projects_eunapolis <- all_projects%>%
  filter(code_immediate %in% projects_AF$code_immediate)

# first, do 2 a panel with 2 figures, showing all projects withing a location
# and the sampled projects!

# The sampled projects and the full project table store the pixel coordinates
# differently, so first put both into one point-per-pixel structure.
project_crs <- terra::crs(
  terra::rast("variables/beef_yield_kg_ha_2021_albers_1km.tif")
)

local_panel_levels <- c(
  "B) All potential projects",
  "C) Sampled projects",
  "D) Example local mitigation"
)

immediate_regions <- readRDS("input_tables/imediate_regions_am_af.rds") %>%
  st_transform(crs = project_crs)

selected_immediate_region <- immediate_regions %>%
  filter(as.character(code_immediate) %in% as.character(unique(projects_AF$code_immediate)))

if (nrow(selected_immediate_region) == 0) {
  stop("Could not find the selected immediate region in input_tables/imediate_regions_am_af.rds.")
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

project_points_to_outlines <- function(project_points, crs = project_crs, pixel_size = 1000) {
  half_pixel <- pixel_size / 2

  pixel_geometry <- map2(
    project_points$x,
    project_points$y,
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

  st_sf(
    project_points %>% select(project_id, project_set),
    geometry = st_sfc(pixel_geometry, crs = crs)
  ) %>%
    group_by(project_id, project_set) %>%
    summarise(.groups = "drop")
}

mitigation_pixels_to_sf <- function(mitigation_pixels, crs = project_crs, pixel_size = 1000) {
  mitigation_pixels %>%
    transmute(
      project_id = paste(x_mitig, y_mitig, sep = "_"),
      project_set = "Selected mitigation pixels",
      x = as.numeric(x_mitig),
      y = as.numeric(y_mitig)
    ) %>%
    project_points_to_outlines(crs = crs, pixel_size = pixel_size)
}

all_project_outlines <- all_projects_eunapolis %>%
  project_points_from_table("All potential projects") %>%
  project_points_to_outlines()

sampled_project_outlines <- projects_AF %>%
  project_points_from_table("Sampled projects") %>%
  project_points_to_outlines()

all_panel <- all_project_outlines %>%
  mutate(panel = factor("B) All potential projects", levels = local_panel_levels))

sampled_context_panel <- all_project_outlines %>%
  mutate(panel = factor("C) Sampled projects", levels = local_panel_levels))

sampled_panel <- sampled_project_outlines %>%
  mutate(panel = factor("C) Sampled projects", levels = local_panel_levels))

# Third panel: randomly choose one ABACUS high/low leakage project in this
# immediate region and show the local mitigation pixels selected to offset its
# forgone production.
set.seed(123)

abacus_local_scc3 <- readRDS(
  "mitigation_inputs/inputs_for_mitigation_leakage_abacus_local_scale_yield_prj_high_low_leakage_SCC_3_round1.rds"
)

abacus_local_scc3_eunapolis <- abacus_local_scc3 %>%
  filter(
    as.character(code_immediate) %in% as.character(unique(projects_AF$code_immediate))
  ) %>%
  distinct(coordinates, .keep_all = TRUE)

if (nrow(abacus_local_scc3_eunapolis) == 0) {
  stop(
    "No Eunapolis projects were found in the ABACUS local SCC3 high/low input. ",
    "Check that projects_AF and the high_low_leakage mitigation input include ",
    "the same immediate region."
  )
}

example_project_for_mitigation <- abacus_local_scc3_eunapolis %>%
  slice_sample(n = 1)

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
  project = example_project_for_mitigation,
  mitigation_scale = "local"
)

example_project_outline <- example_project_for_mitigation %>%
  project_points_from_table("Sampled projects") %>%
  project_points_to_outlines() %>%
  mutate(panel = factor("D) Example local mitigation", levels = local_panel_levels))

example_mitigation_outlines <- example_mitigation_pixels %>%
  mitigation_pixels_to_sf() %>%
  mutate(panel = factor("D) Example local mitigation", levels = local_panel_levels))

example_mitigation_label <- example_project_for_mitigation %>%
  transmute(
    project_id = coordinates,
    total_FP_leaked = total_FP_leaked,
    selected_pixels = nrow(example_mitigation_pixels),
    mitigation_area_ha = sum(example_mitigation_pixels$area_required, na.rm = TRUE)
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
    title = "A) Eunápolis - Porto Seguro, Bahia, within the Atlantic Forest"
  ) +
  theme_void(base_size = 8) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 4)),
    plot.margin = margin(5, 5, 0, 5)
  )

project_map_eunapolis <- ggplot() +
  geom_sf(
    data = selected_immediate_region_panels,
    fill = NA,
    colour = "#303030",
    linewidth = 0.45,
    inherit.aes = FALSE
  ) +
  geom_sf(
    data = all_panel,
    aes(colour = project_set),
    fill = NA,
    linewidth = 0.15,
    alpha = 0.55
  ) +
  geom_sf(
    data = sampled_context_panel,
    colour = "grey82",
    fill = NA,
    linewidth = 0.12,
    alpha = 0.45
  ) +
  geom_sf(
    data = sampled_panel,
    aes(colour = project_set),
    fill = NA,
    linewidth = 0.35,
    alpha = 0.95
  ) +
  geom_sf(
    data = example_project_outline,
    aes(colour = project_set),
    fill = NA,
    linewidth = 0.45,
    alpha = 0.95
  ) +
  geom_sf(
    data = example_mitigation_outlines,
    aes(colour = project_set),
    fill = NA,
    linewidth = 0.25,
    alpha = 0.75
  ) +
  facet_wrap(~panel, nrow = 1) +
  scale_colour_manual(
    values = c(
      "All potential projects" = "#4E79A7",
      "Sampled projects" = "#D55E00",
      "Selected mitigation pixels" = "#009E73"
    )
  ) +
  coord_sf(expand = FALSE) +
  labs(
    colour = NULL,
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 8) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid.major = element_line(colour = "grey90", linewidth = 0.2),
    strip.text = element_text(face = "bold"),
    plot.margin = margin(2, 5, 5, 5)
  )

dir.create("figures/response_reviewers", recursive = TRUE, showWarnings = FALSE)

library(ggpubr)

panel <- ggarrange(locator_map_eunapolis,project_map_eunapolis,nrow=2,heights = c(1,2)) 

ggsave(filename = "figures/response_reviewers/project_map_eunapolis.png",
       plot = panel,
       width = 16,
       height = 14,
       units = "cm",
       dpi = 300,
       bg = "white")
# png(
#   filename = "figures/response_reviewers/project_map_eunapolis.png",
#   width = 15,
#   height = 15,
#   units = "cm",
#   res = 300
# )
# 
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(
#   nrow = 2,
#   ncol = 1,
#   heights = unit(c(1.55, 5.25), "in")
# )))
# 
# print(locator_map_eunapolis, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
# print(project_map_eunapolis, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
# dev.off()
# 
# locator_map_eunapolis
# project_map_eunapolis
# example_mitigation_label

