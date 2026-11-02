# Calculate mean, median, 95% CI, and mean-based carbon-pool shares for AGB,
# BGB, and SOC in pasture and forest pixels in Amazonia and the Atlantic Forest.

library(tidyverse)
library(sf)
library(terra)

#-------------------------------------------------------------------------------

walker_data_dir <- "C:/Users/franc/OneDrive - University of Cambridge/Data/Walker_et_al_2022"

forest_threshold <- 0.95
pasture_threshold <- 0.95

output_dir <- "output_tables/reviewers_request"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

resolve_existing_path <- function(paths) {
  existing <- paths[file.exists(paths)]

  if (length(existing) == 0) {
    stop("Could not find any of these files: ", paste(paths, collapse = "; "))
  }

  existing[1]
}

carbon_source_path <- function(file_name) {
  resolve_existing_path(c(
    file.path("variables", file_name),
    file.path("variables", "raw", file_name),
    file.path("variables", "hide", file_name),
    file.path(walker_data_dir, file_name),
    file_name
  ))
}

align_carbon_layer_to_agc <- function(input_path,
                                      output_path,
                                      template,
                                      brazil_boundary,
                                      method = "bilinear",
                                      overwrite = FALSE) {
  if (file.exists(output_path) && !overwrite) {
    return(rast(output_path))
  }

  message("Aligning ", input_path, " to AGC 1 km grid...")

  source_raster <- rast(input_path)
  brazil_boundary_template_crs <- st_transform(brazil_boundary, crs(template))

  source_aligned <- project(source_raster, template, method = method)
  source_brazil <- source_aligned %>%
    crop(brazil_boundary_template_crs) %>%
    mask(brazil_boundary_template_crs)

  writeRaster(source_brazil, output_path, overwrite = TRUE)
  rast(output_path)
}

summarise_values <- function(values) {
  values <- values[is.finite(values)]
  n_pixels <- length(values)

  if (n_pixels == 0) {
    return(tibble(
      n_pixels = 0L,
      mean = NA_real_,
      median = NA_real_,
      ci95_low = NA_real_,
      ci95_high = NA_real_
    ))
  }

  mean_value <- mean(values)
  se_value <- sd(values) / sqrt(n_pixels)

  tibble(
    n_pixels = n_pixels,
    mean = mean_value,
    median = median(values),
    ci95_low = mean_value - qnorm(0.975) * se_value,
    ci95_high = mean_value + qnorm(0.975) * se_value
  )
}

extract_carbon_summary <- function(carbon_layer,
                                   carbon_pool,
                                   biome_boundary,
                                   biome_name,
                                   cover_mask,
                                   cover_class) {
  target <- st_transform(biome_boundary, crs(carbon_layer))

  carbon_values <- carbon_layer %>%
    crop(target) %>%
    mask(target)

  cover_mask_values <- cover_mask %>%
    crop(carbon_values)

  if (!compareGeom(carbon_values, cover_mask_values, stopOnError = FALSE)) {
    cover_mask_values <- resample(cover_mask_values, carbon_values, method = "near")
  }

  cover_mask_values <- cover_mask_values %>%
    mask(target)

  values <- carbon_values %>%
    mask(cover_mask_values) %>%
    values(na.rm = TRUE) %>%
    as.numeric()

  summarise_values(values) %>%
    mutate(
      biome = biome_name,
      cover_class = cover_class,
      carbon_pool = carbon_pool,
      .before = 1
    )
}

#-------------------------------------------------------------------------------

agc <- rast("variables/AGC_current_Walter_22_layer_albers_1km_res.tif")

br <- readRDS("input_tables/br.rds") %>%
  st_transform(crs(agc))

biomes <- readRDS("input_tables/am_af.rds") %>%
  st_transform(crs(agc)) %>%
  filter(name_biome %in% c("Amazônia", "Mata Atlântica")) %>%
  mutate(
    biome = case_when(
      name_biome == "Amazônia" ~ "Amazon",
      name_biome == "Mata Atlântica" ~ "Atlantic Forest",
      TRUE ~ name_biome
    )
  )

if (nrow(biomes) != 2) {
  stop("Expected to find Amazonia and Atlantic Forest in input_tables/am_af.rds.")
}

soc_source <- carbon_source_path("Base_Cur_SOC_MgCha_500m.tif")
bgb_source <- carbon_source_path("Base_Cur_BGB_MgCha_500m.tif")

soc <- align_carbon_layer_to_agc(
  input_path = soc_source,
  output_path = "variables/Base_Cur_SOC_MgCha_albers_1km_res.tif",
  template = agc,
  brazil_boundary = br
)

bgb <- align_carbon_layer_to_agc(
  input_path = bgb_source,
  output_path = "variables/Base_Cur_BGB_MgCha_albers_1km_res.tif",
  template = agc,
  brazil_boundary = br
)

agc_br_path <- "variables/AGC_current_Walter_22_layer_albers_1km_res_br.tif"

if (file.exists(agc_br_path)) {
  agc_br <- rast(agc_br_path)
} else {
  agc_br <- agc %>%
    crop(br) %>%
    mask(br)

  writeRaster(agc_br, agc_br_path, overwrite = FALSE)
}

forest_cover <- rast("variables/forest_cover_albers_1km_2021.tif") %>%
  resample(agc, method = "bilinear")

pasture_cover <- rast("variables/pasture_coverage_lapig_2021_col8albers_pj_1000mres.tif") %>%
  resample(agc, method = "bilinear")

forest_mask <- ifel(forest_cover >= forest_threshold, 1, NA)
pasture_mask <- ifel(pasture_cover >= pasture_threshold, 1, NA)

carbon_layers <- list(
  AGB = agc_br,
  BGB = bgb,
  SOC = soc
)

cover_masks <- list(
  forest = forest_mask,
  pasture = pasture_mask
)

carbon_summary <- pmap_dfr(
  expand_grid(
    biome_index = seq_len(nrow(biomes)),
    cover_class = names(cover_masks),
    carbon_pool = names(carbon_layers)
  ),
  \(biome_index, cover_class, carbon_pool) {
    extract_carbon_summary(
      carbon_layer = carbon_layers[[carbon_pool]],
      carbon_pool = carbon_pool,
      biome_boundary = biomes[biome_index, ],
      biome_name = biomes$biome[biome_index],
      cover_mask = cover_masks[[cover_class]],
      cover_class = cover_class
    )
  }
) %>%
  group_by(biome, cover_class) %>%
  mutate(
    total_mean_carbon = sum(mean, na.rm = TRUE),
    mean_share_of_total = mean / total_mean_carbon
  ) %>%
  ungroup() %>%
  select(
    biome,
    cover_class,
    carbon_pool,
    n_pixels,
    mean,
    median,
    ci95_low,
    ci95_high,
    total_mean_carbon,
    mean_share_of_total
  ) %>%
  arrange(
    factor(biome, levels = c("Atlantic Forest", "Amazon")),
    factor(cover_class, levels = c("forest", "pasture")),
    factor(carbon_pool, levels = c("AGB", "BGB", "SOC"))
  )

write_csv(
  carbon_summary,
  file.path(output_dir, "carbon_share_AGB_BGB_SOC_by_biome_cover.csv")
)

saveRDS(
  carbon_summary,
  file.path(output_dir, "carbon_share_AGB_BGB_SOC_by_biome_cover.rds")
)


