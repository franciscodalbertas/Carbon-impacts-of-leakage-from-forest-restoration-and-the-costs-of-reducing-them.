# load packages ----------------------------------------------------------------

library(tidyr)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(scales)
library(shadowtext)

#-------------------------------------------------------------------------------

# read results
df <- readRDS("output_tables/abacus_verra_random_projects_local_biome_country_scale_prj_yield_SCC_3_USD.rds")

required_cols <- c(
  "name_biome",
  "approach",
  "leakage_scale",
  "mitigation_strategy",
  "param",
  "CO2_cost_USD"
)

missing_cols <- setdiff(required_cols, names(df))

if (length(missing_cols) > 0) {
  stop("Missing columns in combined random-project data: ", paste(missing_cols, collapse = ", "))
}

df <- df %>%
  mutate(
    param = tolower(as.character(param)),
    source = case_when(
      param == "verra" ~ "Verra",
      param == "abacus" ~ "ABACUS",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(source))

# defining order
df$approach <- factor(df$approach, levels = c("ignoring", "no_mitigation"))
df$source <- factor(df$source, levels = c("Verra", "ABACUS"))

# filter strategy and scale
country <- df %>%
  filter(
    leakage_scale %in% c("ignoring", "country"),
    mitigation_strategy != "minimize_costs",
    approach %in% c("ignoring", "no_mitigation")
  )

# make it more like figure 4
y_label <- expression("Cost per tonne of CO"[2]*"e (USD)")

plot_data <- country %>%
  mutate(
    CO2_cost = if_else(CO2_cost_USD <= 0, NA_real_, CO2_cost_USD),
    name_biome = case_when(
      name_biome %in% c("Amazon", "Amazonia", "Amaz\u00f4nia", "Amaz\u00f3nia") ~ "Amazon",
      name_biome %in% c("Atlantic Forest", "Mata Atlantica", "Mata Atl\u00e2ntica") ~ "Atlantic Forest",
      TRUE ~ as.character(name_biome)
    ),
    name_biome = factor(name_biome, levels = c("Atlantic Forest", "Amazon"))
  ) %>%
  filter(!is.na(name_biome))

mean_labels_country <- plot_data %>%
  filter(!is.na(CO2_cost)) %>%
  group_by(name_biome, approach, source) %>%
  summarize(mean_cost = mean(CO2_cost, na.rm = TRUE), .groups = "drop") %>%
  mutate(label_colour = if_else(source == "Verra", "black", "lightgray"))

panel_labels <- tibble(
  approach = factor(c("ignoring", "no_mitigation"), levels = c("ignoring", "no_mitigation")),
  name_biome = factor("Atlantic Forest", levels = c("Atlantic Forest", "Amazon")),
  label_y = Inf,
  panel_label = c("A", "B")
)

dodge_width <- 0.78
violin_width <- 0.75

plot_verra_abacus_country <- plot_data %>%
  ggplot(aes(x = name_biome, y = CO2_cost)) +
  geom_violin(
    aes(
      fill = source,
      colour = source,
      group = interaction(name_biome, source)
    ),
    position = position_dodge(width = dodge_width),
    width = violin_width,
    linewidth = 0.4,
    na.rm = TRUE
  ) +
  shadowtext::geom_shadowtext(
    data = mean_labels_country,
    aes(
      x = name_biome,
      y = mean_cost,
      label = round(mean_cost, 0),
      group = source,
      colour = label_colour
    ),
    position = position_dodge(width = dodge_width),
    inherit.aes = FALSE,
    vjust = 0,
    size = 2.5,
    bg.colour = "white",
    show.legend = FALSE
  ) +
  geom_text(
    data = panel_labels,
    aes(x = name_biome, y = label_y, label = panel_label),
    inherit.aes = FALSE,
    hjust = 5,
    vjust = 1.5,
    fontface = "bold",
    size = 3
  ) +
  scale_fill_manual(
    values = c("Verra" = "lightgray", "ABACUS" = "white"),
    labels = c("Verra" = "30%", "ABACUS" = "100%"),
    name = ""
  ) +
  scale_colour_manual(
    values = c(
      "Verra" = "gray35",
      "ABACUS" = "gray70",
      "black" = "black",
      "lightgray" = "lightgray"
    ),
    breaks = c("Verra", "ABACUS"),
    labels = c("Verra" = "30%", "ABACUS" = "100%"),
    name = ""
  ) +
  scale_y_continuous(trans = "log10", labels = comma) +
  facet_grid(
    ~approach,
    labeller = labeller(approach = c(
      "ignoring" = "Ignoring leakage",
      "no_mitigation" = "Accounting for leakage (no mitigation)"
    )),
    scales = "free"
  ) +
  xlab("") +
  ylab(y_label) +
  theme_classic() +
  theme(
    legend.position = "top",
    text = element_text(size = 7),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_rect(colour = NA)
  )

plot_verra_abacus_country

# main text
ggsave(
  filename = "figures/Fig2_carbon_costs_verra_abacus_random.png",
  plot = plot_verra_abacus_country,
  width = 11,
  height = 7,
  units = "cm"
)
