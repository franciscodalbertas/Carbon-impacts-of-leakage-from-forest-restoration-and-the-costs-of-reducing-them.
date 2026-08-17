# this figure compares mitigation and no mitigation across SCC discount rates,
# showing ABACUS (100% leakage) and Verra (30% leakage) side by side.

# ---- load packages -----------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggpp)

# ---- read data ---------------------------------------------------------------

df <- readRDS("output_tables/testing_scc_leakage_sensitivity.rds")

df <- df %>%
  mutate(
    yield_carbon_ratio = case_when(
      yield_carbon_ratio == "max" ~ "high",
      yield_carbon_ratio == "min" ~ "low",
      yield_carbon_ratio == "random" ~ yield_carbon_ratio
    ),
    yield_carbon_ratio = factor(yield_carbon_ratio, levels = c("random", "high", "low")),
    leakage = factor(leakage, levels = c("30%", "100%")),
    mitigation_strategy = factor(
      mitigation_strategy,
      levels = c("no_mitigation", "optimize_carbon")
    ),
    leakage_ratio = interaction(yield_carbon_ratio, leakage, sep = "-"),
    leakage_ratio = factor(
      leakage_ratio,
      levels = c("high-30%", "high-100%", "low-30%", "low-100%", "random-30%", "random-100%")
    )
  )

# ---- plotting helpers --------------------------------------------------------

y_label <- expression("Cost tCO"[2]*"e (USD)")
tight <- theme(plot.margin = margin(2, 4, 2, 4))
box_width <- 0.28

fill_values <- c(
  "high-30%" = "lightgreen",
  "high-100%" = "lightgreen",
  "low-30%" = "darkgreen",
  "low-100%" = "darkgreen",
  "random-30%" = "white",
  "random-100%" = "white"
)

colour_values <- c(
  "high-30%" = "black",
  "high-100%" = "gray60",
  "low-30%" = "black",
  "low-100%" = "gray60",
  "random-30%" = "black",
  "random-100%" = "gray60"
)

legend_breaks <- c("high-30%", "high-100%", "low-30%", "low-100%")

add_x_position <- function(data) {
  data %>%
    mutate(
      x_base = case_when(
        mitigation_strategy == "no_mitigation" & leakage == "30%" ~ 1,
        mitigation_strategy == "no_mitigation" & leakage == "100%" ~ 2,
        mitigation_strategy == "optimize_carbon" & leakage == "30%" ~ 3,
        mitigation_strategy == "optimize_carbon" & leakage == "100%" ~ 4
      ),
      x_offset = case_when(
        yield_carbon_ratio == "high" ~ -0.15,
        yield_carbon_ratio == "low" ~ 0.15
      ),
      x_plot = x_base + x_offset
    )
}

boxplot_axis_limits <- function(plot_data, reference_data = NULL) {
  whiskers <- plot_data %>%
    group_by(mitigation_strategy, leakage, yield_carbon_ratio) %>%
    summarise(
      ymin = boxplot.stats(CO2_cost_USD)$stats[1],
      ymax = boxplot.stats(CO2_cost_USD)$stats[5],
      .groups = "drop"
    )
  
  y_values <- c(whiskers$ymin, whiskers$ymax, reference_data$median)
  y_values <- y_values[is.finite(y_values) & y_values > 0]
  
  if (length(y_values) == 0) {
    return(NULL)
  }
  
  c(min(y_values) * 0.75, max(y_values) * 1.6)
}

plot_discount_panel <- function(biome, disc_value, panel_label, limit_x = NULL,
                                show_x_axis = FALSE) {
  df_disc <- df %>%
    filter(disc == disc_value)
  
  medians <- df_disc %>%
    filter(CO2_cost_USD > 0) %>%
    group_by(yield_carbon_ratio, mitigation_strategy, name_biome, leakage, leakage_ratio) %>%
    summarise(
      median = median(CO2_cost_USD, na.rm = TRUE),
      low = quantile(CO2_cost_USD, 0.025, na.rm = TRUE),
      high = quantile(CO2_cost_USD, 0.975, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    add_x_position()
  
  missing_positive <- df_disc %>%
    filter(
      name_biome == biome,
      yield_carbon_ratio %in% c("high", "low")
    ) %>%
    group_by(yield_carbon_ratio, mitigation_strategy, name_biome, leakage, leakage_ratio) %>%
    summarise(
      n_positive = sum(CO2_cost_USD > 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(n_positive == 0) %>%
    add_x_position() %>%
    mutate(
      y = 40,
      label = "no net carbon removal\nafter leakage"
    )
  
  med_random <- medians %>%
    filter(
      yield_carbon_ratio == "random",
      mitigation_strategy == "no_mitigation",
      name_biome == biome
    )
  
  med_random_30 <- med_random %>%
    filter(leakage == "30%")
  
  med_random_value_label <- if (nrow(med_random_30) > 0) {
    med_random_30 %>%
      mutate(label_part = round(median, 0)) %>%
      pull(label_part)
  } else {
    NA_character_
  }
  
  df_2plot <- df_disc %>%
    filter(
      name_biome == biome,
      yield_carbon_ratio != "random",
      CO2_cost_USD > 0
    ) %>%
    add_x_position() %>%
    droplevels()
  
  if (is.null(limit_x)) {
    limit_x <- boxplot_axis_limits(df_2plot, med_random_30)
  }
  
  p <- df_2plot %>%
    ggplot(aes(
      x = x_plot,
      y = CO2_cost_USD,
      fill = leakage_ratio,
      colour = leakage_ratio
    )) +
    geom_boxplot(
      aes(group = interaction(mitigation_strategy, leakage, yield_carbon_ratio)),
      width = box_width,
      outlier.shape = NA,
      na.rm = TRUE
    ) +
    geom_hline(
      data = med_random_30,
      aes(
        yintercept = median,
        colour = leakage_ratio,
        linetype = "avg. cost\nno mitigation"
      ),
      inherit.aes = FALSE,
      show.legend = TRUE
    ) +
    shadowtext::geom_shadowtext(
      data = med_random_30,
      aes(
        x = 0.58,
        y = median,
        label = med_random_value_label
      ),
      vjust = -0.35,
      size = 2.5,
      bg.colour = "white",
      colour = "black",
      g.r = 0.2,
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +
    shadowtext::geom_shadowtext(
      data = medians %>%
        filter(
          name_biome == biome,
          yield_carbon_ratio != "random"
        ),
      aes(
        x = x_plot,
        y = median,
        label = round(median, 0),
        colour = leakage_ratio,
        group = interaction(mitigation_strategy, leakage, yield_carbon_ratio)
      ),
      size = 2.5,
      bg.colour = "white",
      g.r = 0.2,
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +
    shadowtext::geom_shadowtext(
      data = missing_positive,
      aes(
        x = x_plot,
        y = y,
        label = label
      ),
      angle = 90,
      hjust = -0.05,
      vjust = 0,
      size = 2,
      bg.colour = "lightgreen",
      colour = "darkgray",
      g.r = 0.001,
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +
    scale_y_log10(
      labels = scales::label_number(),
      oob = scales::oob_keep
    ) +
    scale_x_continuous(
      breaks = c(1, 2, 3, 4),
      labels = c(
        "no mitigation (30%)",
        "no mitigation (100%)",
        "mitigation (30%)",
        "mitigation (100%)"
      ),
      limits = c(0.5, 4.5),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_fill_manual(
      name = "Forgone-production-to-carbon-gained",
      values = fill_values,
      breaks = legend_breaks,
      drop = FALSE
    ) +
    scale_colour_manual(
      name = "Forgone-production-to-carbon-gained",
      values = colour_values,
      breaks = legend_breaks,
      drop = FALSE
    ) +
    scale_linetype_manual(
      name = "",
      values = c("avg. cost\nno mitigation" = "dotted")
    ) +
    guides(
      fill = guide_legend(
        override.aes = list(colour = unname(colour_values[legend_breaks])),
        order = 1
      ),
      colour = "none",
      linetype = guide_legend(
        override.aes = list(colour = "black"),
        order = 2
      )
    ) +
    ylab(y_label) +
    xlab("") +
    theme_classic() +
    theme(
      legend.position = "top",
      text = element_text(size = 7),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      axis.text.x = if (show_x_axis) element_text(size = 6) else element_blank(),
      axis.ticks.x = if (show_x_axis) element_line() else element_blank()
    ) +
    coord_cartesian(ylim = limit_x, clip = "on") +
    annotate(
      "text",
      x = -Inf,
      y = Inf,
      label = panel_label,
      hjust = -0.05,
      vjust = 1.2,
      size = 2.5
    ) +
    tight
  
  p
}

plot_function <- function(biome, df = df) {
  p1_limits <- NULL
  p2_limits <- NULL
  p3_limits <- NULL
  
  p1 <- plot_discount_panel(
    biome = biome,
    disc_value = "3%",
    panel_label = "r = 3%",
    limit_x = p1_limits
  )
  
  p2 <- plot_discount_panel(
    biome = biome,
    disc_value = "2%",
    panel_label = "r = 2%",
    limit_x = p2_limits
  )
  
  p3 <- plot_discount_panel(
    biome = biome,
    disc_value = "4%",
    panel_label = "r = 4%",
    limit_x = p3_limits,
    show_x_axis = TRUE
  )
  
  panel <- ggarrange(
    p1, p2, p3,
    common.legend = TRUE,
    nrow = 3,
    labels = "AUTO",
    align = "v"
  )
  
  if (biome != "Amazônia") {
    ggsave(
      plot = panel,
      "figures/extended_figures/extended_figure_8_AF.png",
      width = 15.5,
      height = 17,
      units = "cm",
      bg = "white"
    )
  } else {
    ggsave(
      plot = panel,
      "figures/extended_figures/extended_figure_9_AM.png",
      width = 15.5,
      height = 17,
      units = "cm",
      bg = "white"
    )
  }
  
  return(panel)
}

# ---- run plots ---------------------------------------------------------------

af_plot <- plot_function(biome = "Mata Atlântica", df = df)


am_plot <- plot_function(biome = "Amazônia", df = df)
