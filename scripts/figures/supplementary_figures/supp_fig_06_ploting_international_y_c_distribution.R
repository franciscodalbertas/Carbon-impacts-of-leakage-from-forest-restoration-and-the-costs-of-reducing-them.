#---- load data -------

library(tidyverse)
library(ggridges)
library(scales)
#----------------------

df <- readRDS("input_tables/global_y_c_ratio.rds")

# add european union production!

eu27_iso3 <- c(
  "AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA",
  "DEU","GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD",
  "POL","PRT","ROU","SVK","SVN","ESP","SWE"
)

df <- df %>%
  # add a new code where EU is included
  mutate(ID = if_else( iso_a3 %in% eu27_iso3,"EU",iso_a3),
         ID_nme = if_else( iso_a3 %in% eu27_iso3,"European Union",name))



# filter top n exports!

exporters <- read.csv("input_tables/beef_exports_2021_USDA-Francisco.csv")%>%
  mutate(production = as.numeric(production),
         country = if_else(country =="United States","United States of America",country))


top10 <- exporters %>%
  ungroup() %>%
  arrange(desc(production), country) %>%
  slice_head(n = 10)

# total_production <- df %>%
#   select(ID,ID_nme,tonnes_beef)%>%
#   distinct()%>%
#   group_by(ID,ID_nme) %>%
#   summarise(tonnes_beef = sum(tonnes_beef))
# 
# 
# top10 <- total_production %>%
#   ungroup() %>%                 
#   arrange(desc(tonnes_beef), ID) %>%
#   slice_head(n = 10)
# 

df_filt <- df %>%
  filter(ID_nme %in% top10$country)

# ---- clean data for ploting ----

df_filt <- df_filt %>%
  filter(!is.infinite(y_c))


summary(df_filt)

# check values of carbon and yield

summary(df_filt$carbon) # also weird..really small values...should be zero

summary(df_filt$yield) # extreme high and low values!

hist(df_filt$yield)
max(df_filt$yield) # yield of 4390 is insane
hist(df_filt$carbon)

quantile(df_filt$yield,0.99 )

#the distributions of yield are weird, but mostly on the bottom. And one outlier
# in the top...define a treshold for the top ~ 500

# there is not enough points in India with treshold =0.1?

# define tresholds for yield - this can be due to error in the data. will trim 2%
# of the data..should be enough


pcts_yield <- df_filt %>%
  group_by(ID) %>%
  summarise(
    bottom  = quantile(.data[["yield"]], 0.01, na.rm = TRUE, names = FALSE),
    upper = quantile(.data[["yield"]], 0.99, na.rm = TRUE, names = FALSE),
    max  = max(.data[["yield"]], 0.01, na.rm = TRUE, names = FALSE),
    #upper = quantile(.data[["yield"]], 0.99, na.rm = TRUE, names = FALSE)
    n     = sum(!is.na(.data[["yield"]])),
    .groups = "drop"
  )

# pcts_carbon <- df_filt %>%
#   group_by(ID) %>%
#   summarise(
#     bottom  = quantile(.data[["carbon"]], 0.025, na.rm = TRUE, names = FALSE),
#     upper = quantile(.data[["carbon"]], 0.975, na.rm = TRUE, names = FALSE),
#     n     = sum(!is.na(.data[["carbon"]])),
#     .groups = "drop"
#   )



  
# filtering yield

df_filt_yield <- df_filt %>%
  left_join(pcts_yield, by = "ID") %>%
  filter(between(.data[["yield"]], bottom, upper)) %>%
  select(-bottom, -upper)%>%
  group_by(ID,ID_nme)


# df_filt_carbon <- df_filt_yield %>%
#   left_join(pcts_carbon, by = "ID") %>%
#   filter(between(.data[["carbon"]], bottom, upper)) %>%
#   select(-bottom, -upper)%>%
#   group_by(ID,ID_nme)

summary(df_filt_yield)

# filtering y_c: now this is only for plotting!
# so should do from the final distribution

# pcts_y_c <- df_filt_carbon %>%
#   group_by(ID) %>%
#   summarise(
#     bottom  = quantile(.data[["y_c"]], 0.05, na.rm = TRUE, names = FALSE),
#     upper = quantile(.data[["y_c"]], 0.95, na.rm = TRUE, names = FALSE),
#     n     = sum(!is.na(.data[["y_c"]])),
#     .groups = "drop"
#   )
# 
# 
# df_filt_y_c <- df_filt_carbon %>%
#   left_join(pcts_y_c, by = "ID") %>%
#   filter(between(.data[["y_c"]], bottom, upper)) %>%
#   select(-bottom, -upper)%>%
#   group_by(ID,ID_nme)

#---- ploting ----

# otdering plots

order <- c(top10$country)

df_filt_yield$ID_nme <- factor(df_filt_yield$ID_nme,levels = rev(order))

order_id <- c("BRA","USA","IND","AUS","NZL","EU","ARG","CAN","URY","PRY")


heterog_C <- ggplot(df_filt_yield, aes(x = carbon, y =ID_nme, fill = ID_nme)) +
  geom_density_ridges(scale = 1.5, alpha = 0.8, rel_min_height = 0.01, color = "white") +
  scale_fill_viridis_d(guide = "none")+
  # scale_x_log10(
  #   #breaks = c(0.1, 0.2, 0.5, 1, 2, 5, 10),
  #   labels = label_number()
  # ) +
  theme_classic() +
  theme(
    #legend.position = c(0.98, 0.98),
    #legend.justification = c("right", "top"),
    legend.background = element_rect(fill = "white", color = NA),
    #legend.box.background = element_rect(color = "black"),
    text = element_text(size = 8)
  )+
  guides(linetype = guide_legend(title = NULL))

# not capping it over time is really inflating values elsewhere!
# in particular in high latitudes!
heterog_C

heterog_y <- ggplot(df_filt_yield, aes(x = yield, y =ID_nme, fill = ID_nme)) +
  geom_density_ridges(scale = 1.5, alpha = 0.8, rel_min_height = 0.01, color = "white") +
  scale_fill_viridis_d(guide = "none")+
  # scale_x_log10(
  #   #breaks = c(0.1, 0.2, 0.5, 1, 2, 5, 10),
  #   labels = label_number()
  # ) +
  theme_classic() +
  theme(
    #legend.position = c(0.98, 0.98),
    #legend.justification = c("right", "top"),
    legend.background = element_rect(fill = "white", color = NA),
    #legend.box.background = element_rect(color = "black"),
    text = element_text(size = 8)
  )+
  guides(linetype = guide_legend(title = NULL))


heterog_y

df_filt_yield$ID <- factor(df_filt_yield$ID,levels = rev(order_id))

# 1) Summaries per country (use quantile 2.5–97.5% as a 95% interval)
sum_df <- df_filt_yield %>%
  filter(is.finite(y_c), y_c > 0) %>%                 # log-scale safe
  group_by(ID, ID_nme) %>%
  summarise(
    median  = median(y_c, na.rm = TRUE),
    lower = quantile(y_c, 0.025, na.rm = TRUE, names = FALSE),
    upper = quantile(y_c, 0.975, na.rm = TRUE, names = FALSE),
    n     = sum(!is.na(y_c)),
    .groups = "drop"
  )


# 1) 20% quantile for Brazil (keep >0 for log scale)
br_q20 <- df_filt_yield %>%
  filter(ID == "BRA", is.finite(y_c), y_c > 0) %>%
  summarise(
    ID  = first(ID),
    q20 = pmax(quantile(y_c, 0.20, na.rm = TRUE, names = FALSE),
               .Machine$double.eps)  # avoid 0 on log scale
  )

# filtering for plot (does not work!)
df_plot <- df_filt_yield %>% 
  dplyr::filter(y_c >= 0, y_c <= 1000)

heterog_plot <- ggplot(df_filt_yield, aes(x = (y_c), y =ID, fill = ID_nme)) +
  geom_density_ridges(scale = 1.5, alpha = 0.8, rel_min_height = 0.01, color = "white") +
  # 95% interval as a horizontal segment
  geom_segment(data = sum_df,
               aes(x = lower, xend = upper, y = ID, yend = ID),
               inherit.aes = FALSE, linewidth = 0.6, colour = "black") +
  # mean as a point
  geom_point(data = sum_df,
             aes(x = median, y = ID),
             inherit.aes = FALSE, size = 2.5, shape = 21, colour = "black", fill = "white") +scale_fill_viridis_d(guide = "none")+
  scale_x_log10(
    breaks = c(0.1, 1, 10, 1000),
    labels = label_number()
  ) +
  coord_cartesian(xlim=c(0,100))+
  theme_classic() +
  theme(text = element_text(size = 8))+
  guides(linetype = guide_legend(title = NULL))+
  ylab("")+
  xlab("forgone-production-to-carbon-gained ratio distribution")

heterog_plot

# 2) Your plot (add this AFTER building heterog_plot)
final_plot <- heterog_plot +
  geom_vline(xintercept = br_q20$q20,
             linetype = "dashed", linewidth = 0.5, colour = "firebrick") +
  geom_point(data = br_q20,
             aes(x = q20, y = ID),
             inherit.aes = FALSE,
             shape = 24, size = 2.5, stroke = 0.4,
             fill = "white", colour = "firebrick") +
  geom_text(data = br_q20,
            aes(x = q20, y = ID, label = "20%"),
            inherit.aes = FALSE,
            vjust = -2, 
            hjust = -0.1,
            size = 2.2, colour = "firebrick")+
  coord_cartesian(clip = "off")


# headroom based on your data (keeps log scale)
xr <- range(df_filt_yield$y_c[df_filt_yield$y_c > 0], na.rm = TRUE)

final_plot <- final_plot +
  scale_x_log10(
    limits = c(xr[1], xr[2] * 1.25),                 # extend max 25%
    expand = expansion(mult = c(0.02, 0.08)),        # extra padding
    labels = label_number(accuracy = 0.01, big.mark = " ")
  ) +
  theme(plot.margin = margin(5.5, 18, 5.5, 5.5))     # more space on right

final_plot

ggsave(final_plot,
       filename =  "figures/supp_figures/supp_fig_06.png",
       height = 10,
       width = 12,
       units = "cm")
