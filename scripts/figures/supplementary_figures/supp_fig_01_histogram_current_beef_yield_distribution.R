# load packages ----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggpubr)
library(scales)

#-------------------------------------------------------------------------------


# yield data
yield <- readRDS("input_tables/beef_yield_municipality_kg_ha_2021.rds")
yield <- filter(yield,yield>0)
# Calculate 5th and 95th percentiles
bottom_5 <- quantile(yield$density, 0.05, na.rm = TRUE)
top_5 <- quantile(yield$density, 0.95, na.rm = TRUE)

#min_boundary_upper <- max(yield$density[yield$outlier==0])

# loged version

distribution_density <- yield %>%
  ggplot(aes(x = density))+
  geom_histogram(fill = "blue",alpha = 0.2)+
  # geom_vline(aes(xintercept = mean(density, na.rm = TRUE)), 
  #            color = "black", linetype = "dashed", size = 1, show.legend = TRUE) +  # Mean line
  # geom_vline(aes(xintercept = median(density, na.rm = TRUE)), 
  #            color = "gray", linetype = "dotted", size = 1, show.legend = TRUE) +  # Median line
  # # bottom treshold
  geom_vline(aes(xintercept = bottom_5), 
             color = "black", size = 1, show.legend = TRUE)+
  # upper treshold
  geom_vline(aes(xintercept = top_5), 
             color = "black", size = 1, show.legend = TRUE)+
  # Add text annotations for bottom_5 and top_5
  geom_text(aes(x = bottom_5-0.08, y = 0, label = paste(round(bottom_5, 2))), 
            color = "black", vjust = -0.5, size = 2) +
  geom_text(aes(x = top_5+3.2, y = 0, label = paste(round(top_5, 2))), 
            color = "black", vjust = -0.5, size = 2) +
  scale_x_continuous(trans = "log10", labels = comma) +
  theme_classic() +
  labs(
    title = "",
    x = expression("stocking rate (animal units " *ha^-1 *"log10 scale)"),
    y = ""
  )+
  theme(
    axis.title.y = element_text(size = 7, face = "bold"), # Y-axis title
    axis.text.y = element_text(size = 7, face = "bold"),  # Y-axis tick labels
    axis.title.x = element_text(size = 7, face = "bold"), # X-axis title
    axis.text.x = element_text(size = 7, face = "bold"),   # X-axis tick labels
    title = element_text(size= 7)
  ) 

distribution_density

# loged version

yield_plot <- yield %>%
  filter(density >= 0, density <= 15)

distribution_density_unloged <- yield_plot %>%
  ggplot(aes(x = density))+
  geom_histogram(fill = "blue",alpha = 0.2)+
  # geom_vline(aes(xintercept = mean(density, na.rm = TRUE)), 
  #            color = "black", linetype = "dashed", size = 1, show.legend = TRUE) +  # Mean line
  # geom_vline(aes(xintercept = median(density, na.rm = TRUE)), 
  #            color = "gray", linetype = "dotted", size = 1, show.legend = TRUE) +  # Median line
  # # bottom treshold
  geom_vline(aes(xintercept = bottom_5), 
             color = "black", size = 1, show.legend = TRUE)+
  # upper treshold
  geom_vline(aes(xintercept = top_5), 
             color = "black", size = 1, show.legend = TRUE)+
  # Add text annotations for bottom_5 and top_5
  geom_text(aes(x = bottom_5+0.5, y = 0, label = paste(round(bottom_5, 2))), 
            color = "black", vjust = -0.5, size = 2) +
  geom_text(aes(x = top_5+0.5, y = 0, label = paste(round(top_5, 2))), 
            color = "black", vjust = -0.5, size = 2) +
  scale_x_continuous( labels = comma) +
  #coord_cartesian(xlim = c(0,10)) +
  theme_classic() +
  labs(
    title = "",
    x = expression("stocking rate (AU " *ha^-1*")"),
    y = ""
  )+
  theme(
    axis.title.y = element_text(size = 7, face = "bold"), # Y-axis title
    axis.text.y = element_text(size = 7, face = "bold"),  # Y-axis tick labels
    axis.title.x = element_text(size = 7, face = "bold"), # X-axis title
    axis.text.x = element_text(size = 7, face = "bold"),   # X-axis tick labels
    title = element_text(size= 7)
  ) 

distribution_density_unloged



yield_ditribution <- yield %>%
  filter(outlier==0,
         yield>0) %>%
  ggplot(aes(x = yield))+
  geom_histogram(fill = "blue",alpha = 0.2)+
  theme_classic() +
  labs(
    title = "",
    x = expression("yield (kg " *ha^-1 * yr^-1*" )"),
    y = ""
  )+
  theme(
    axis.title.y = element_text(size = 7, face = "bold"), # Y-axis title
    axis.text.y = element_text(size = 7, face = "bold"),  # Y-axis tick labels
    axis.title.x = element_text(size = 7, face = "bold"), # X-axis title
    axis.text.x = element_text(size = 7, face = "bold"),   # X-axis tick labels
    title = element_text(size= 7)
  ) 

pannel <- ggarrange(distribution_density_unloged,yield_ditribution,nrow = 2,labels = "AUTO")
  

# saving 

ggsave(filename = "figures/methods/manuscript/density_yield_distribution_unlogged.png",
       pannel,
       width = 12,
       height = 12,
       units = "cm")
