# install.packages(c("cowplot", "magick"))  # if needed
library(cowplot)

# read images (magick handles PNG, transparency, etc.)
imgA <- magick::image_read("figures/methods/manuscript/current_yield_map_2021.png")
imgB <- magick::image_read("figures/methods/manuscript/delta_carbon.png")
imgC <- magick::image_read("figures/methods/manuscript/sr_carrying_map_2021.png")
imgD <- magick::image_read("figures/supp/manuscript/pnr_map.png")
imgE <- magick::image_read("figures/methods/manuscript/restoration_costs_USD.png")

# turn each image into a ggplot-like object
pA <- ggdraw() + 
  draw_image(imgA, x = 0, y = 0, width = 1, height = 1, hjust = 0, vjust = 0)+
  theme(legend.title = element_text(size = 7),
        legend.text  = element_text(size = 7))
pB <- ggdraw() + draw_image(imgB, x = 0, y = 0, width = 1, height = 1, hjust = 0, vjust = 0)
pC <- ggdraw() + draw_image(imgC, x = 0, y = 0, width = 1, height = 1, hjust = 0, vjust = 0)
pD <- ggdraw() + draw_image(imgD, x = 0, y = 0, width = 1, height = 1, hjust = 0, vjust = 0)
pE <- ggdraw() + draw_image(imgE, x = 0, y = 0, width = 1, height = 1, hjust = 0, vjust = 0)





# arrange into a panel with automatic A–D tags
panel <- plot_grid(pA, pB, pC, pD,pE,
                   ncol = 2, align = "hv", axis = "tblr",
                   labels = "AUTO", label_size = 8, label_fontface = "bold")


panel <- panel +
  theme(
    legend.title = element_text(size = 20),
    legend.text  = element_text(size = 20)
  )


ggsave("figures/supp/manuscript/maps_pannel.png", panel, 
       width = 15, 
       height = 20,
       units = "cm",
       dpi = 300,
       bg = "white")

ggsave("figures/supp/manuscript/maps_pannel.svg", panel, 
       width = 15, 
       height = 20,
       units = "cm",
       dpi = 300,
       bg = "white")
