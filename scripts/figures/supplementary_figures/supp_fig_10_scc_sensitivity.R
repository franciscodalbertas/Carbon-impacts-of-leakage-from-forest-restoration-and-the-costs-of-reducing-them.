# load packages ----------------------------------------------------------------

library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyverse)
#-------------------------------------------------------------------------------

# plot the net present value of the SCC

df <- readRDS("input_tables/SCC_rcp45.rds")

scc_2021 = 133.08431

df %>%
  ggplot(  aes(x = Year, y = SCC_t))+
  geom_line()+
  scale_x_continuous(breaks = c(2021,2041,2061,2081)) +
  theme_classic()

df$ratio = df$SCC_t/scc_2021

# Define custom breaks for alignment
primary_breaks <- round(seq(min(df$SCC_t), max(df$SCC_t, na.rm = TRUE), length.out = 4),1)  # Adjust number of breaks
secondary_breaks <- round(primary_breaks / scc_2021,2)  # Ensuring alignment

scc_plot <- df %>%
  ggplot(aes(x = Year)) +
  geom_line(aes(y = SCC_t), color = "darkgrey") +  # First y-axis
  #geom_line(aes(y = SCC_t / scc_2021), color = "red") +  # Second variable scaled
  # scale_x_continuous(
  #   breaks = c(2021, 2041, 2061, 2081)
  #   ) +
  scale_y_continuous(
    breaks = primary_breaks,
    name = expression("Social Cost of Carbon (SCC"["t"]^"disc" * ")")
    ,  # First y-axis label
    sec.axis = sec_axis(~ ./scc_2021, name = expression(R[t]),
                        breaks = secondary_breaks,
                        #labels = c("0.6","0.7","0.9","1")
    )  # Second y-axis label
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(size = 7), # Y-axis title
    axis.text.y = element_text(size = 7),  # Y-axis tick labels
    axis.title.x = element_text(size = 7), # X-axis title
    axis.text.x = element_text(size = 7))



# ggsave(plot = scc_plot,"figures/methods/manuscript/SCC_over_time.png",
#        width = 12,
#        height = 10,
#        units = "cm")  

#---- version with all curves ----


# plot the net present value of the SCC

df <- readRDS("input_tables/SCC_rcp45_sensibility.rds")

# wide to long

df_l <- df %>% pivot_longer(cols=2:4)

sec.axis = sec_axis(
  ~ . / 133.1,
  name = bquote(R[t] ~ "(normalized to" ~ SCC[3 * "%" ] ~ ")")
)

scc_plot_sensib <- ggplot(df_l, aes(x = Year, y = value, color = name, linetype = name)) +
  geom_line(size = 1) +
  scale_color_manual(
    values = c("SCC_t_2" = "black", "SCC_t_3" = "gray40", "SCC_t_4" = "gray70"),
    labels = c("2% (start = 636.5)", "3% (start = 133.0)", "4% (start = 68.2)"),
    name = NULL
  ) +
  scale_linetype_manual(
    values = c("SCC_t_2" = "solid", "SCC_t_3" = "dashed", "SCC_t_4" = "dotted"),
    guide = "none"
  ) +
  scale_y_continuous(
    name = expression("Social Cost of Carbon (SCC"["t"]^"disc" * ")"),
    sec.axis = sec.axis) +
  theme_classic() +
  theme(
    legend.position = "top",
    axis.title.y.right = element_text(color = "black"),
    axis.title.y.left = element_text(color = "black"),
    axis.title.y = element_text(size = 7), # Y-axis title
    axis.text.y = element_text(size = 7),  # Y-axis tick labels
    axis.title.x = element_text(size = 7), # X-axis title
    axis.text.x = element_text(size = 7)
  )

scc_plot_sensib  

#---- new panel figure ----

# individual curves aligned!

plot_function <- function(df,SCC){
  df2 <- df%>%filter(name == SCC)
  if(SCC == "SCC_t_2"){
    lab <-  "2% (start = 636.5)"
    ylab <- expression("Social Cost of Carbon (SCC"["t"]^"disc" * ")")
  }else if(SCC  == "SCC_t_3"){
    lab  <-  "3% (start = 133.1)"
    ylab <- ""
  }else{
    lab  <-  "4% (start = 68.2)"
    ylab <- ""
  }
  
  p <- ggplot(df2, aes(x = Year, y = value, color = name, linetype = name)) +
    geom_line(size = 1) +
    scale_color_manual(
      values = c("SCC_t_2" = "black", "SCC_t_3" = "gray40", "SCC_t_4" = "gray70"),
      labels = lab,
      name = NULL
    ) +
    scale_linetype_manual(
      values = c("SCC_t_2" = "solid", "SCC_t_3" = "dashed", "SCC_t_4" = "dotted"),
      guide = "none"
    ) +
    scale_y_continuous(
      name = ylab,
      #sec.axis = sec.axis
    ) +
    #facet_grid(~name,scales = "free") +
    theme_classic() +
    theme(
      legend.position = "top",
      axis.title.y.right = element_text(color = "black"),
      axis.title.y.left = element_text(color = "black"),
      axis.title.y = element_text(size = 7), # Y-axis title
      axis.text.y = element_text(size = 7),  # Y-axis tick labels
      axis.title.x = element_text(size = 7), # X-axis title
      axis.text.x = element_text(size = 7)
    )
  
  return(p)
  
}

scc_plot_sensib_1 <- plot_function(df=df_l,SCC = "SCC_t_2")
scc_plot_sensib_2 <- plot_function(df=df_l,SCC = "SCC_t_3")
scc_plot_sensib_3 <- plot_function(df=df_l,SCC = "SCC_t_4")

top_fig <- ggarrange(scc_plot_sensib_1,
                     scc_plot_sensib_2,
                     scc_plot_sensib_3,
                     ncol=3)

# multiple curves with the r_ratio only

df_l <- df_l%>%
  group_by(name)%>%
  mutate(r_t = case_when(name == "SCC_t_2"~value/636.51585,
                         name == "SCC_t_3"~value/133.08431,
                         name == "SCC_t_4"~value/68.22638))




r_t_p <- ggplot(df_l, aes(x = Year, y = r_t, color = name, linetype = name)) +
  geom_line(size = 1) +
  scale_color_manual(
    values = c("SCC_t_2" = "black", "SCC_t_3" = "gray40", "SCC_t_4" = "gray70"),
    #labels = lab,
    name = NULL
  ) +
  scale_linetype_manual(
    values = c("SCC_t_2" = "solid", "SCC_t_3" = "dashed", "SCC_t_4" = "dotted"),
    guide = "none"
  ) +
  scale_y_continuous(
    name = bquote(R[t] ~ "(normalized to" ~ SCC["t=0" ] ~ ")"),
    #sec.axis = sec.axis
  ) +
  scale_x_continuous(breaks = seq(2021,2220,20))+
  #facet_grid(~name,scales = "free") +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.title.y.right = element_text(color = "black"),
    axis.title.y.left = element_text(color = "black"),
    axis.title.y = element_text(size = 7), # Y-axis title
    axis.text.y = element_text(size = 7),  # Y-axis tick labels
    axis.title.x = element_text(size = 7), # X-axis title
    axis.text.x = element_text(size = 7)
  )


full_fig <- ggarrange(top_fig,
                      r_t_p,
                      nrow=2,
                      heights = c(1,2))

full_fig

ggsave(plot = full_fig,"figures/supp_figures/supp_fig_10.png",
       width = 14,
       height = 14,
       units = "cm")  
