# this figure should bring about the costs comparisons of not mitigating vs
# mitigating across scc discount and extensifications

# ---- load packages -----------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggpp)
#-------------------------------------------------------------------------------

# reading the data 
df <- readRDS("output_tables/testing_scc_leakage_sensitivity.rds") 


# change lables
df <- df %>%
  mutate(yield_carbon_ratio = case_when(yield_carbon_ratio=="max"~"high",
                                        yield_carbon_ratio=="min"~"low",
                                        yield_carbon_ratio=="random"~yield_carbon_ratio))

# create a column to fill!
df <- df %>%
  mutate(fill_col= paste0(mitigation_strategy,"_",yield_carbon_ratio))



#---- ploting ------------------------------------------------------------------

# make a function to wrap all that
# still need to modify the labelling of the dotted lines: not good for the amazon!
# I need to reduce figure width up to 15 cm...and then workout the height


plot_function <- function(biome, df = df){

  #biome is either Amazônia or Mata Atlântica
  #=============================================================================
  # 1st row of the panel r=3%; l=30% and l = 100%
  #=============================================================================
  
  df_1 <- df %>% filter(disc=="3%",
                        #leakage =="100%"
                        )
  
  # define order of the x-axis
  
  df_1 $yield_carbon_ratio <-  factor(df_1 $yield_carbon_ratio,
                                    levels = (c("random","high","low")))
  
  # calculate dataframe with mean and confint
  
  medians <- df_1  %>%
    group_by(yield_carbon_ratio,mitigation_strategy,name_biome,leakage)%>%
    # exclude negative values
    filter(CO2_cost_USD>=0)%>%
    summarise(median = median(CO2_cost_USD),
              low = quantile(CO2_cost_USD, 0.025, na.rm = TRUE),
              high = quantile(CO2_cost_USD, 0.75, na.rm = TRUE),
              .groups = "drop" )%>%bind_rows(data.frame(yield_carbon_ratio = "high",
                                                        mitigation_strategy = "no_mitigation",
                                                        name_biome = "Mata Atlântica",
                                                        median = NA,
                                                        low =NA,
                                                        high=NA))
  
  # for p1 (i think this should be only the no mitigation 30%)
  med_random <- medians %>%
    filter(yield_carbon_ratio == "random",
           leakage == "30%",
           name_biome ==biome)%>%
    pull(median)
  
  y_label = expression("Cost tCO"[2]*"e (USD)") 
  
  pos <- position_dodge2(width = 0.55, preserve = "single", padding = 0)  # use same for all plots
  
  if(biome == "Mata Atlântica"){
  
    df_1_2plot <- df_1 %>%
    filter(name_biome == biome,
           yield_carbon_ratio!="random",
    ) %>%
    mutate(
      CO2_cost_USD = if_else(CO2_cost_USD<0,1,CO2_cost_USD)
    )%>%
    droplevels()
    
    # define minimum value for the x axis (it is only necessary for the af plot...)
    limit_x <- c(6, NA)
    
  }else{
    df_1_2plot <- df_1 %>%
      filter(name_biome == biome,
             yield_carbon_ratio!="random",
      ) %>%
      droplevels()
    
    limit_x <- c(NA, NA)
    
  }
  # old code
  p1 <- df_1_2plot %>%
    ggplot(aes(x = mitigation_strategy,
               y = CO2_cost_USD,
               fill = yield_carbon_ratio)) +
    geom_boxplot(
      position = pos,
      width = 0.55,
      trim = FALSE,
      outliers = F
      ) +
    shadowtext::geom_shadowtext(
      data = medians %>% filter(name_biome == biome,
                                yield_carbon_ratio!="random"),
      aes(x = mitigation_strategy,
          y = median,
          label = round(median, 0),
          colour = yield_carbon_ratio,
          group = yield_carbon_ratio),
      position = pos,                        # same dodge as violins
      size = 2.5,
      bg.colour = "white",
      r.g = 0.2,
      colour = "black",
      inherit.aes = FALSE
    ) +
    scale_y_log10(limits = limit_x, oob = scales::oob_keep) +
    scale_fill_manual(
      name = "Forgone-production-to-carbon-gained",
      values = c("lightgreen","darkgreen"),
      breaks = c("high","low"),                              # exact level order
      labels = c("high", "low"),
      drop = FALSE
    ) +
    ylab(y_label)+
    xlab("")+
    theme_classic()+
    theme(
      legend.position = "top",
      text = element_text(size = 7),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  
   # add average cost value
  p1 <- p1 + geom_hline(yintercept = med_random, linetype = "dotted")
  
  # add annotations about the scenario
  p1 <- p1+
    coord_cartesian(clip = "off") + # so the label can sit snugly at the edge
    annotate(
      "text",
      x = -Inf, y = Inf,# upper-left corner of the panel
      label = "r = 3%; l = 100%",
      hjust = -0.05, vjust = 1.2, # nudge in from the edges
      size = 2.5
    )
  # add label on top of the dotted line (average cost without mitigation)
  hor_label <- paste0("avg. cost no mitigation =",round(med_random,0)," USD")
  
  p1 <- p1+
    coord_cartesian(clip = "off") + # so the label can sit snugly at the edge
    annotate(
      "text",
      x = Inf, y =med_random*1.5,# upper-left corner of the panel
      label = hor_label,
      hjust = 1.05, 
      vjust = 1.2, # nudge in from the edges
      size = 2.5
    )
  
  
  
  # short margins
  
  tight <- theme(plot.margin = margin(2, 4, 2, 4))  # t, r, b, l in pts
  
  p1 <- p1+ tight
  
  # add anotations in place of the boxplot!! 
  annotation_no_netC <- data.frame(
    name_biome = biome,
    approach = "no_mitigation",
    x = 0.75,  # left of x-axis
    y = 150,     # do it quite high, the text will be vertical
    label = "no carbon net-of-leakage"
  )
  
  # this step should not occur for amazon
  if(biome =="Mata Atlântica"){
  
    p1 <- p1+
      # geom_text_npc(
      #   data = annotation_no_netC,
      #   aes(npcx = 0.2,        # 0 = left, 1 = right
      #       npcy = 0.45,        # 0 = bottom, 1 = top  (pick your vertical spot)
      #       label = label),
      #   angle = 90,
      #   hjust = 0,              # along the rotated baseline
      #   vjust = 0.5,
      #   size  = 2,
      #   colour = "lightgreen",
      #   inherit.aes = FALSE
      # )
      shadowtext::geom_shadowtext(
        data = annotation_no_netC,
          aes(x = 0.8,        # 0 = left, 1 = right
              y = 150,        # 0 = bottom, 1 = top  (pick your vertical spot)
              label = label),
          angle = 90,
        vjust = 0,
        size = 2,
        bg.colour = "lightgreen",
        colour = "darkgray",
        g.r = 0.001,
          inherit.aes = FALSE
      )
  }
  #p1
  
  # note: maybe drop the ditribution, to turn everything even!!
  
  #=============================================================================
  # second row
  #=============================================================================
  df_2 <- df %>% filter(disc=="2%",
                        leakage =="100%")
  
  
  
  
  
  # define order of the x-axis
  
  df_2$yield_carbon_ratio <-  factor(df_2$yield_carbon_ratio,
                                      levels = (c("high","low","random")))
  
  
  if(biome == "Mata Atlântica"){
    
    df_2_2plot <- df_2 %>%
      filter(name_biome == biome,
             yield_carbon_ratio!="random",
      ) %>%
      mutate(
        CO2_cost_USD = if_else(CO2_cost_USD<0,1,CO2_cost_USD)
      )%>%
      droplevels()
    
    # define minimum value for the x axis (it is only necessary for the af plot...)
    #limit_x <- c(5, NA)
    
  }else{
    df_2_2plot <- df_2 %>%
      filter(name_biome == biome,
             yield_carbon_ratio!="random",
      ) %>%
      # mutate(
      #   CO2_cost_USD = if_else(CO2_cost_USD<0,1,CO2_cost_USD)
      # )%>%
      droplevels()
    
    limit_x <- c(NA, NA)
    
  }
  
  medians2 <- df_2  %>%
    group_by(yield_carbon_ratio,mitigation_strategy,name_biome)%>%
    filter(CO2_cost_USD>=0)%>%
    summarise(median = median(CO2_cost_USD),
              low = quantile(CO2_cost_USD, 0.025, na.rm = TRUE),
              high = quantile(CO2_cost_USD, 0.975, na.rm = TRUE),
              .groups = "drop" )%>%bind_rows(data.frame(yield_carbon_ratio = "high",
                                                        mitigation_strategy = "no_mitigation",
                                                        name_biome = "Mata Atlântica",
                                                        median = NA,
                                                        low =NA,
                                                        high=NA))
  
  
  med_random_p2 <- medians2 %>%
    filter(yield_carbon_ratio == "random",
           name_biome ==biome)%>%
    pull(median)
  
  p2 <- df_2_2plot %>%
    ggplot(aes(x = mitigation_strategy,
               y = CO2_cost_USD,
               fill = yield_carbon_ratio)) +
    geom_boxplot(
      position = pos, 
      width = 0.55, 
      trim = FALSE,
       outliers = F
                 ) +
    shadowtext::geom_shadowtext(
      data = medians2 %>% filter(name_biome == biome,yield_carbon_ratio!="random"),
      aes(x = mitigation_strategy,
          y = median,
          label = round(median, 0),
          #fill = mitigation_strategy,        # ensure same grouping
          group = yield_carbon_ratio),
      position = pos,                        # same dodge as violins
      size = 2.5,
      bg.colour = "white",
      colour = "black",
      inherit.aes = FALSE
    ) +
    scale_y_log10(limits = limit_x, oob = scales::oob_keep) +
    scale_fill_manual(
      name = "forgone-production-to-garbon-gained",
      values = c("lightgreen","darkgreen"),
      breaks = c("high","low"),                              # exact level order
      labels = c("high", "low"),
      drop = FALSE
    ) +
    xlab("")+
    ylab(y_label)+
    theme_classic()+
    theme(
      legend.position = "top",
      text = element_text(size = 7),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  
  p2 <- p2 + geom_hline(yintercept = med_random_p2, linetype = "dotted")
  
  p2 <- p2 +
    coord_cartesian(clip = "off") +              # so the label can sit snugly at the edge
    annotate(
      "text",
      x = -Inf, y = Inf,                         # upper-left corner of the panel
      label = "r = 2%; l = 100%",
      hjust = -0.05, vjust = 1.2,                # nudge in from the edges
      size = 2.5
    )+
    tight
  
  # only for the AF!!
  
  if(biome=="Mata Atlântica"){
  
  p2 <- p2 +
    # geom_text_npc(
    #   data = annotation_no_netC,
    #   aes(npcx = 0.2,        # 0 = left, 1 = right
    #       npcy = 0.47,        # 0 = bottom, 1 = top  (pick your vertical spot)
    #       label = label),
    #   angle = 90,
    #   hjust = 0,              # along the rotated baseline
    #   vjust = 0.5,
    #   size  = 2,
    #   colour = "lightgreen",
    #   inherit.aes = FALSE
    # )
    shadowtext::geom_shadowtext(
      data = annotation_no_netC,
      aes(x = 0.8,        # 0 = left, 1 = right
          y = 150,        # 0 = bottom, 1 = top  (pick your vertical spot)
          label = label),
      angle = 90,
      vjust = 0,
      size = 2,
      bg.colour = "lightgreen",
      colour = "darkgray",
      g.r = 0.001,
      inherit.aes = FALSE
    )
  
  }
  
  # add label on top of the dotted line (average cost without mitigation)
  hor_label2 <- paste0("avg. cost no mitigation =",round(med_random_p2,0)," USD")
  
  # add label on top of the dotted line (average cost without mitigation)
  p2 <- p2+
    coord_cartesian(clip = "off") + # so the label can sit snugly at the edge
    annotate(
      "text",
      x = Inf, y =med_random_p2*1.5,# upper-left corner of the panel
      label = hor_label2,
      hjust = 1.05, 
      vjust = 1.2, # nudge in from the edges
      size = 2.5
    )
  
  
  #p2
  #=============================================================================
  # 3rd row
  #=============================================================================
  
  df_3 <- df %>% filter(disc=="4%",
                        leakage =="100%")
  
  # define order of the x-axis
  
  df_3$yield_carbon_ratio <-  factor(df_3$yield_carbon_ratio,
                                     levels = (c("high","low","random")))
  
  medians3 <- df_3  %>%
    group_by(yield_carbon_ratio,mitigation_strategy,name_biome)%>%
    filter(CO2_cost_USD>=0)%>%
    summarise(median = median(CO2_cost_USD),
              low = quantile(CO2_cost_USD, 0.025, na.rm = TRUE),
              high = quantile(CO2_cost_USD, 0.975, na.rm = TRUE),
              .groups = "drop" )%>%bind_rows(data.frame(yield_carbon_ratio = "high",
                                                        mitigation_strategy = "no_mitigation",
                                                        name_biome = "Mata Atlântica",
                                                        median = NA,
                                                        low =NA,
                                                        high=NA))
  
  
  med_random_p3 <- medians3 %>%
    filter(yield_carbon_ratio == "random",
           name_biome ==biome)%>%
    pull(median)
  
  if(biome == "Mata Atlântica"){
    
    df_3_2plot <- df_3 %>%
      filter(name_biome == biome,
             yield_carbon_ratio!="random",
      ) %>%
      mutate(
        CO2_cost_USD = if_else(CO2_cost_USD<0,1,CO2_cost_USD)
      )%>%
      droplevels()
    
    # define minimum value for the x axis (it is only necessary for the af plot...)
    limit_x <- c(12, NA)
    
  }else{
    df_3_2plot <- df_3 %>%
      filter(name_biome == biome,
             yield_carbon_ratio!="random",
      ) %>%
      # mutate(
      #   CO2_cost_USD = if_else(CO2_cost_USD<0,1,CO2_cost_USD)
      # )%>%
      droplevels()
    
    limit_x <- c(NA, NA)
    
  }
  
  p3 <- df_3_2plot %>%
    ggplot(aes(x = mitigation_strategy,
               y = CO2_cost_USD,
               fill = yield_carbon_ratio)) +
    geom_boxplot(
      position = pos, 
      width = 0.55, 
      trim = FALSE,
      outliers = F) +
    shadowtext::geom_shadowtext(
      data = medians3 %>% filter(name_biome == biome,yield_carbon_ratio!="random"),
      aes(x = mitigation_strategy,
          y = median,
          label = round(median, 0),
          #fill = mitigation_strategy,        # ensure same grouping
          group = yield_carbon_ratio),
      position = pos,                        # same dodge as violins
      size = 2.5,
      bg.colour = "white",
      colour = "black",
      inherit.aes = FALSE
    ) +
    scale_y_log10(limits = limit_x, oob = scales::oob_keep) +
    scale_fill_manual(
      name = "Forgone-production-to-carbon-gained",
      values = c("lightgreen","darkgreen"),
      breaks = c("high","low"),                              # exact level order
      labels = c("high", "low"),
      drop = FALSE
    ) +
    xlab("")+
    ylab(y_label)+
    theme_classic()+
    theme(
      legend.position = "top",
      text = element_text(size = 7),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  
  p3 <- p3 + geom_hline(yintercept = med_random_p3, linetype = "dotted")
  
  p3 <- p3 +
    coord_cartesian(clip = "off") +              # so the label can sit snugly at the edge
    annotate(
      "text",
      x = -Inf, y = Inf,                         # upper-left corner of the panel
      label = "r = 4%; l = 100%",
      hjust = -0.05, vjust = 1.2,                # nudge in from the edges
      size = 2.5
    )+
    tight
  
  
  if(biome=="Mata Atlântica"){
  
  p3 <- p3 +
    # geom_text_npc(
    #   data = annotation_no_netC,
    #   aes(npcx = 0.2,        # 0 = left, 1 = right
    #       npcy = 0.47,        # 0 = bottom, 1 = top  (pick your vertical spot)
    #       label = label),
    #   angle = 90,
    #   hjust = 0,              # along the rotated baseline
    #   vjust = 0.5,
    #   size  = 2,
    #   colour = "lightgreen",
    #   inherit.aes = FALSE
    # )
    shadowtext::geom_shadowtext(
      data = annotation_no_netC,
      aes(x = 0.8,        # 0 = left, 1 = right
          y = 150,        # 0 = bottom, 1 = top  (pick your vertical spot)
          label = label),
      angle = 90,
      vjust = 0,
      size = 2,
      bg.colour = "lightgreen",
      colour = "darkgray",
      g.r = 0.001,
      inherit.aes = FALSE
    )
  
  }
  hor_label3 <- paste0("avg. cost no mitigation =",round(med_random_p3,0)," USD")
  # add label on top of the dotted line (average cost without mitigation)
  p3 <- p3+
    coord_cartesian(clip = "off") + # so the label can sit snugly at the edge
    annotate(
      "text",
      x = Inf, y =med_random_p3*1.5,# upper-left corner of the panel
      label = hor_label3,
      hjust = 1.05, 
      vjust = 1.2, # nudge in from the edges
      size = 2.5
    )
  
  
  #p3
  
  #=============================================================================
  # 4rd row (30% leakage only)
  #=============================================================================
  
  df_4 <- df %>% filter(disc=="3%",
                        leakage =="30%")
  
  # define order of the x-axis
  
  df_4$yield_carbon_ratio <-  factor(df_4$yield_carbon_ratio,
                                     levels = (c("high","low","random")))
  
  medians4 <- df_4  %>%
    group_by(yield_carbon_ratio,mitigation_strategy,name_biome)%>%
    filter(CO2_cost_USD>=0)%>%
    mutate(mitigation_strategy = if_else(mitigation_strategy=="optimize_carbon","mitigation",mitigation_strategy))%>%
    summarise(median = median(CO2_cost_USD),
              low = quantile(CO2_cost_USD, 0.025, na.rm = TRUE),
              high = quantile(CO2_cost_USD, 0.975, na.rm = TRUE),
              .groups = "drop" )
  
  medians4$mitigation_strategy <- factor(medians4$mitigation_strategy,levels = (c("no_mitigation","mitigation")))
  
  df_4$mitigation_strategy <- factor(df_4$mitigation_strategy,levels =(c("no_mitigation","optimize_carbon")))
  
  med_random_p4 <- medians4 %>%
    filter(yield_carbon_ratio == "random",
           name_biome ==biome)%>%
    pull(median)
  
  
  p4 <- df_4 %>%
    filter(name_biome == biome,
           yield_carbon_ratio!="random") %>%
    droplevels()%>%
    mutate(mitigation_strategy = if_else(mitigation_strategy=="optimize_carbon","mitigation",mitigation_strategy))%>%
    ggplot(aes(x = mitigation_strategy,
               y = CO2_cost_USD,
               fill = yield_carbon_ratio)) +
    geom_boxplot(
      position = pos, 
      width = 0.55, 
      trim = FALSE,
      outliers = F) +
    shadowtext::geom_shadowtext(
      data = medians4 %>% filter(name_biome == biome,yield_carbon_ratio!="random"),
      aes(x = mitigation_strategy,
          y = median,
          label = round(median, 0),
          #fill = mitigation_strategy,        # ensure same grouping
          group = yield_carbon_ratio ),
      position = pos,                        # same dodge as violins
      size = 2.5,
      bg.colour = "white",
      colour = "black",
      inherit.aes = FALSE
    ) +
    scale_y_log10() +
    scale_x_discrete(labels = c("no mitigation","mitigation"))+
    xlab("")+
    ylab(y_label)+
    scale_fill_manual(
      name = "forgone-production-to-carbon-gained",
      values = c("lightgreen","darkgreen"),
      breaks = c("high","low"),                              # exact level order
      labels = c("high", "low"),
      drop = FALSE
    ) +
    theme_classic()+
    theme(
      legend.position = "top",
      text = element_text(size = 7),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      #axis.text.x = element_blank(),
      #axis.ticks.x = element_blank()
    )
  
  p4 <- p4 + geom_hline(yintercept = med_random_p4, linetype = "dotted")
  
  p4 <- p4 +   
    coord_cartesian(clip = "off") +              # so the label can sit snugly at the edge
    annotate(
      "text",
      x = -Inf, y = Inf,                         # upper-left corner of the panel
      label = "r = 3%; l = 30%",
      hjust = -0.05, vjust = 1.2,                # nudge in from the edges
      size = 2.5
    )+
    tight
  
  
  hor_label4 <- paste0("avg cost no mitigation =",round(med_random_p4,0)," USD")
  
  if(biome =="Mata Atlântica"){m_factor=2}else{m_factor = 1.5}
  p4 <- p4+
    coord_cartesian(clip = "off") + # so the label can sit snugly at the edge
    annotate(
      "text",
      x = Inf, y =med_random_p4*m_factor,# upper-left corner of the panel
      label = hor_label4,
      hjust = 1.05, 
      vjust = 1.2, # nudge in from the edges
      size = 2.5
    )
  
  
  panel <- ggarrange(p1,p2,p3,p4,common.legend = T,nrow=4,labels = "AUTO",align = "v")
  
    #panel
  
  #p2 is rubish!!
  # still need to work on it!! 
  if(biome != "Amazônia"){
    ggsave(plot = panel,"figures/extended_figures/boxplot_sensitivity_discount_leakage_AF.png",
          width = 15,height = 20,units = "cm")
  }else{
    ggsave(plot = panel,"figures/extended_figures/boxplot_sensitivity_discount_leakage_AM.png",
           width = 15,height = 20,units = "cm")
    
    }
  
  return(panel)

}

# run for the atlantic forest

af_plot <- plot_function(biome = "Mata Atlântica",df = df)

# probably the amazon is different?!!! check!! yes. it is!
am_plot <- plot_function(biome = "Amazônia",df = df)
