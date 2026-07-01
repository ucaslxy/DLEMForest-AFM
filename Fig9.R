# -*- coding: utf-8 -*-
# author: xiaoyong.li@sdut.edu.cn

library(readxl)
library(tidyverse)
library(gridExtra)
library(cowplot)
library(extrafont)
library(grid)

# Load data
df <- read_excel("factor_fig9.xlsx")

# Define color schemes
colors_4 <- c(
  "Mgmt"    = "#D8A76F",
  "CO2"     = "#B0B0B0",
  "Ndep"    = "#B6463A",
  "Climate" = "#4A7C59"
)

colors_5 <- c(
  "GenImp"   = "#F4A582",
  "SitePrep" = "#92C5DE",
  "Nfert"    = "#B6463A",
  "Thinning" = "#66C2A5",
  "Harvest"  = "#FEE08B"
)

# Prepare 4-factor data for VegC
df_veg4 <- df %>%
  pivot_longer(cols = c(Veg_Mgmt,Veg_CO2,Veg_Ndep,Veg_Climate),
               names_to = "Factor", values_to = "Value") %>%
  mutate(Factor = str_replace(Factor,"Veg_",""),
         Factor = factor(Factor, levels = c("Mgmt","CO2","Ndep","Climate")))

# Prepare 4-factor data for SoilC
df_soil4 <- df %>%
  pivot_longer(cols = c(Soil_Mgmt,Soil_CO2,Soil_Ndep,Soil_Climate),
               names_to = "Factor", values_to = "Value") %>%
  mutate(Factor = str_replace(Factor,"Soil_",""),
         Factor = factor(Factor, levels = c("Mgmt","CO2","Ndep","Climate")))

# Prepare 5-factor data for VegC
df_veg5 <- df %>%
  pivot_longer(cols = c(Veg_GenImp,Veg_SitePrep,Veg_Nfert,Veg_Thinning,Veg_Harvest),
               names_to = "Factor", values_to = "Value") %>%
  mutate(Factor = str_replace(Factor,"Veg_",""),
         Factor = factor(Factor, levels=c("GenImp","SitePrep","Nfert","Thinning","Harvest")))

# Prepare 5-factor data for SoilC
df_soil5 <- df %>%
  pivot_longer(cols = c(Soil_GenImp,Soil_SitePrep,Soil_Nfert,Soil_Thinning,Soil_Harvest),
               names_to = "Factor", values_to = "Value") %>%
  mutate(Factor = str_replace(Factor,"Soil_",""),
         Factor = factor(Factor, levels=c("GenImp","SitePrep","Nfert","Thinning","Harvest")))

# Define plotting function
plot_bar <- function(df_long, title, colors, label=NULL){
  p <- ggplot(df_long, aes(x=Decade, y=Value, fill=Factor)) +
    geom_bar(stat="identity", color = '#B0B0B0', linewidth = 0.2) +
    scale_fill_manual(values=colors) +
    labs(fill = NULL) + 
    theme_bw() +
    theme(
      text = element_text(family = "Arial", size = 8, color = "black"),
      axis.text = element_text(family = "Arial", size = 8, color = "black"),
      axis.title = element_text(family = "Arial", size = 8, color = "black"),
      axis.line.x = element_line(size=0.2, color="black"),
      axis.ticks.x = element_line(size=0.2, color="black"),
      axis.line.y = element_line(size=0.2, color="black"),
      axis.ticks.y = element_line(size=0.2, color="black"),
      axis.text.x = element_text(angle=45, hjust=1),
      axis.ticks.length = unit(3, "pt"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      legend.text = element_text(family = "Arial", size = 8, color = "black"),
      legend.title = element_blank()
    ) +
    geom_hline(yintercept=0, color="black", size = 0.2) +
    scale_x_discrete(breaks = c("1910s", "1930s", "1950s", "1970s", "1990s", "2010s"))+
    scale_y_continuous(
      limits = c(-550, 250),
      breaks = seq(-550, 250, by = 200)
    )+
    ylab(title) +
    xlab("")
  
  if(!is.null(label)){
    p <- p + annotate("text", x=-Inf, y=Inf, label=label, 
                      hjust=-0.5, vjust=1.5,
                      size=4, family="Arial")
  }
  
  return(p)
}

# Create panels
pA <- plot_bar(df_veg4, "Contribution to VegC (Tg C)", colors_4, label="a")
pB <- plot_bar(df_soil4, "Contribution to SOC (Tg C)", colors_4, label="b")
pC <- plot_bar(df_veg5, "Contribution to VegC (Tg C)", colors_5, label="c")
pD <- plot_bar(df_soil5, "Contribution to SOC (Tg C)", colors_5, label="d")

# Extract unified legends
legend_AB <- get_legend(
  ggplot(df_veg4, aes(x=Decade, y=Value, fill=Factor)) +
    geom_bar(stat="identity", color = '#B0B0B0', linewidth = 0.2) +
    scale_fill_manual(
      values = colors_4,
      labels = c(
        "Mgmt"    = "Mgmt",
        "CO2"     = expression(CO[2]),
        "Ndep"    = "Ndep",
        "Climate" = "Climate"
      )
    ) +
    labs(fill = NULL) +
    theme_bw() +
    theme(legend.position="right",
          element_text(family = "Arial", size = 8, color = "black"),
          legend.key.size = unit(12, "pt"),
          legend.spacing.y = unit(6, "pt"))
)

legend_CD <- get_legend(
  ggplot(df_veg5, aes(x=Decade, y=Value, fill=Factor)) +
    geom_bar(stat="identity", color = '#B0B0B0', linewidth = 0.2) +
    scale_fill_manual(values=colors_5) +
    labs(fill = NULL) +
    theme_bw() +
    theme(legend.position="right",
          element_text(family = "Arial", size = 8, color = "black"),
          legend.key.size = unit(12, "pt"),
          legend.spacing.y = unit(6, "pt"))
)

# Helper function to center legends vertically
center_legend <- function(legend, top = 1, bottom = 1){
  arrangeGrob(
    nullGrob(),
    legend,
    nullGrob(),
    ncol = 1,
    heights = c(top, 3, bottom)
  )
}

legend_AB_centered <- center_legend(legend_AB, top = 1.2, bottom = 0.8)
legend_CD_centered <- center_legend(legend_CD, top = 1.2, bottom = 0.8)

# Combine and save the final plot
panel_plot <- grid.arrange(
  arrangeGrob(pA, pB, pC, pD, ncol = 2),
  arrangeGrob(
    legend_AB_centered,
    legend_CD_centered,
    ncol = 1
  ),
  ncol = 2,
  widths = c(5.0, 1.0)
)

ggsave("fig9.jpg",
       panel_plot,
       width=14, height=10, unit = 'cm',
       dpi=300)