# -*- coding: utf-8 -*-
# author: xiaoyong.li@sdut.edu.cn

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

# Load and organize data
df <- read_excel("cstorage_trend_fig.xlsx")

df <- df %>%
  rename(
    FVegC_total   = FVegC,
    FSoilC_total  = FSoilC,
    PFVegC_total  = PFVegC,
    PFSoilC_total = PFSoilC,
    NFVegC_density = NFVegCD,
    PFVegC_density = PFVegCD,
    NFSoilC_density = NFSoilCD,
    PFSoilC_density = PFSoilCD
  )

df$decade <- factor(df$decade, levels = unique(df$decade))


# ------------ Panel a: Total Forest Carbon Storage ------------
df_a <- df %>%
  dplyr::select(decade, FVegC_total, FSoilC_total) %>%
  pivot_longer(cols = -decade, names_to = "Type", values_to = "Value") %>%
  mutate(
    Type_show = recode(Type,
                       "FVegC_total" = "VegC",
                       "FSoilC_total" = "SOC")
  )

p_a <- ggplot(df_a, aes(decade, Value, fill = Type_show)) +
  geom_col(color = '#B0B0B0', linewidth = 0.2) +
  scale_fill_manual(values = c(VegC="#4A7C59", SOC="#D8A76F")) +
  labs(y="Forest Carbon storage (Pg C)", x=NULL) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    text = element_text(family = "Arial", size = 8, color = "black"),
    axis.text = element_text(family = "Arial", size = 8, color = "black"),
    axis.title = element_text(family = "Arial", size = 8, color = "black"),
    axis.line.x = element_line(size=0.2, color="black"),
    axis.ticks.x = element_line(size=0.2, color="black"),
    axis.line.y = element_line(size=0.2, color="black"),
    axis.ticks.y = element_line(size=0.2, color="black"),
    axis.ticks.length = unit(3, "pt"),
    axis.text.x = element_text(angle=45, hjust=1),
    legend.position = c(0.2, 0.85),
    legend.background = element_rect(fill=alpha("white",0.7), color=NA),
    legend.key.size = unit(0.6, "lines"),
    legend.text = element_text(size=8),
    legend.title = element_blank()
  ) +
  scale_x_discrete(breaks = c("1910s", "1930s", "1950s", "1970s", "1990s", "2010s")) +
  scale_y_continuous(
    limits = c(0, 21),
    breaks = seq(0, 18, by = 5)
  ) +
  annotate("text", x=-Inf, y=Inf, label="a", 
           hjust=-0.5, vjust=1.5, size=4, family="Arial")

# ------------ Panel b: Total PF Carbon Storage ------------
df_b <- df %>%
  dplyr::select(decade, PFVegC_total, PFSoilC_total) %>%
  pivot_longer(cols = -decade, names_to = "Type", values_to = "Value") %>%
  mutate(
    Type_show = recode(Type,
                       "PFVegC_total" = "VegC",
                       "PFSoilC_total" = "SOC")
  )

p_b <- ggplot(df_b, aes(decade, Value, fill = Type_show)) +
  geom_col(color = '#B0B0B0', linewidth = 0.2) +
  scale_fill_manual(values=c(VegC="#66C2A5", SOC="#F4A582")) +
  labs(y="PF Carbon storage (Pg C)", x=NULL) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle=45, hjust=1),
    text = element_text(family = "Arial", size = 8, color = "black"),
    axis.text = element_text(family = "Arial", size = 8, color = "black"),
    axis.title = element_text(family = "Arial", size = 8, color = "black"),
    axis.line.x = element_line(size=0.2, color="black"),
    axis.ticks.x = element_line(size=0.2, color="black"),
    axis.line.y = element_line(size=0.2, color="black"),
    axis.ticks.y = element_line(size=0.2, color="black"),
    axis.ticks.length = unit(3, "pt"),
    legend.position = c(0.2, 0.85),
    legend.background = element_rect(fill=alpha("white",0.7), color=NA),
    legend.key.size = unit(0.6, "lines"),
    legend.text = element_text(size=8),
    legend.title = element_blank()
  ) +
  scale_x_discrete(breaks = c("1910s", "1930s", "1950s", "1970s", "1990s", "2010s")) +
  scale_y_continuous(
    limits = c(0, 2.4),
    breaks = seq(0, 2.0, by = 0.5)
  ) +
  annotate("text", x=-Inf, y=Inf, label="b", 
           hjust=-0.5, vjust=1.5, size=4, family="Arial")

# ------------ Panel c: VegC Density ------------
df_c <- df %>%
  dplyr::select(decade, NFVegC_density, PFVegC_density) %>%
  pivot_longer(cols = -decade, names_to = "Type", values_to = "Value") %>%
  mutate(
    Type_show = recode(Type,
                       "NFVegC_density" = "NF",
                       "PFVegC_density" = "PF")
  )

p_c <- ggplot(df_c, aes(decade, Value, group = Type_show, color = Type_show)) +
  geom_line(linewidth=0.6, alpha=0.9) +
  geom_point(shape=1, size=2.0, stroke=0.8) +
  scale_color_manual(values=c(NF="#66C2A5", PF="#F28E2B")) +
  labs(y=expression("VegC density (Mg C ha"^-1*")"), x=NULL) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle=45, hjust=1),
    text = element_text(family = "Arial", size = 8, color = "black"),
    axis.text = element_text(family = "Arial", size = 8, color = "black"),
    axis.title = element_text(family = "Arial", size = 8, color = "black"),
    axis.line.x = element_line(size=0.2, color="black"),
    axis.ticks.x = element_line(size=0.2, color="black"),
    axis.line.y = element_line(size=0.2, color="black"),
    axis.ticks.y = element_line(size=0.2, color="black"),
    axis.ticks.length = unit(3, "pt"),
    legend.position = c(0.85, 0.85),
    legend.background = element_rect(fill=alpha("white",0.7), color=NA),
    legend.key.size = unit(0.5,"lines"),
    legend.text = element_text(size=8),
    legend.title = element_blank()
  ) +
  scale_x_discrete(breaks = c("1910s", "1930s", "1950s", "1970s", "1990s", "2010s")) +
  scale_y_continuous(
    limits = c(0, 120),
    breaks = seq(0, 110, by = 30)
  ) +
  annotate("text", x=-Inf, y=Inf, label="c", 
           hjust=-0.5, vjust=1.5, size=4, family="Arial")

# ------------ Panel d: SOC Density ------------
df_d <- df %>%
  dplyr::select(decade, NFSoilC_density, PFSoilC_density) %>%
  pivot_longer(cols = -decade, names_to = "Type", values_to = "Value") %>%
  mutate(
    Type_show = recode(Type,
                       "NFSoilC_density" = "NF",
                       "PFSoilC_density" = "PF")
  )

p_d <- ggplot(df_d, aes(decade, Value, group = Type_show, color = Type_show)) +
  geom_line(linewidth=0.6, alpha=0.9) +
  geom_point(shape=1, size=1.5, stroke=0.8) +
  scale_color_manual(values=c(NF="#66C2A5", PF="#F28E2B")) +
  labs(y=expression("SOC density (Mg C ha"^-1*")"), x=NULL) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle=45, hjust=1),
    text = element_text(family = "Arial", size = 8, color = "black"),
    axis.text = element_text(family = "Arial", size = 8, color = "black"),
    axis.title = element_text(family = "Arial", size = 8, color = "black"),
    axis.line.x = element_line(size=0.2, color="black"),
    axis.ticks.x = element_line(size=0.2, color="black"),
    axis.line.y = element_line(size=0.2, color="black"),
    axis.ticks.line.y = element_line(size=0.2, color="black"),
    axis.ticks.length = unit(3, "pt"),
    legend.position = c(0.85, 0.85),
    legend.background = element_rect(fill=alpha("white",0.7), color=NA),
    legend.key.size = unit(0.5,"lines"),
    legend.text = element_text(size=8),
    legend.title = element_blank()
  ) +
  scale_x_discrete(breaks = c("1910s", "1930s", "1950s", "1970s", "1990s", "2010s")) +
  scale_y_continuous(
    limits = c(0, 120),
    breaks = seq(0, 110, by = 30)
  ) +
  annotate("text", x=-Inf, y=Inf, label="d", 
           hjust=-0.5, vjust=1.5, size=4, family="Arial")

# ------------ Combine and Save Plots ------------
final_plot <- grid.arrange(p_a, p_b, p_c, p_d, ncol = 2)

ggsave("fig6.jpg",
       final_plot,
       width=14, height=10, unit = 'cm',
       dpi=300)