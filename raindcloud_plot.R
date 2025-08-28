library(ggplot2)
library(dplyr)
library(ggdist)
library(tidyquant)
library(tidyverse)
library(tidyquant)
library(ggdist)
library(ggthemes)

mpg %>% 
  filter(cyl == 4) %>% 
  ggplot(aes(x = "", y = hwy)) +
  
  # Half-violin
  stat_halfeye(
    adjust = 0.5,
    justification = -0.2,
    .width = 0,
    point_colour = NA,
    fill = "#2c3e50"  # You can customize the fill color here
  ) +
  
  # Boxplot
  geom_boxplot(
    width = 0.12,
    outlier.color = NA,
    alpha = 0.5,
    fill = "#2980b9"
  ) +
  
  # Dots
  stat_dots(
    side = "left",
    justification = 1.1,
    binwidth = 0.25
  ) +
  
  # Labels and theme
  theme_tq() +
  labs(
    title = "RainCloud Plot: Cylinders = 4",
    x = "",
    y = "Highway Fuel Economy (hwy)"
  ) +
  
  coord_flip()
