library(tidyr)
library(dplyr)
library(ggplot2)
data(ames, package = "modeldata")
dim(ames)
plot(ames$Lot_Area,ames$Sale_Price)
spline_rec <- recipe(Sale_Price ~ Longitude, data = ames) |>
  step_spline_natural(Longitude, deg_free = 6, keep_original_cols = TRUE) |>
  prep()

library(splines)

# Get min/max values of age using the range() function
agelims = Wage %>%
  select(age) %>%
  range

dtf<-read.delim("clipboard")
plot(dtf$temp, dtf$ozone)

n=500; err = 1;
x = runif(n);x=sort(x);
y = sin(12*(x+0.2))/(x+0.2) + rnorm(n, 0, err);
plot(x,y, col="red")

