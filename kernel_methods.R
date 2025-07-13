library(ggplot2)
library(psych)
library(lattice)
library(KernSmooth)
data(mpg)
pairs.panels(mpg)

# histograms
hist(mpg$hwy, breaks = seq(6, 50, 2))
hist(mpg$hwy)
hist(mpg$hwy,breaks="FD")
ggplot(mpg, aes(x = hwy)) +
  geom_histogram(color="black",fill="white") +
  theme_minimal()
histogram(~ hwy, data = mpg)

x <- mpg$hwy
bin_width <- dpih(x)
nbins <- seq(min(x) - bin_width,
             max(x) + bin_width,
             by = bin_width)
hist(x, breaks = nbins,
     main = "Plug-in method")

# uniform kernel
xgrid = seq(6, 50, 0.1)
histden = sapply(xgrid, FUN = function(x, obs, h) sum( ((x-h/2) <= obs) * ((x+h/2) > obs))/h/length(obs), 
                 obs = mpg$hwy, h = 2)
hist(mpg$hwy, breaks = seq(6, 50, 2))
plot(xgrid, histden, type = "s")

# histogram with density
ggplot(mpg, aes(x = hwy,y=after_stat(density))) +
  geom_histogram(color="black",fill="white") +
  geom_density(alpha=.2)+
  theme_minimal()
