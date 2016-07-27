## visualize_pairwise_dist.R
##
## visualize the pairwise dist between hours and color dist
##
setwd("~/Documents/twitter-diurnal-analysis/")


library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(reshape)
library(GGally)
library(pairwiseDist)


dt = fread("./data/cities_HSV_dist_corr_mean.csv", header = TRUE)

head(dt)

subset.cities = c("New York", "Chicago", "Los Angeles", "Houston", "Washington")

dt.sub = subset(dt, city %in% subset.cities)

ggplot(dt.sub, aes(x = dist.hours.distances.H, y = hue.dist, colour = city)) + 
  geom_point(size = 1.5) + stat_smooth(method = "lm") + scale_colour_brewer(palette="Set1") -> p 
