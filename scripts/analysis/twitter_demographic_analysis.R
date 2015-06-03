## twitter_demographic_analysis.R
##
## basic analysis of modes and faces 
##

setwd("~/Documents/twitter-diurnal-analysis/")


library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(reshape)
library(plyr)
library(GGally)
load("./data/housing_and_ACS.Rda")

f$RegionName = as.character(f$RegionName)
f$RegionName[which(f$RegionName == "Saint Louis")] = "St. Louis"

df.demographic = f[,c("RegionName", "bachelors", "disabled", "income", "pop", "unemployed", "median.2013")]
names(df.demographic)[1] = "city"

df.demographic$log.pop = log(df.demographic$pop)
df.demographic$log.median.2013 = log(df.demographic$median.2013)


cities.faces = read.csv("./data/city_faces.csv", header = TRUE, stringsAsFactors = FALSE)
cities.entropy = read.csv("./data/city_diurnal_faces_entropies.csv", header = TRUE, stringsAsFactors = FALSE) 
cities.modes.entropy = read.csv("./data/city_diurnal_entropies.csv", header = TRUE, stringsAsFactors = FALSE)
names(cities.entropy)[-1] = paste0(names(cities.entropy)[-1], ".entropy")

city.color.corr = read.csv("./data/city_correlation_HUE_probs.csv", header = TRUE, stringsAsFactors = FALSE)
city.hue.corr = read.csv("./data/cities_hue_dist_corr_mean.csv", header = TRUE, stringsAsFactors = FALSE)

df = join_all(list(cities.entropy, cities.faces, cities.modes.entropy, df.demographic, city.color.corr, city.hue.corr))

##
## linear modelling of housing prices
##

housing.census = lm(median.2013 ~ income + disabled + unemployed + bachelors, data = df)
summary(housing.census)

housing.twitter = lm(median.2013 ~ solo.face.rate.alt.tree.entropy + num.rows/pop + hue.corr + solo.face.alt.tree/pop, data = df)
summary(housing.twitter)                              
