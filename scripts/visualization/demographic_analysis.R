## demographi_analysis.R
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


## using ggpairs from: http://stackoverflow.com/questions/21716567/use-ggpairs-to-create-this-plot
## https://github.com/tonytonov/ggally/blob/master/R/gg-plots.r

png(file = "./figures/demographic_pairs.png", width = 800, 
    height=800, units = "px", bg = "white")
#ggpairs(microbiome.df, columns =top.lists  , colour='subject.type')
ggpairs(df.demographic[,-1])
dev.off()


cities.faces = read.csv("./data/city_faces.csv", header = TRUE, stringsAsFactors = FALSE)
cities.entropy = read.csv("./data/city_diurnal_faces_entropies.csv", header = TRUE, stringsAsFactors = FALSE) 
cities.modes.entropy = read.csv("./data/city_diurnal_entropies.csv", header = TRUE, stringsAsFactors = FALSE)
names(cities.entropy)[-1] = paste0(names(cities.entropy)[-1], ".entropy")
df = join_all(list(cities.entropy, cities.faces, cities.modes.entropy, df.demographic))

df.raw = df[,-c(1, which(grepl("tree", names(df))))]

png(file = "./figures/DEMOGRAPHIC_faces_modes_raw_pairs.png", width = 4200, 
    height=4200, units = "px", bg = "white")
#ggpairs(microbiome.df, columns =top.lists  , colour='subject.type')
ggpairs(df.raw[,-1])
dev.off()

df.rates = df.raw
df.rates$num.faces.alt = df.rates$num.faces.alt/df.rates$num.rows
df.rates$num.people.social.faces.alt = df.rates$num.people.social.faces.alt/df.rates$num.rows
df.rates$solo.face.alt = df.rates$solo.face.alt/df$num.rows
df.rates$social.face.alt = df.rates$social.face.alt/df.rates$num.rows
df.rates$face.present.alt = df.rates$face.present.alt/df.rates$num.rows

png(file = "./figures/DEMOGRAPHIC_faces_modes_rates_pairs.png", width = 4200, 
    height=4000, units = "px", bg = "white")
#ggpairs(microbiome.df, columns =top.lists  , colour='subject.type')
ggpairs(df.rates[,-1], diag = list(params = c(size = 7)), upper = list(params = c(size = 10)), 
        lower = list(continuous = "smooth"))
dev.off()
