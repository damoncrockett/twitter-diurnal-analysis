## HSV_modes_faces_analysis.R
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
cities.faces = read.csv("./data/city_faces.csv", header = TRUE, stringsAsFactors = FALSE)
cities.entropy = read.csv("./data/city_diurnal_faces_entropies.csv", header = TRUE, stringsAsFactors = FALSE) 

cities.modes.entropy = read.csv("./data/city_diurnal_entropies.csv", header = TRUE, stringsAsFactors = FALSE)

names(cities.entropy)[-1] = paste0(names(cities.entropy)[-1], ".entropy")

df = join_all(list(cities.entropy, cities.faces, cities.modes.entropy))

library(GGally)

df.raw = df[,-c(1, which(grepl("tree", names(df))))]


png(file = "./figures/faces_modes_raw_pairs.png", width = 4000, 
     height=4000, units = "px", bg = "white")
#ggpairs(microbiome.df, columns =top.lists  , colour='subject.type')
ggpairs(df.raw[,-1])
dev.off()

df.rates = df.raw
df.rates$num.faces.alt = df.rates$num.faces.alt/df.rates$num.rows
df.rates$num.people.social.faces.alt = df.rates$num.people.social.faces.alt/df.rates$num.rows
df.rates$solo.face.alt = df.rates$solo.face.alt/df$num.rows
df.rates$social.face.alt = df.rates$social.face.alt/df.rates$num.rows
df.rates$face.present.alt = df.rates$face.present.alt/df.rates$num.rows

png(file = "./figures/faces_modes_rates_pairs.png", width = 4000, 
    height=4000, units = "px", bg = "white")
#ggpairs(microbiome.df, columns =top.lists  , colour='subject.type')
ggpairs(df.rates[,-1])
dev.off()
