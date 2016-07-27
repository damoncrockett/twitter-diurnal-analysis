## diurnal-faces-and-HSV.R
##

setwd("~/Documents/twitter-diurnal-analysis/")
library(data.table)
library(plyr)
library(ggplot2)
library(reshape)

faces.hours = read.csv("./data/face_hours.csv", header = TRUE, stringsAsFactors = FALSE)
faces.hours$city = tolower(faces.hours$city)
#faces.hours[,-which(names(faces.hours) %in% c("hour", "city"))]
faces.hours$normalied.face.present = faces.hours$face.present/faces.hours$num.rows

value.hours = read.csv("./data/value_hours.csv", header = TRUE, stringsAsFactors = FALSE)


dirunal.cities = merge(faces.hours, value.hours, by = c("hour", "city"))

diurnal.basic = dirunal.cities[,c("hour", "city", "V.bin.0.normalized", "normalied.face.present")]

diurnal.m = melt(diurnal.basic, id.vars = c("hour", "city"))

p = ggplot(diurnal.m, aes(x = as.factor(hour), y = value, colour = variable)) + 
  geom_point(size = .8) + facet_wrap(~city, scales = "free", nrow = 6) + 
  theme(strip.text.x = element_text(size = 6),
        strip.background = element_blank(),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())