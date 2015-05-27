## aggregate_faces.R
##
## find summary statistics for face data

setwd("~/Documents/twitter-diurnal-analysis/")

library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(reshape)
dt = fread("./data/Top_60_faces_alt_and_alt_tree_HSV_modes.csv", header = TRUE)
dt$day = as.Date(dt$postedTime)
dt$week.day = weekdays(dt$day)


normalized.num.faces = function(df){
  return(data.frame(num.rows = nrow(df), 
                    num.faces.alt = sum(df$num_faces_alt), face.present.alt = length(which(df$num_faces_alt > 0)), 
                    solo.face.alt = length(which(df$num_faces_alt == 1)), social.face.alt = length(which(df$num_faces_alt > 1)),
                    num.faces.alt.tree = sum(df$num_faces_alt.tree), face.present.alt.tree = length(which(df$num_faces_alt.tree > 0)), 
                    solo.face.alt.tree = length(which(df$num_faces_alt.tree == 1)), social.face.alt.tree = length(which(df$num_faces_alt.tree > 1))
                    ))
}

as.data.frame(dt) %>% 
  group_by(city, hour) %>% 
  do(normalized.num.faces(.)) %>% 
  as.data.frame() -> diurnal.faces

as.data.frame(dt) %>% 
  group_by(city) %>% 
  do(normalized.num.faces(.)) %>% 
  as.data.frame() -> diurnal.cities

