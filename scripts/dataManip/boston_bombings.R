## boston_bombings.R
##
## find subset of data corresponding to boston bombing.  
##
setwd("~/Documents/twitter-diurnal-analysis/")

library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(reshape)
dt = fread("./data/Top_60_faces_alt_and_alt_tree_HSV_modes.csv", header = TRUE)
cities.gps = read.csv("~/Desktop/boundbox_US_top_60.csv", header = TRUE, stringsAsFactors = FALSE)
dt = subset(dt, city == "Boston")
get.correct.day = function(i){
  x = as.POSIXct(strptime(dt$postedTime[i], format = "%Y-%m-%dT%H:%M:%S"), tz = "GMT")
  tz.str = cities.gps[which(cities.gps$city == dt$city[i]),"time.zone"]
  return(as.IDate(format(x, tz = tz.str)))
}
dt$day = do.call("c", lapply(c(1:nrow(dt)), get.correct.day))

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

com.mode = data.frame(lon = Mode(round(dt$lon,2)), lat = Mode(round(dt$lat,2)))

boston.bombing.17 = subset(dt, day == "2013-04-17")
