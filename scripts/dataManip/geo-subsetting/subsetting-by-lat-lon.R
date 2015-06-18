setwd("~/Documents/twitter-diurnal-analysis/")

library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(reshape)
dt = fread("./data/Top_60_faces_alt_and_alt_tree_HSV_modes.csv", header = TRUE)
cities.gps = read.csv("~/Desktop/boundbox_US_top_60.csv", header = TRUE, stringsAsFactors = FALSE)
get.correct.day = function(i){
  x = as.POSIXct(strptime(dt$postedTime[i], format = "%Y-%m-%dT%H:%M:%S"), tz = "GMT")
  tz.str = cities.gps[which(cities.gps$city == dt$city[i]),"time.zone"]
  return(as.IDate(format(x, tz = tz.str)))
}
dt$day = do.call("c", lapply(c(1:nrow(dt)), get.correct.day))
dt$week.day = weekdays(dt$day)

##----------------------------------------------------------------
##
## KEEP ONLY WEEKENDS
##
##----------------------------------------------------------------
week.ends = c("Friday", "Saturday", "Sunday")
dt = dt[week.day %in% week.ends]

##----------------------------------------------------------------
##
## shift hours
##
##----------------------------------------------------------------
shift.hours = function(df){
  if (df$week.day[1] == "Friday"){
    shift.amount = 0
  }
  if (df$week.day[1] == "Saturday"){
    shift.amount = 24
  }
  if (df$week.day[1] == "Sunday"){
    shift.amount = 48
  }
  df$hour = df$hour + shift.amount
  return(df)
}

as.data.frame(dt) %>% 
  group_by(week.day) %>% 
  do(shift.hours(.)) %>% 
  as.data.frame() -> dt.shifted.hours
rm(dt)
gc()

##----------------------------------------------------------------
##
## subset by lat lon
##
##----------------------------------------------------------------

bbox_radius = .01
city.subset = function(df, com.mode){
  city.subset = subset(df, lat > (com.mode$lat - bbox_radius) & lat < (com.mode$lat + bbox_radius) & lon > (com.mode$lon - bbox_radius) & lon < (com.mode$lon + bbox_radius))
  return(city.subset)
}

gaslamp.gps = data.frame(lon = -117.159167, lat = 32.711667)
gaslamp.dt = city.subset(dt.shifted.hours, gaslamp.gps)

northpark.gps = data.frame(lat = 32.740831, lon = -117.129719)
northpark.dt = city.subset(dt.shifted.hours,northpark.gps)
northpark.dt = rbind(northpark.dt[,c("file_path", "hour", "H.mode")], data.frame(file_path = rep("/home/myazdani/Documents/twitter_projects/image_subsets/black-thumb.png", length(c(0:71))), hour = c(0:71), H.mode = -1))
gaslamp.dt = rbind(gaslamp.dt[,c("file_path", "hour", "H.mode")], data.frame(file_path = rep("/home/myazdani/Documents/twitter_projects/image_subsets/black-thumb.png", length(c(0:71))), hour = c(0:71), H.mode = -1))

write.csv(northpark.dt, file = "~/Desktop/north_park.csv", row.names = FALSE, quote = FALSE)
write.csv(gaslamp.dt, file = "~/Desktop/gaslamp.csv", row.names = FALSE, quote = FALSE)
