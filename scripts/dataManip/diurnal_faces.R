## diurnal_faces.R
##
## hourly pairwise differences using RGB modes

setwd("~/Documents/twitter_projects/")
library(data.table)

dt = fread("./processedData/features/colorFeatures/RGB_modes/US_RGB_modes_top_cities.csv", header = TRUE)

num_faces = fread("./processedData/features/twitter_num_faces.csv", header = TRUE)
get.filename = function(x) {
  splitted = strsplit(x, split = "/")[[1]]
  return(splitted[length(splitted)])
} 

num_faces$filename  = sapply(num_faces$file_path, get.filename)
df = as.data.frame(dt)
rm(dt)
num_faces = as.data.frame(num_faces)
dt.faces = merge(num_faces, dt, by.x = "file_path", by.y = "image.path")
rm(dt)
dt.faces$filename.x = NULL
dt.faces$filename.y = NULL
cities.gps = read.csv("./meta_data/boundbox_US.csv", header = TRUE, stringsAsFactors = FALSE)
cities.gps$time.zone = "America/Los_Angeles"
cities.gps$time.zone[1] = "America/Chicago"
cities.gps$time.zone[3] = "America/Chicago"
cities.gps$time.zone[4] = "America/Denver"
cities.gps$time.zone[5] = "America/Chicago"
cities.gps$time.zone[6] = "America/New_York"
cities.gps$time.zone[8] = "America/New_York"
cities.gps$time.zone[9] = "America/New_York"
cities.gps$time.zone[10] = "America/Chicago"
cities.gps$time.zone[11] = "America/New_York"
cities.gps$time.zone[16] = "America/Chicago"
cities.gps$time.zone[17] = "America/Chicago"

get.correct.hour = function(i){
  x = as.POSIXct(strptime(dt.faces$postedTime[i], format = "%Y-%m-%dT%H:%M:%S"), tz = "GMT")
  tz.str = cities.gps[which(cities.gps$city == dt.faces$city[i]),"time.zone"]
  return(hour(format(x, tz = tz.str)))
}
dt.faces$hour = sapply(c(1:nrow(dt.faces)), get.correct.hour)
library(plyr)
normalized.num.faces = function(df){
  return(data.frame(hour = df$hour[1], city = df$city[1],num.rows = nrow(df), num.faces = sum(df$num_faces), 
                    face.present = length(which(df$num_faces > 0)), solo.face = length(which(df$num_faces == 1)),
                    social.face = length(which(df$num_faces > 1))))
}
dt.clean = na.omit(dt.faces)
faces.hours = ddply(dt.clean, .(hour, city), normalized.num.faces)
