## weather_and_faces.R
##
## measure influence of weather on face stats
##

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
#dt$day = as.Date(dt$postedTime)
dt$week.day = weekdays(dt$day)
dt$season = month(dt$day) 

##----------------------------------------------------------------
##
## merge with weather
##
##----------------------------------------------------------------

dt.weather = fread("~/Dropbox/TwitterPaper/data/US_HSV_modes_top60_cities_weather.csv", header = TRUE)

setkey(dt.weather, image.path)
setkey(dt, file_path)
dt.just.weather = dt.weather[,setdiff(names(dt.weather), names(dt)), with = FALSE]
rm(dt.weather)
gc()

res = unique(dt[dt.just.weather])
rm(dt)
rm(dt.just.weather)
gc()

##------------------------------------------------------------------------------------------
## Reduce Hue spectrum resolution
## Go from 180 bins to 12
## More details here: 
## https://github.com/myazdani/OpenCV-Hue/blob/master/scripts/Hue-spectrums.ipynb
##------------------------------------------------------------------------------------------
num.bins = 12
num.steps = 180/num.bins
bin.edges.left = seq(from = 0, to = 180, by = num.steps) #includes bin from left, but not right
bin.edges.left[1] = 1
bin.edges.left = c(0, bin.edges.left)
names(bin.edges.left)[1:10] = paste0("bin.0", c(0:9))
names(bin.edges.left)[11:length(bin.edges.left)] = paste0("bin.",c(11:length(bin.edges.left)-1))

find.hue.bin = function(x){
  return(names(bin.edges.left)[max(which(x >= bin.edges.left))])
}

res$H.mode.binned = sapply(res$H.mode, find.hue.bin)


##------------------------------------------------------------------------------------------
## compute aggregates of faces, hue bin 0, and "retweets"
##------------------------------------------------------------------------------------------

normalized.num.faces = function(df){
  social.df = data.frame(num.rows = nrow(df), 
                         num.faces.alt = sum(df$num_faces_alt), face.present.alt = length(which(df$num_faces_alt > 0)), 
                         num.people.social.faces.alt = sum(df$num_faces_alt[which(df$num_faces_alt > 1)]),
                         solo.face.alt = length(which(df$num_faces_alt == 1)), social.face.alt = length(which(df$num_faces_alt > 1)),
                         num.faces.alt.tree = sum(df$num_faces_alt.tree), face.present.alt.tree = length(which(df$num_faces_alt.tree > 0)), 
                         solo.face.alt.tree = length(which(df$num_faces_alt.tree == 1)), social.face.alt.tree = length(which(df$num_faces_alt.tree > 1)),
                         num.people.social.faces.alt.tree = sum(df$num_faces_alt.tree[which(df$num_faces_alt.tree > 1)]),
                         bin.0.count = length(which(df$H.mode.binned == "bin.00")), num.unique.images = length(unique(df$filename)),
                         num.unique.face.images.alt = length(unique(df$filename[which(df$num_faces_alt > 0)])),
                         num.unique.face.images.alt.tree = length(unique(df$filename[which(df$num_faces_alt.tree > 0)])))
  return(cbind(social.df, df[1,c("temp", "humidity", "wind","rain", "sky")]))
}

as.data.frame(res) %>% 
  group_by(city, season, week.day, hour) %>% 
  do(normalized.num.faces(.)) %>% 
  as.data.frame() -> social.df

write.csv(social.df, file = "./data/social_weather_aggs.csv", row.names = FALSE, quote = FALSE)
