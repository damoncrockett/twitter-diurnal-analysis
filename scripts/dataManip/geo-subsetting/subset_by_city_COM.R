## subset_by_city_COM.R
##
## find center of mass of gps points using mode and 
##
setwd("~/Documents/twitter-diurnal-analysis/")

library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(reshape)
dt = fread("./data/Top_60_faces_alt_and_alt_tree_HSV_modes.csv", header = TRUE)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

compute.com = function(dt){
  com.mode = data.frame(lon = Mode(round(dt$lon,2)), lat = Mode(round(dt$lat,2)))
  return(com.mode)
}


as.data.frame(dt) %>% 
  group_by(city) %>% 
  do(compute.com(.)) %>% 
  as.data.frame() -> cities.com


##------------------------------------------------------------------------------
##
## extract city subsets based on COM
##
##------------------------------------------------------------------------------
bbox_radius = .02

city.subset = function(df){
  com.mode = data.frame(lon = Mode(round(df$lon,2)), lat = Mode(round(df$lat,2)))
  city.subset = subset(df, lat > (com.mode$lat - bbox_radius) & lat < (com.mode$lat + bbox_radius) & lon > (com.mode$lon - bbox_radius) & lon < (com.mode$lon + bbox_radius))
  return(city.subset)
}

as.data.frame(dt) %>% 
  group_by(city) %>% 
  do(city.subset(.)) %>% 
  as.data.frame() -> cities.com.subsets

as.data.frame(table(cities.com.subsets$city)) -> cities.fr

top.20 = as.character(cities.fr[order(cities.fr$Freq, decreasing = TRUE)[c(1:20)], "Var1"])
top.20.cities = subset(cities.com.subsets, city %in% top.20)

##------------------------------------------------------------------------------
##
## correct day
##
##------------------------------------------------------------------------------
rm(dt)
gc()
dt = top.20.cities 
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
## shift hours
##
##----------------------------------------------------------------
shift.hours = function(df){
  if (df$week.day[1] == "Sunday"){
    shift.amount = 0
  }
  if (df$week.day[1] == "Monday"){
    shift.amount = 1*24
  }
  if (df$week.day[1] == "Tuesday"){
    shift.amount = 2*24
  }
  if (df$week.day[1] == "Wednesday"){
    shift.amount = 3*24
  }
  if (df$week.day[1] == "Thursday"){
    shift.amount = 4*24
  }
  if (df$week.day[1] == "Friday"){
    shift.amount = 5*24
  }
  if (df$week.day[1] == "Saturday"){
    shift.amount = 6*24
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


get.sub.sample = function(df){
  if (nrow(df) < 10000){
    # sample with replacement
    df.sub = df[sample(c(1:nrow(df)), size = 10000, replace = TRUE),] 
  }
  else{
    df.sub = df[sample(c(1:nrow(df)), size = 10000, replace = FALSE),] 
  }
  return(df.sub)
}

dt.shifted.hours %>%
  group_by(city) %>%
  do(get.sub.sample(.)) %>%
  as.data.frame() -> dt.subsample

write_results = function(df){
  path = "./"
  filename = paste0(path, df$city[1], "_com10K.csv")
  phony.df = data.frame(file_path = 
                          rep("/home/myazdani/Documents/twitter_projects/image_subsets/black-thumb.png", length(unique(df$hour))), 
                        hour = unique(df$hour), H.mode = -1)
  write.table(rbind(df[,c("file_path", "hour", "H.mode")], phony.df), file = filename, quote = FALSE, sep = ",", col.names= FALSE, row.name = FALSE)
}

dt.subsample %>%
  group_by(city) %>%
  do(write_results(.))
