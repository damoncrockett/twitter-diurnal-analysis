## random_subset_city.R
##
## 10K random sample from major cities...
##
setwd("~/Documents/twitter-diurnal-analysis/")

library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(reshape)
dt = fread("./data/Top_60_faces_alt_and_alt_tree_HSV_modes.csv", header = TRUE)

##------------------------------------------------------------------------------
##
## find top 20 cities
##
##------------------------------------------------------------------------------
as.data.frame(table(dt$city)) -> cities.fr

top.20 = as.character(cities.fr[order(cities.fr$Freq, decreasing = FALSE)[c(1:20)], "Var1"])
top.20.cities = subset(dt, city %in% top.20)


##------------------------------------------------------------------------------
##
## take random 10K sample from each city
##
##------------------------------------------------------------------------------
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

as.data.frame(top.20.cities) %>% 
  group_by(city) %>% 
  do(get.sub.sample(.)) %>% 
  as.data.frame() -> cities.samples



##------------------------------------------------------------------------------
##
## correct day
##
##------------------------------------------------------------------------------
rm(dt)
gc()
dt = cities.samples
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





write_results = function(df){
  path = "./"
  filename = paste0(path, df$city[1], "_10K-sample.csv")
  phony.df = data.frame(file_path = 
                          rep("/home/myazdani/Documents/twitter_projects/image_subsets/black-thumb.png", length(c(min(df$hour):max(df$hour)))), 
                        hour = c(min(df$hour):max(df$hour)), H.mode = -1)
  write.table(rbind(df[,c("file_path", "hour", "H.mode")], phony.df), file = filename, quote = FALSE, sep = ",", col.names= FALSE, row.name = FALSE)
}

dt.shifted.hours %>%
  group_by(city) %>%
  do(write_results(.))
