## user-stats-computation.R
##
## compute user stat aggregates
##
##

setwd("~/Documents/twitter-diurnal-analysis/")

library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(reshape)
dt = fread("~/Dropbox/TwitterPaper/data/US_HSV_modes_top60_cities_weather_user.csv", header = TRUE)

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

dt$H.mode.binned = sapply(dt$H.mode, find.hue.bin)


##------------------------------------------------------------------------------------------
## 
## Aggregate user statistics
##
##------------------------------------------------------------------------------------------
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

user.stats = function(df){
  user.df = data.frame(num.images = nrow(df), gps.sd = sd(df$lat) + sd(df$lon), dom.city = Mode(df$city), num.cities = length(unique(df$city)),
                       typical.hue = Mode(df$H.mode.binned), avg.hue = mean(df$H.mode), sd.hue = sd(df$H.mode))
  return(user.df)
}
dt$actor = as.character(dt$actor)
as.data.frame(dt) %>% 
  group_by(actor) %>% 
  do(user.stats(.)) %>% 
  as.data.frame() -> user.activies.df

user.activies.df[is.na(user.activies.df)] = 0
write.csv(user.activies.df, file = './data/features/user-features/user_stats.csv', row.names = FALSE, quote = FALSE)


##------------------------------------------------------------------------------------------
## 
## Aggregate user statistics for each city
##
##------------------------------------------------------------------------------------------

city.user.stats = function(df){
  num.img.quantile.df = as.data.frame(t(quantile(df$num.images, probs = c(.95, 1))))
  names(num.img.quantile.df) = paste0("num.imgs.q", names(num.img.quantile.df)) 
  
  num.cities.quantile.df = as.data.frame(t(quantile(df$num.cities, probs = c(.95, 1))))
  names(num.cities.quantile.df) = paste0("num.cities.q", names(num.cities.quantile.df)) 
  
  city.user.df = data.frame(num.users = length(unique(df$actor)), hue.mode = Mode(df$typical.hue))
  return(cbind(city.user.df, num.img.quantile.df, num.cities.quantile.df))
}

user.activies.df %>% 
  group_by(dom.city) %>% 
  do(city.user.stats(.)) %>% 
  as.data.frame() -> city.user.agg

write.csv(city.user.agg, file = "./data/features/user-features/user_agg_stats.csv", row.names = FALSE, quote = FALSE)

