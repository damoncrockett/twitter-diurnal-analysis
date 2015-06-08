## city_COM.R
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
cities.gps = read.csv("~/Desktop/boundbox_US_top_60.csv", header = TRUE, stringsAsFactors = FALSE)
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

##------------------------------------------------------------------------------
##
## visualize COM for mode and median
##
##------------------------------------------------------------------------------
compute.com = function(dt){
  com.mode = data.frame(lon = Mode(round(dt$lon,2)), lat = Mode(round(dt$lat,2)))
  com.median = data.frame(lon = median(dt$lon), lat = median(dt$lat))
  #p = ggplot(dt, aes(x = lon, y = lat)) + geom_point(alpha = .05, size = 1) + geom_point(data = com.mode, colour = "red", shape = "*", size = 10) + geom_point(data = com.median, colour = "green", shape = "*", size = 10)
  p = ggplot(dt, aes(x = lon, y = lat)) + geom_point() + geom_point(data = com.mode, colour = "red", shape = "*", size = 10) + geom_point(data = com.median, colour = "green", shape = "*", size = 10)
  png(file = paste0("./figures/", dt$city[1], ".png"), bg = "transparent", width = 600, height = 600)
  print(p)
  dev.off()
  com.mode$com.type = "mode"
  com.median$com.type = "median"
  return(rbind(com.mode, com.median))
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
bbox_radius = .08

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

write.csv(top.20.cities, file = "./data/top_20_cities_COM.csv", row.names = FALSE, quote = FALSE)

social.cities = subset(top.20.cities, num_faces_alt.tree > 0 )

write.csv(social.cities, file = "./data/top_20_social_cities_COM.csv", row.names = FALSE, quote = FALSE)

social.df = social.cities[,c("filename", "city", "hour")]
#social.df$filename = sapply(social.df$filename, FUN = function(x) paste0("/Users/jaja/Documents/twitter_data_grant/images/top20social/", x))

write.csv(social.df, file = "./data/top_20_social_hour.csv", row.names = FALSE, quote = FALSE)
