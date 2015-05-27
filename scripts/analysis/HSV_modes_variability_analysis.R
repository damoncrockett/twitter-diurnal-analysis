## HSV_modes_variability_analysis.R
##
## compute the variability within days and across days of HSV modes
## using formulations from Naaman et. al 2012
##

setwd("~/Documents/twitter-diurnal-analysis/")

library(data.table)
library(plyr)
library(ggplot2)
library(plotly)
dt = fread("./data/Top_60_faces_alt_and_alt_tree_HSV_modes.csv", header = TRUE)
dt$day = as.Date(dt$postedTime)

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
## HSV mode hourly distributions for each city
##------------------------------------------------------------------------------------------
unique(dt$H.mode.binned) -> the.bins
hue.dist = function(df){
  freqs = as.data.frame(t(as.matrix(table(df$H.mode.binned)/sum(table(df$H.mode.binned)))))
  sample.bins = names(freqs)
  if (length(setdiff(the.bins, sample.bins) != 0){
    freqs[,setdiff(the.bins, sample.bins)] = 1
  }
  names(freqs) = paste0("H", names(freqs))
  return(freqs)
}

as.data.frame(dt) %>% 
  group_by(city, hour) %>% 
  do(hue.dist(.)) %>% 
  as.data.frame() -> Hue.hourly.modes

Hue.hourly.modes[is.na(Hue.hourly.modes)] = 0

##------------------------------------------------------------------------------------------
## Variability within days
## equation 2 in Naaman 2012
##------------------------------------------------------------------------------------------
diurnal.probability = function(df){
  df[,-c(1,2)] = df[,-c(1,2)]/lapply(df[,-c(1,2)], sum)
  return(df)
}

Hue.hourly.modes %>%
  group_by(city) %>%
  do(diurnal.probability(.)) %>%
  as.data.frame() -> Hue.diurnal.probs

## visualize results
library(reshape)
H.m = melt(Hue.diurnal.probs, id.vars = c("city", "hour"))
library(ggplot2)
ggplot(H.m, aes(x = hour, y = value, colour = city)) + geom_point() + facet_wrap(~variable, scales = "free_y")

## compute entropies for each city
entropy.hours.rows = function(df){  
  entrops = as.data.frame(lapply(as.data.frame(df[,-c(1,2)]), FUN = function(x) -sum(x*log(x))))
  return(entrops)
}

Hue.diurnal.probs %>%
  group_by(city) %>%
  do(entropy.hours.rows(.)) %>%
  as.data.frame() -> city.entropies


##------------------------------------------------------------------------------------------
## Variability across days
##------------------------------------------------------------------------------------------

## compute later

##------------------------------------------------------------------------------------------
## Visualize variabilities 
##------------------------------------------------------------------------------------------



# select subset of cities
top.cities = read.csv("./data/top_60_lower48_cities_and_rankd.csv", header = TRUE, stringsAsFactors = FALSE)
top.5 = head(top.cities$City, 5)
bottom.5 = tail(top.cities$City, 5)


city.entropies.m = melt(subset(city.entropies, city %in% c(top.5, bottom.5)), id.vars = "city")
ggplot(city.entropies.m, aes(x = variable, y = value, colour = city)) + geom_point()

