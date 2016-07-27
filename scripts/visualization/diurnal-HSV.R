## diurnal-HSV
##

setwd("~/Documents/twitter-diurnal-analysis/")
library(data.table)
library(plyr)
library(ggplot2)
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
#bin.edges.left[1] = 
#bin.edges.left = c(0, bin.edges.left)
names(bin.edges.left)[1:10] = paste0("bin.0", c(0:9))
names(bin.edges.left)[11:length(bin.edges.left)] = paste0("bin.",c(11:length(bin.edges.left)-1))

find.hue.bin = function(x){
  return(names(bin.edges.left)[max(which(x >= bin.edges.left))])
}

dt$H.mode.binned = sapply(dt$H.mode, find.hue.bin)

##------------------------------------------------------------------------------------------
##
## compute Hue bin stats for each hour
##
##------------------------------------------------------------------------------------------
normalized.bin.counts = function(df){
  bin.counts = as.data.frame(t(as.matrix(table(df$H.mode.binned))))/nrow(df)
  names(bin.counts) = paste0(names(bin.counts), ".normalized")
  return(bin.counts)
}

hue.hours = ddply(dt, .(hour, city), normalized.bin.counts)
hue.hours.m = melt(hue.hours, id.vars = c("hour", "city"))

ggplot(hue.hours.m, aes(x = hour, y = value, colour = city)) + geom_point() + facet_wrap(~variable, scales = "free")

##------------------------------------------------------------------------------------------
##
## compute Value bin stats for each hour
##
##------------------------------------------------------------------------------------------
normalized.Value.counts = function(df){
  bin.counts = as.data.frame(t(as.matrix(table(df$V.binned))))/nrow(df)
  names(bin.counts) = paste0("V.bin.",c(0:(ncol(bin.counts)-1)), ".normalized")
  return(bin.counts)
}

value.hours = ddply(dt, .(hour, city), normalized.Value.counts)
value.hours.m = melt(value.hours, id.vars = c("hour", "city"))

ggplot(value.hours.m, aes(x = hour, y = value, colour = city)) + geom_point() + facet_wrap(~variable, scales = "free")

p = ggplot(value.hours, aes(x = as.factor(hour), y = V.bin.0.normalized)) + 
  geom_point(size = .8) + facet_wrap(~city, scales = "free", nrow = 6) + 
  theme(strip.text.x = element_text(size = 6),
        strip.background = element_blank(),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

write.csv(value.hours, file = "./data/value_hours.csv", row.names = FALSE, quote = FALSE)
