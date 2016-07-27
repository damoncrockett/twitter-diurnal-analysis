## diurnal-HSV
##

setwd("~/Documents/twitter-diurnal-analysis/")
library(data.table)
library(plyr)
library(ggplot2)
library(reshape)
library(lubridate)
library(plotly)

#df = read.csv("~/Documents/wifire/processedData/features/all_SD_HSV_mode.csv", header = TRUE, stringsAsFactors = FALSE)
df = read.csv("./data/six_months_SD_HSV_mode.csv", header = TRUE, stringsAsFactors = FALSE)

get.hour = function(x) {
  splitted = strsplit(x, split = "/")[[1]]
  filename = splitted[length(splitted)]
  val = strsplit(filename, split = ".jpg")[[1]][1]
  return(hour(as.POSIXct(as.numeric(val), origin="1970-01-01")))
}

get.day = function(x){
  splitted = strsplit(x, split = "/")[[1]]
  return(splitted[length(splitted)-2])
}

df$hour = sapply(df$image.path, get.hour)
df$day = sapply(df$image.path, get.day)

df$H.binned = cut(df$H.mode, breaks = 12)
df$S.binned = cut(df$S.mode, breaks = 16)
df$V.binned = cut(df$V.mode, breaks = 16)

normalized.Value.counts = function(df){
  bin.counts = as.data.frame(t(as.matrix(table(df$V.binned))))/nrow(df)
  names(bin.counts) = paste0("V.bin.",c(0:(ncol(bin.counts)-1)), ".normalized")
  return(bin.counts)
}

value.hours = ddply(df, .(hour), normalized.Value.counts)
value.hours.m = melt(value.hours, id.vars = c("hour"))

ggplot(value.hours.m, aes(x = hour, y = value)) + geom_point() + facet_wrap(~variable, scales = "free")
head(value.hours)

py <- plotly()
p = ggplot(value.hours, aes(x = hour, y = V.bin.0.normalized)) + geom_point()
response <- py$ggplotly(p, kwargs=list(filename="diurnal-brightness-control", fileopt="overwrite"))

