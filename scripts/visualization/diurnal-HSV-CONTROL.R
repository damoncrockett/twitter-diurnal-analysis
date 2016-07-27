## diurnal-HSV-control
##
setwd("~/Documents/twitter-diurnal-analysis/")
library(data.table)
library(plyr)
library(ggplot2)
library(reshape)

##---------------------------------------------------------------
##
## get social media data
##
##---------------------------------------------------------------
dt = fread("~/Dropbox/TwitterPaper/data/US_HSV_modes_top60_cities_weather_user.csv", header = TRUE)
dt.sd = subset(dt, city == "new york")
dt.sd.HSV = dt.sd[,.(image.path, hour, H.mode, S.mode, V.mode)]
dt.sd.HSV$data.type = "social.media"

##---------------------------------------------------------------
##
## get controlled data
##
##---------------------------------------------------------------
df = read.csv("./data/six_months_SD_HSV_mode.csv", header = TRUE, stringsAsFactors = FALSE)
get.hour = function(x) {
  splitted = strsplit(x, split = "/")[[1]]
  filename = splitted[length(splitted)]
  val = strsplit(filename, split = ".jpg")[[1]][1]
  return(hour(as.POSIXct(as.numeric(val), origin="1970-01-01")))
}

df$hour = sapply(df$image.path, get.hour)
df$data.type = "control"

##---------------------------------------------------------------
##
## merge results
##
##---------------------------------------------------------------
sd.control = rbind(dt.sd.HSV, df)

sd.control$H.binned = cut(sd.control$H.mode, breaks = 12)
sd.control$S.binned = cut(sd.control$S.mode, breaks = 16)
sd.control$V.binned = cut(sd.control$V.mode, breaks = 16)

normalized.Value.counts = function(df){
  bin.counts = as.data.frame(t(as.matrix(table(df$V.binned))))/nrow(df)
  names(bin.counts) = paste0("V.bin.",c(0:(ncol(bin.counts)-1)), ".normalized")
  return(bin.counts)
}

value.hours = ddply(sd.control, .(hour, data.type), normalized.Value.counts)
value.hours.m = melt(value.hours, id.vars = c("hour", "data.type"))

p = ggplot(value.hours.m, aes(x = hour, y = value, colour = data.type)) + geom_point() + facet_wrap(~variable, scales = "free")


normalized.Hue.counts = function(df){
  bin.counts = as.data.frame(t(as.matrix(table(df$H.binned))))/nrow(df)
  names(bin.counts) = paste0("H.bin.",c(0:(ncol(bin.counts)-1)), ".normalized")
  bin.counts$avg.V = median(df$V.mode)
  return(bin.counts)
}
hue.hours = ddply(sd.control, .(hour, data.type), normalized.Hue.counts)
hue.hours.m = melt(hue.hours, id.vars = c("hour", "data.type", "avg.V"))
p = ggplot(subset(hue.hours.m, data.type == "social.media"), aes(x = hour, y = value, colour = avg.V)) + 
  geom_point() + facet_wrap(~variable, scales = "free") + scale_fill_brewer()

p = ggplot(subset(hue.hours.m, data.type == "control"), aes(x = hour, y = value, colour = avg.V)) + 
  geom_point() + facet_wrap(~variable, scales = "free") + scale_fill_brewer()

##---------------------------------------------------------------
##
## do simple avg
##
##---------------------------------------------------------------

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

simple.avg = function(df){
  return(data.frame(avg.H = Mode(df$H.mode), avg.S = Mode(df$S.mode), avg.V = Mode(df$V.mode)))
}

avg.HSV = ddply(sd.control, .(data.type, hour), simple.avg)
avg.HSV.m = melt(avg.HSV, id.vars = c("hour", "data.type"))

p = ggplot(avg.HSV.m, aes(x = hour, y = value, colour = data.type)) + geom_point() + facet_wrap(~variable, scales = "free")


##---------------------------------------------------------------
##
## prep samples
##
##---------------------------------------------------------------

take.samples = function(df){
  index = base::sample(nrow(df), size = 1000, replace = TRUE)
  return(df[index, ])
}

df.sample = ddply(sd.control, .(data.type, hour), take.samples)

write.csv(df.sample[,c('image.path', 'data.type', 'hour', 'H.mode', 'S.mode', 'V.mode')], file = "./data/dom_HSV_sample_SD_control.csv", row.names = FALSE, quote = FALSE)
