## simple_summary_stats.R
##
## hourly pairwise differences using RGB modes

setwd("~/Documents/twitter_data_grant/")
library(data.table)

dt.1 = fread("./processedData/features/color/US_RGB_modes_top_cities.csv", header = TRUE)
dt.2 = fread("./processedData/features/color/US_RGB_modes_remaining_cities.csv", header = TRUE)
dt = rbind(dt.1, dt.2)
rm(dt.1)
rm(dt.2)
#--
## data setup
#--
dt$hour = sapply(dt$postedTime, FUN = function(x) substr(strsplit(x, split = "T")[[1]][2], 1,2))
M = apply(dt[,c("R.mode", "G.mode", "B.mode"), with = FALSE], 1, max)
m = apply(dt[,c("R.mode", "G.mode", "B.mode"), with = FALSE], 1, min)
C = M - m
alpha = .5*(2*dt$R.mode - dt$G.mode - dt$B.mode)
beta = (sqrt(3)/2)*(dt$G.mode - dt$B.mode)

dt$Hue = atan2(beta, alpha)
dt$Chroma = sqrt(alpha**2 + beta**2)
dt$Value = M
dt$Saturaion = dt$Chroma/dt$Value

dt.clean = na.omit(dt)
rm(dt)
#res.H = as.data.frame(dt.clean[, as.list(quantile(Hue, probs = seq(.1, .9, .1))), by = list(hour, city)])
#res.S = as.data.frame(dt.clean[, as.list(quantile(Saturaion, probs = seq(.1, .9, .1))), by = list(hour, city)])
#res.V = as.data.frame(dt.clean[, as.list(quantile(Value, probs = seq(.1, .9, .1))), by = list(hour, city)])
#names(res.H)[-c(1,2)] = c("ten", "twenty", "thirty", "forty", "med", "sixty", "seventy", "eighty", "ninety")
#names(res.S)[-c(1,2)] = c("ten", "twenty", "thirty", "forty", "med", "sixty", "seventy", "eighty", "ninety")
#names(res.V)[-c(1,2)] = c("ten", "twenty", "thirty", "forty", "med", "sixty", "seventy", "eighty", "ninety")

res.H = as.data.frame(dt.clean[, as.list(quantile(Hue, probs = seq(.25, .75, .25))), by = list(hour, city)])
res.S = as.data.frame(dt.clean[, as.list(quantile(Saturaion, probs = seq(.25, .75, .25))), by = list(hour, city)])
res.V = as.data.frame(dt.clean[, as.list(quantile(Value, probs = seq(.25, .75, .25))), by = list(hour, city)])
names(res.H)[-c(1,2)] = c("twenty-five", "fifty", "seventy-five")
names(res.S)[-c(1,2)] = c("twenty-five", "fifty", "seventy-five")
names(res.V)[-c(1,2)] = c("twenty-five", "fifty", "seventy-five")
library(reshape)
res.H.m = melt(res.H, id.vars= c("hour", "city"))
res.S.m = melt(res.S, id.vars= c("hour", "city"))
res.V.m = melt(res.V, id.vars= c("hour", "city"))

ggplot(res.H.m, aes(x = hour, y = value, colour = city)) + geom_point() + facet_wrap( ~variable, nrow = 3 , scales = "free_y") + ylab("Hue") -> p.H
ggplot(res.S.m, aes(x = hour, y = value, colour = city)) + geom_point() + facet_wrap( ~variable, nrow = 3 , scales = "free_y") + ylab("Saturation") -> p.S
ggplot(res.V.m, aes(x = hour, y = value, colour = city)) + geom_point() + facet_wrap( ~variable, nrow = 3 , scales = "free_y") + ylab("Value") -> p.V

#-------------------------------------------------------------------------------------------
## REMOVE WHITES
#-------------------------------------------------------------------------------------------

dt.no.white = subset(dt.clean, Value != 255)
res.H = as.data.frame(dt.no.white[, as.list(quantile(Hue, probs = seq(.25, .75, .25))), by = list(hour, city)])
res.S = as.data.frame(dt.no.white[, as.list(quantile(Saturaion, probs = seq(.25, .75, .25))), by = list(hour, city)])
res.V = as.data.frame(dt.no.white[, as.list(quantile(Value, probs = seq(.25, .75, .25))), by = list(hour, city)])
names(res.H)[-c(1,2)] = c("twenty-five", "fifty", "seventy-five")
names(res.S)[-c(1,2)] = c("twenty-five", "fifty", "seventy-five")
names(res.V)[-c(1,2)] = c("twenty-five", "fifty", "seventy-five")
res.H.m = melt(res.H, id.vars= c("hour", "city"))
res.S.m = melt(res.S, id.vars= c("hour", "city"))
res.V.m = melt(res.V, id.vars= c("hour", "city"))

ggplot(res.H.m, aes(x = hour, y = value, colour = city)) + geom_point() + facet_wrap( ~variable, nrow = 3 , scales = "free_y") + ylab("Hue") + ggtitle("No whites")-> p.H.no.white
ggplot(res.S.m, aes(x = hour, y = value, colour = city)) + geom_point() + facet_wrap( ~variable, nrow = 3 , scales = "free_y") + ylab("Saturation") + ggtitle("No whites") -> p.S.no.white
ggplot(res.V.m, aes(x = hour, y = value, colour = city)) + geom_point() + facet_wrap( ~variable, nrow = 3 , scales = "free_y") + ylab("Value")  + ggtitle("No whites")-> p.V.no.white

library(plotly)
py <- plotly()
out <- py$ggplotly(p.H, kwargs=list(filename="hue-quantiles-per-hour", fileopt="overwrite"))
out <- py$ggplotly(p.S, kwargs=list(filename="sat-quantiles-per-hour", fileopt="overwrite"))
out <- py$ggplotly(p.V, kwargs=list(filename="value-quantiles-per-hour", fileopt="overwrite"))
out <- py$ggplotly(p.H.no.white, kwargs=list(filename="hue-quantiles-per-hour-no-white", fileopt="overwrite"))
out <- py$ggplotly(p.S.no.white, kwargs=list(filename="sat-quantiles-per-hour-no-white", fileopt="overwrite"))
out <- py$ggplotly(p.V.no.white, kwargs=list(filename="value-quantiles-per-hour-no-white", fileopt="overwrite"))


#-------------------------------------------------------------------------------------------
## Only aggreate for each city
#-------------------------------------------------------------------------------------------

res.H.no.white = as.data.frame(dt.no.white[, as.list(quantile(Hue, probs = seq(.25, .75, .25))), by = list(city)])
res.S.no.white = as.data.frame(dt.no.white[, as.list(quantile(Saturaion, probs = seq(.25, .75, .25))), by = list(city)])
res.V.no.white = as.data.frame(dt.no.white[, as.list(quantile(Value, probs = seq(.25, .75, .25))), by = list(city)])
names(res.H.no.white)[-1] = c("Hue-no-white-twenty-five", "Hue-no-white-fifty", "Hue-no-white-seventy-five")
names(res.S.no.white)[-1] = c("Sat-no-white-twenty-five", "Sat-no-white-fifty", "Sat-no-white-seventy-five")
names(res.V.no.white)[-1] = c("Value-no-white-twenty-five", "Value-no-white-fifty", "Value-no-white-seventy-five")

res.H = as.data.frame(dt.clean[, as.list(quantile(Hue, probs = seq(.25, .75, .25))), by = list(city)])
res.S = as.data.frame(dt.clean[, as.list(quantile(Saturaion, probs = seq(.25, .75, .25))), by = list(city)])
res.V = as.data.frame(dt.clean[, as.list(quantile(Value, probs = seq(.25, .75, .25))), by = list(city)])
names(res.H)[-1] = c("Hue-twenty-five", "Hue-fifty", "Hue-seventy-five")
names(res.S)[-1] = c("Sat-twenty-five", "Sat-fifty", "Sat-seventy-five")
names(res.V)[-1] = c("Value-twenty-five", "Value-fifty", "Value-seventy-five")


join_all(list(res.H, res.S, res.V, res.H.no.white, res.S.no.white, res.V.no.white)) -> HSV.stats.cities
