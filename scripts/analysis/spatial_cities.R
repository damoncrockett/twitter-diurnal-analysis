## hourly_cities.R
##
## hourly pairwise differences using RGB modes

setwd("~/Documents/twitter_data_grant/")
library(data.table)

dt = fread("./processedData/features/color/US_RGB_modes_top_cities.csv", header = TRUE)
cities.gps = read.csv("./meta_data/boundbox_US.csv", header = TRUE, stringsAsFactors = FALSE)
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
# res.H = as.data.frame(dt.clean[, as.list(quantile(Hue, probs = seq(.1, .9, .1))), by = list(city)])
# res.S = as.data.frame(dt.clean[, as.list(quantile(Saturaion, probs = seq(.1, .9, .1))), by = list(city)])
# res.V = as.data.frame(dt.clean[, as.list(quantile(Value, probs = seq(.1, .9, .1))), by = list(city)])

res.H = as.data.frame(dt.clean[, as.list(quantile(Hue, probs = seq(.1, .9, .1))), by = list(hour, city)])
res.S = as.data.frame(dt.clean[, as.list(quantile(Saturaion, probs = seq(.1, .9, .1))), by = list(hour, city)])
res.V = as.data.frame(dt.clean[, as.list(quantile(Value, probs = seq(.1, .9, .1))), by = list(hour, city)])



#####-------------------------------------
##### setup spatial distance matrix
#####-------------------------------------
cities.gps = cities.gps[order(cities.gps$city),]

#dist.cities = as.matrix(dist(cities.gps[,c(4,5)], diag = TRUE, upper = TRUE))
dist.cities = as.matrix(dist(cities.gps[,c(5)], diag = TRUE, upper = TRUE))
rownames(dist.cities) = cities.gps$city
colnames(dist.cities) = cities.gps$city
#####-------------------------------------
##### setup hour distance matrix
#####-------------------------------------
get.dist.df = function(x){
  if (is.matrix(x)){ 
    # if it is a matrix already, then assume that it is a proper distance matrix
    z = x
  } 
  else{
    z = as.matrix(dist(x)) 
  }
  z[lower.tri(z,diag=TRUE)]=NA  #Prepare to drop duplicates and meaningless information
  z=as.data.frame(as.table(z))  #Turn into a 3-column table
  z=na.omit(z)  #Get rid of the junk we flagged above
  z=z[order(-abs(z$Freq)),] 
  
  z$Var1 = as.character(z$Var1)
  z$Var2 = as.character(z$Var2)
  ordered.pair = c()
  for (i in c(1:nrow(z))){
    if (z$Var1[i] > z$Var2[i]){
      ordered.pair = c(ordered.pair, 
                       paste(z$Var1[i], "-",z$Var2[i]))
    }
    else{
      ordered.pair = c(ordered.pair,
                       paste(z$Var2[i], "-",z$Var1[i]))
    }
  }
  z$pairs = ordered.pair
  z$Var1 = NULL
  z$Var2 = NULL
  return(z)
}

dist.cities.df = get.dist.df(dist.cities)
names(dist.cities.df)[1] = "cities.dist"

cities.HSV.list = list()
hours = unique(res.H$hour)
library(plyr)
for (i in c(1:length(hours))){
  res.H.temp = subset(res.H, hours == hours[i])
  res.H.temp$hour = NULL
  res.S.temp = subset(res.S, hours == hours[i])
  res.S.temp$hour = NULL
  res.V.temp = subset(res.V, hours == hours[i])
  res.V.temp$hour = NULL
  
  ## get H distances
  row.names(res.H.temp) = res.H.temp[,1]
  H.dist = get.dist.df(res.H.temp[,-1])
  names(H.dist)[1] = "hue.dist"
  ## get S distances
  row.names(res.S.temp) = res.S.temp[,1]
  S.dist = get.dist.df(res.S.temp[,-1])
  names(S.dist)[1] = "saturation.dist"
  ## get V distancesobjects()
  row.names(res.V.temp) = res.V.temp[,1]
  V.dist = get.dist.df(res.V.temp[,-1])
  names(V.dist)[1] = "value.dist"
  cities.HSV = join_all(list(dist.cities.df, H.dist, S.dist, V.dist))
  cities.HSV$hours = hours[i]
  cities.HSV.list[[i]] = cities.HSV
}

cities.HSV.res = do.call(rbind, cities.HSV.list)


p = ggplot(cities.HSV.res, aes(x = cities.dist, y = hue.dist, label = pairs))  + geom_text(size = 3, alpha = .9) + stat_smooth(method = lm) + xlab("Difference in cities") + ylab("Difference in Hue Distributions") + facet_wrap(~hours)
#ggsave(file = "./figures/Hue_time.png", p)
p = ggplot(cities.HSV.res, aes(x = cities.dist, y = saturation.dist, label = pairs))  + geom_text(size = 3, alpha = .9) + stat_smooth(method = lm) + xlab("Difference in cities") + ylab("Difference in Saturation Distributions")+ facet_wrap(~hours)
#ggsave(file = "./figures/Saturation_time.png", p)
p = ggplot(cities.HSV.res, aes(x = cities.dist, y = value.dist, label = pairs))  + geom_text(size = 3, alpha = .9) + stat_smooth(method = lm) + xlab("Difference in cities") + ylab("Difference in Value Distributions") + facet_wrap(~hours)
#ggsave(file = "./figures/Value_time.png", p)


