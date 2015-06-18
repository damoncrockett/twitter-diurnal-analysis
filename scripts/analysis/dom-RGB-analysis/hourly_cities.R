## hourly_cities.R
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
res.H = as.data.frame(dt.clean[, as.list(quantile(Hue, probs = seq(.1, .9, .1))), by = list(hour, city)])
res.S = as.data.frame(dt.clean[, as.list(quantile(Saturaion, probs = seq(.1, .9, .1))), by = list(hour, city)])
res.V = as.data.frame(dt.clean[, as.list(quantile(Value, probs = seq(.1, .9, .1))), by = list(hour, city)])

#####-------------------------------------
##### PCA visualization of aggregates
#####-------------------------------------

return_PCA = function(df){
  pca.res = princomp(df)
  return(pca.res$scores[,c(1:4)])
}
library(ggplot2)
pca.H = cbind(res.H[,c(1,2)], return_PCA(res.H[,-c(1,2)]))
p = ggplot(pca.H, aes(x = Comp.1, y = Comp.2, label = hour)) + geom_text(size = 2) + ggtitle("PCA of Hue Distributions") + facet_wrap(~city)
#ggsave(file = "./figures/Hue_PCA.png", p)
pca.S = cbind(res.S[,c(1,2)], return_PCA(res.S[,-c(1,2)]))
p = ggplot(pca.S, aes(x = Comp.1, y = Comp.2, label = hour)) + geom_text(size = 2) + ggtitle("PCA of Saturation Distributions") + facet_wrap(~city)
#ggsave(file = "./figures/Saturation_PCA.png", p)
pca.V = cbind(res.V[,c(1,2)], return_PCA(res.V[,-c(1,2)]))
p = ggplot(pca.V, aes(x = Comp.1, y = Comp.2, label = hour)) + geom_text(size = 2) + ggtitle("PCA of Value Distributions") + facet_wrap(~city)
#ggsave(file = "./figures/Value_PCA.png", p)


#####-------------------------------------
##### setup hour distance matrix
#####-------------------------------------
first.row = c(c(0:12), c(11:1))
dist.hours = toeplitz(first.row)
hour.names = unique(res.S$hour)
rownames(dist.hours) = hour.names[order(as.integer(hour.names))]
colnames(dist.hours) = hour.names[order(as.integer(hour.names))]

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

dist.hours.df = get.dist.df(dist.hours)
names(dist.hours.df)[1] = "hours.dist"

hours.HSV.list = list()
cities = unique(res.H$city)
library(plyr)
for (i in c(1:length(cities))){
  res.H.temp = subset(res.H, city == cities[i])
  res.H.temp$city = NULL
  res.S.temp = subset(res.S, city == cities[i])
  res.S.temp$city = NULL
  res.V.temp = subset(res.V, city == cities[i])
  res.V.temp$city = NULL
  
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
  hours.HSV = join_all(list(dist.hours.df, H.dist, S.dist, V.dist))
  hours.HSV$city = cities[i]
  hours.HSV.list[[i]] = hours.HSV
}

hours.HSV.res = do.call(rbind, hours.HSV.list)

p = ggplot(hours.HSV.res, aes(x = hours.dist, y = hue.dist, colour = city))  + geom_point(size = 3, alpha = .9) + stat_smooth(method = lm) + xlab("Difference in hours") + ylab("Difference in Hue Distributions")
#ggsave(file = "./figures/Hue_time.png", p)
p = ggplot(hours.HSV.res, aes(x = hours.dist, y = saturation.dist, colour = city))  + geom_point(size = 3, alpha = .9) + stat_smooth(method = lm) + xlab("Difference in hours") + ylab("Difference in Saturation Distributions")
#ggsave(file = "./figures/Saturation_time.png", p)
p = ggplot(hours.HSV.res, aes(x = hours.dist, y = value.dist, colour = city))  + geom_point(size = 3, alpha = .9) + stat_smooth(method = lm) + xlab("Difference in hours") + ylab("Difference in Value Distributions")
#ggsave(file = "./figures/Value_time.png", p)

find.corr.hue = function(df){
  return(data.frame(hue.corr = cor(df$hue.dist, df$hours.dist),sat.corr =  cor(df$saturation.dist, df$hours.dist), val.corr = cor(df$value.dist, df$hours.dist)))
}

city.corr.hue = ddply(hours.HSV.res, .(city), find.corr.hue)
print(city.corr.hue)
