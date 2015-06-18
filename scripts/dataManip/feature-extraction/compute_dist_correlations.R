## copmute_distance_correlations.R
##
## basic analysis of modes and faces 
##
setwd("~/Documents/twitter-diurnal-analysis/")


library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(reshape)
library(GGally)
library(pairwiseDist)

h.df.1 = read.csv("./data/HSV_hourly_agg_using_mean/top16_hourly_AVG_using_hue_probs.csv", header = TRUE, stringsAsFactors = FALSE)
h.df.2 = read.csv("./data/HSV_hourly_agg_using_mean/remaining_cities_hourly_AVG_using_hue_probs.csv", header = TRUE, stringsAsFactors = FALSE)
h.df = rbind(h.df.1, h.df.2)

NYC = subset(h.df, city == "Santa Ana")
return_PCA = function(df){
  pca.res = prcomp(df)
  return(pca.res$x[,c(1:4)])
}
library(ggplot2)
pca.H = cbind(NYC[,c(1,2)], return_PCA(NYC[,-c(1,2)]))
p = ggplot(pca.H, aes(x = PC1, y = PC2, label = hour)) + geom_text() 

s.df.1 = read.csv("./data/HSV_hourly_agg_using_STDmean/top16_hourly_AVG_using_SAT_probs.csv", header = TRUE, stringsAsFactors = FALSE)
s.df.2 = read.csv("./data/HSV_hourly_agg_using_STDmean/remaining_cities_hourly_AVG_using_SAT_probs.csv", header = TRUE, stringsAsFactors = FALSE)
s.df = rbind(s.df.1, s.df.2)

v.df.1 = read.csv("./data/HSV_hourly_agg_using_STDmean/top16_hourly_AVG_using_VALUE_probs.csv", header = TRUE, stringsAsFactors = FALSE)
v.df.2 = read.csv("./data/HSV_hourly_agg_using_STDmean/remaining_cities_hourly_AVG_using_VALUE_probs.csv", header = TRUE, stringsAsFactors = FALSE)
v.df = rbind(v.df.1, v.df.2)

#####-------------------------------------
##### setup hour distance matrix
#####-------------------------------------
first.row = c(c(0:12), c(11:1))
dist.hours = toeplitz(first.row)
hour.names = unique(h.df$hour)
rownames(dist.hours) = hour.names[order(as.integer(hour.names))]
colnames(dist.hours) = hour.names[order(as.integer(hour.names))]


#####-------------------------------------
##### compute pairwise distances
#####-------------------------------------

return.pairwise.df = function(df, image.type){
  # image.type needs to be one of either H, S or V characters to pick the appropriate columns from df
  df = df[order(df$hour), ]
  pairwise.res = target.source.diff(target.key = as.numeric(rownames(dist.hours)), target.df = dist.hours,
                                    source.key = df$hour, source.df = df[,which(grepl(image.type, names(df)))], 
                                    target.matrix = TRUE)
  return(pairwise.res)
}


h.df %>%
  group_by(city) %>%
  do(return.pairwise.df(., "H")) %>%
  as.data.frame() -> cities.hue.dist.corr

names(cities.hue.dist.corr)[4] = "hue.dist"

s.df %>%
  group_by(city) %>%
  do(return.pairwise.df(., "S")) %>%
  as.data.frame() -> cities.sat.dist.corr

names(cities.sat.dist.corr)[4] = "sat.dist"

v.df %>%
  group_by(city) %>%
  do(return.pairwise.df(., "V")) %>%
  as.data.frame() -> cities.value.dist.corr

names(cities.value.dist.corr)[4] = "value.dist"

rm(h.df, h.df.1, h.df.2, s.df, s.df.1, s.df.2, v.df, v.df.1, v.df.2)
gc()

#res = join_all(list(cities.hue.dist.corr, cities.sat.dist.corr, cities.value.dist.corr), by = c("city", "pairs"))

write.csv(cities.hue.dist.corr, file = "./data/cities_hue_dist_corr_mean.csv", row.names = FALSE, quote = FALSE)

get.spearman = function(df){
  return(data.frame(spearman = cor(df$dist.hours.distances, df$hue.dist)))
}

cities.hue.dist.corr %>%
  group_by(city) %>%
  do(get.spearman(.)) %>%
  as.data.frame() -> cities.hue.social
