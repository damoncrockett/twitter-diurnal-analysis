## twitter_demographic_analysis.R
##
## basic analysis of modes and faces 
##

setwd("~/Documents/twitter-diurnal-analysis/")
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(reshape)
library(plyr)
library(GGally)

##---------------------------------------------------------------------------------
##
## load demographic data
##
##---------------------------------------------------------------------------------
load("./data/housing_and_ACS.Rda")

f$RegionName = as.character(f$RegionName)
f$RegionName[which(f$RegionName == "Saint Louis")] = "St. Louis"

df.demographic = f[,c("RegionName", "bachelors", "disabled", "income", "pop", "unemployed", "median.2013")]
names(df.demographic)[1] = "city"

##---------------------------------------------------------------------------------
##
## load gallup data
##
##---------------------------------------------------------------------------------
# gallup = read.csv("~/Dropbox/TwitterPaper/data/demographic-data/gallup.txt", header = TRUE, stringsAsFactors = FALSE)
# df = merge(gallup,f)

##---------------------------------------------------------------------------------
##
## load twitter data
##
##---------------------------------------------------------------------------------
cities.faces = read.csv("./data/features/face-features/city_faces.csv", header = TRUE, stringsAsFactors = FALSE)
cities.entropy = read.csv("./data/features/face-features/city_diurnal_faces_entropies.csv", header = TRUE, stringsAsFactors = FALSE) 
cities.modes.entropy = read.csv("./data/features/color-features/city_diurnal_entropies.csv", header = TRUE, stringsAsFactors = FALSE)
names(cities.entropy)[-1] = paste0(names(cities.entropy)[-1], ".entropy")

city.color.corr = read.csv("./data/features/color-features/city_correlation_HUE_probs.csv", header = TRUE, stringsAsFactors = FALSE)
city.hue.corr = read.csv("./data/features/cities_hue_dist_corr_mean.csv", header = TRUE, stringsAsFactors = FALSE)

city.users = read.csv("./data/features/user-features/user_agg_stats.csv", header = TRUE, stringsAsFactors = FALSE)
names(city.users)[1] = "city"

##---------------------------------------------------------------------------------
##
## make all city names lower case
##
##---------------------------------------------------------------------------------
cities.entropy$city = tolower(cities.entropy$city)
cities.faces$city = tolower(cities.faces$city)
cities.modes.entropy$city = tolower(cities.modes.entropy$city)
df.demographic$city = tolower(df.demographic$city)
city.color.corr$city = tolower(city.color.corr$city)
city.users$city = tolower(city.users$city)

##---------------------------------------------------------------------------------
##
## merge twitter and demographic data
##
##---------------------------------------------------------------------------------
#df = join_all(list(cities.entropy, cities.faces, cities.modes.entropy, df.demographic, city.color.corr, city.hue.corr))
df = join_all(list(cities.entropy, cities.faces, cities.modes.entropy, df.demographic, city.color.corr, city.users))

##---------------------------------------------------------------------------------
##
## prepare twitter predictors
##
##---------------------------------------------------------------------------------
tweet.predictors = names(df)[-which(names(df) %in% c("city", "bachelors", "disabled", "income", "pop", "unemployed", "median.2013", "num.rows", "hue.mode"))]
tweet.raw.counts = c("num.faces.alt", "face.present.alt","num.people.social.faces.alt","solo.face.alt","social.face.alt", "bin.0.count", "num.unique.images" ,"num.unique.face.images.alt",
                     "num.faces.alt.tree", "face.present.alt.tree","num.people.social.faces.alt.tree","solo.face.alt.tree","social.face.alt.tree", "num.unique.face.images.alt.tree", 
                     "num.users", "num.imgs.q95.", "num.imgs.q100.")
df[,tweet.raw.counts] = df[,tweet.raw.counts]/df$num.rows


num.predictors = 4
tweet.predictor.combs = combn(tweet.predictors, num.predictors, simplify = FALSE)


##---------------------------------------------------------------------------------
##
## helper functions
##
##---------------------------------------------------------------------------------

display.helper = function(x){
  print("top 3 R-squared using twitter predictors")
  print(head(x[order(x, decreasing = TRUE)], 3))
  print(head(tweet.predictor.combs[order(x, decreasing = TRUE)], 3))
}


write.results = function(census.model, twitter.model){
  out = capture.output(print("*************************************************************************"))
  cat(out,file="results/out.txt",sep="\n",append=TRUE)
  out<-capture.output(summary(census.model))
  cat(out,file="results/out.txt",sep="\n",append=TRUE)
  out<-capture.output(display.helper(twitter.model))
  cat(out,file="results/out.txt",sep="\n",append=TRUE)
  
}

##---------------------------------------------------------------------------------
##
## linear modelling of housing prices
##
##---------------------------------------------------------------------------------
housing.census = lm(median.2013 ~ bachelors+disabled+income+unemployed, data = df)
median.2013.R.squared = sapply(tweet.predictor.combs, FUN = function(x) summary(lm(median.2013 ~ ., data = df[,c("median.2013", x)]))$r.squared)

write.results(housing.census, median.2013.R.squared)

##---------------------------------------------------------------------------------
##
## linear modelling of incomes
##
##---------------------------------------------------------------------------------
income.census = lm(income ~ bachelors+disabled +unemployed + median.2013, data = df)
income.R.squared = sapply(tweet.predictor.combs, FUN = function(x) summary(lm(income ~ ., data = df[,c("income", x)]))$r.squared)

write.results(income.census, income.R.squared)

##---------------------------------------------------------------------------------
##
## linear modelling of education
##
##---------------------------------------------------------------------------------
bachelors.census = lm(bachelors ~ income+disabled +unemployed + median.2013, data = df)
bachelors.R.squared = sapply(tweet.predictor.combs, FUN = function(x) summary(lm(bachelors ~ ., data = df[,c("bachelors", x)]))$r.squared)

write.results(bachelors.census, bachelors.R.squared)

##---------------------------------------------------------------------------------
##
## linear modelling of unemployment
##
##---------------------------------------------------------------------------------
unemployed.census = lm(unemployed ~ income+disabled + bachelors + median.2013, data = df)
unemployed.R.squared = sapply(tweet.predictor.combs, FUN = function(x) summary(lm(unemployed ~ ., data = df[,c("unemployed", x)]))$r.squared)

write.results(unemployed.census, unemployed.R.squared)

##---------------------------------------------------------------------------------
##
## linear modelling of disability
##
##---------------------------------------------------------------------------------
disabled.census = lm(disabled ~ income+ unemployed+ bachelors + median.2013, data = df)
disabled.R.squared = sapply(tweet.predictor.combs, FUN = function(x) summary(lm(disabled ~ ., data = df[,c("disabled", x)]))$r.squared)

write.results(disabled.census, disabled.R.squared)

