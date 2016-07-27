## demographic-correlation-analysis.R
##
## basic analysis of modes and faces 
##

setwd("~/Documents/twitter-diurnal-analysis/")
library(data.table)
library(dplyr)
library(ggplot2)
#library(plotly)
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
df.gallup = read.csv("~/Dropbox/TwitterPaper/data/demographic-data/gallup.txt", header = TRUE, stringsAsFactors = FALSE)
names(df.gallup)[1] = "city"


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
df.gallup$city = tolower(df.gallup$city)

##---------------------------------------------------------------------------------
##
## merge twitter and demographic data
##
##---------------------------------------------------------------------------------
#df = join_all(list(cities.entropy, cities.faces, cities.modes.entropy, df.demographic, city.color.corr, city.hue.corr))
df = join_all(list(cities.entropy, cities.faces, cities.modes.entropy, df.demographic, city.color.corr, city.users, df.gallup))

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

df.rel = df
df.rel$pop = NULL
df.rel$num.rows = NULL
#df.rel = df.rel[,c(1:34, 40:46, 35:39, 47:52)]
#df.rel = df.rel[,c(1:34, 40:46, 35:39)]
df.rel$hue.mode = NULL
cor(na.omit(df.rel[,-1]), method = "spearman") -> res
# 
# library(corrplot)
# png(file = "./figures/DEMOGRAPHIC_corr.png", width = 4200, 
#     height=4000, units = "px", bg = "white")
# 
# corrplot(res, tl.cex = 4)
# 
# dev.off()

twitter.features = c("face.present.rate.alt.entropy", "solo.face.rate.alt.entropy", "social.face.rate.alt.entropy", "num.faces.alt",
                     "face.present.alt", "num.people.social.faces.alt", "solo.face.alt", "social.face.alt","bin.0.count", "num.unique.images",
                     "num.unique.face.images.alt", "Hbin.00", "Hbin.01", "Hbin.02", "Hbin.03", "Hbin.04", "Hbin.05","Hbin.06","Hbin.07","Hbin.08",
                     "Hbin.09","Hbin.10","Hbin.11","Hbin.12","hue.corr","num.users")

housing.corrs = sapply(df.rel[,twitter.features], FUN = function(x) cor(x, df.rel$median.2013, use = "complete.obs", method = "pearson"))
income.corrs = sapply(df.rel[,twitter.features], FUN = function(x) cor(x, df.rel$income, use = "complete.obs", method = "pearson"))
bachelors.corrs = sapply(df.rel[,twitter.features], FUN = function(x) cor(x, df.rel$bachelors, use = "complete.obs", method = "pearson"))
unemployed.corrs = sapply(df.rel[,twitter.features], FUN = function(x) cor(x, df.rel$unemployed, use = "complete.obs", method = "pearson"))
disabled.corrs = sapply(df.rel[,twitter.features], FUN = function(x) cor(x, df.rel$disabled, use = "complete.obs", method = "pearson"))
overall.corrs = sapply(df.rel[,twitter.features], FUN = function(x) cor(x, df.rel$overall, use = "complete.obs", method = "pearson"))
purpose.corrs = sapply(df.rel[,twitter.features], FUN = function(x) cor(x, df.rel$purpose, use = "complete.obs", method = "pearson"))
social.corrs = sapply(df.rel[,twitter.features], FUN = function(x) cor(x, df.rel$social, use = "complete.obs", method = "pearson"))
physical.corrs = sapply(df.rel[,twitter.features], FUN = function(x) cor(x, df.rel$physical, use = "complete.obs", method = "pearson"))
financial.corrs = sapply(df.rel[,twitter.features], FUN = function(x) cor(x, df.rel$financial, use = "complete.obs", method = "pearson"))
community.corrs = sapply(df.rel[,twitter.features], FUN = function(x) cor(x, df.rel$community, use = "complete.obs", method = "pearson"))

single.var.corrs = data.frame(housing.pearson = housing.corrs, income.pearson = income.corrs, bachelors.pearson = bachelors.corrs, 
                              unemployed.pearson = unemployed.corrs, disabled.pearson = disabled.corrs, overall.pearson = overall.corrs, 
                              social.pearson = social.corrs, physical.pearson = physical.corrs, financial.cpearson = financial.corrs, 
                              community.pearson = community.corrs)
write.csv(single.var.corrs, file = "./results/single_variable_corr_all_pearson.csv", row.names = TRUE, quote = FALSE)


##---------------------------------------------------------------------------------
##
## mega-feature models ON CENSUS
##
##---------------------------------------------------------------------------------

housing.model = lm(median.2013 ~ ., data = df.rel[, c(twitter.features, "median.2013")])
cat(capture.output(summary(housing.model)), file = "./results/housing_model.txt", sep = "\n")

income.model = lm(income ~ ., data = df.rel[, c(twitter.features, "income")])
cat(capture.output(summary(income.model)), file = "./results/income_model.txt", sep = "\n")

bachelors.model = lm(bachelors ~., data = df.rel[, c(twitter.features, "bachelors")])
cat(capture.output(summary(bachelors.model)), file = "./results/bachelors_model.txt", sep = "\n")

unemployment.model = lm(unemployed ~., data = df.rel[, c(twitter.features, "unemployed")])
cat(capture.output(summary(unemployment.model)), file = "./results/unemployed_model.txt", sep = "\n")

disability.model = lm(disabled ~., data = df.rel[, c(twitter.features, "disabled")])
cat(capture.output(summary(disability.model)), file = "./results/disabled_model.txt", sep = "\n")


##---------------------------------------------------------------------------------
##
## mega-feature models ON GALLUP
##
##---------------------------------------------------------------------------------


df.rel = na.omit(df.rel)
df.rel[,twitter.features] = lapply(df.rel[,twitter.features], rank)
# library(caret)
# twitter.features = twitter.features[-findCorrelation(cor(df.rel[,twitter.features]), cutoff = .9)]

overall.model = lm(overall ~ ., data = df.rel[,c(twitter.features, "overall")])
cat(capture.output(summary(overall.model)), file = "./results/overall_model.txt", sep = "\n")

purpose.model = lm(purpose ~ ., data = df.rel[,c(twitter.features, "purpose")])
cat(capture.output(summary(purpose.model)), file = "./results/purpose_model.txt", sep = "\n")

physical.model = lm(physical ~ ., data = df.rel[,c(twitter.features, "physical")])
cat(capture.output(summary(physical.model)), file = "./results/physical_model.txt", sep = "\n")

social.model = lm(social ~ ., data = df.rel[,c(twitter.features, "social")])
cat(capture.output(summary(social.model)), file = "./results/social_model.txt", sep = "\n")

financial.model = lm(financial ~ ., data = df.rel[,c(twitter.features, "financial")])
cat(capture.output(summary(financial.model)), file = "./results/financial_model.txt", sep = "\n")

community.model = lm(community ~ ., data = df.rel[,c(twitter.features, "community")])
cat(capture.output(summary(community.model)), file = "./results/community_model.txt", sep = "\n")

