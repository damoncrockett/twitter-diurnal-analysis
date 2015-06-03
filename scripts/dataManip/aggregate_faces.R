## aggregate_faces.R
##
## find summary statistics for face data

setwd("~/Documents/twitter-diurnal-analysis/")

library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(reshape)
dt = fread("./data/Top_60_faces_alt_and_alt_tree_HSV_modes.csv", header = TRUE)
dt$day = as.Date(dt$postedTime)
dt$week.day = weekdays(dt$day)


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
## compute aggregates of faces, hue bin 0, and "retweets"
##------------------------------------------------------------------------------------------

normalized.num.faces = function(df){
  return(data.frame(num.rows = nrow(df), 
                    num.faces.alt = sum(df$num_faces_alt), face.present.alt = length(which(df$num_faces_alt > 0)), 
                    num.people.social.faces.alt = sum(df$num_faces_alt[which(df$num_faces_alt > 1)]),
                    solo.face.alt = length(which(df$num_faces_alt == 1)), social.face.alt = length(which(df$num_faces_alt > 1)),
                    num.faces.alt.tree = sum(df$num_faces_alt.tree), face.present.alt.tree = length(which(df$num_faces_alt.tree > 0)), 
                    solo.face.alt.tree = length(which(df$num_faces_alt.tree == 1)), social.face.alt.tree = length(which(df$num_faces_alt.tree > 1)),
                    num.people.social.faces.alt.tree = sum(df$num_faces_alt.tree[which(df$num_faces_alt.tree > 1)]),
                    bin.0.count = length(which(df$H.mode.binned == "bin.00")), num.unique.images = length(unique(df$filename)),
                    num.unique.face.images.alt = length(unique(df$filename[which(df$num_faces_alt > 0)])),
                    num.unique.face.images.alt.tree = length(unique(df$filename[which(df$num_faces_alt.tree > 0)]))
                    ))
}

as.data.frame(dt) %>% 
  group_by(city, hour) %>% 
  do(normalized.num.faces(.)) %>% 
  as.data.frame() -> diurnal.faces

as.data.frame(dt) %>% 
  group_by(city) %>% 
  do(normalized.num.faces(.)) %>% 
  as.data.frame() -> cities.faces

##------------------------------------------------------------------------------------------
## Variability within days
## equation 2 in Naaman 2012
##------------------------------------------------------------------------------------------
face.rates = c("face.present.rate.alt" , "face.present.rate.alt.tree", "solo.face.rate.alt",
               "solo.face.rate.alt.tree", "social.face.rate.alt", "social.face.rate.alt.tree")
diurnal.probability = function(df){
  df$face.present.rate.alt = (df$face.present.alt+1)/df$num.rows
  df$face.present.rate.alt.tree = (df$face.present.alt.tree+1)/df$num.rows
  df$solo.face.rate.alt = (df$solo.face.alt+1)/df$num.rows
  df$solo.face.rate.alt.tree = (df$solo.face.alt.tree+1)/df$num.rows
  df$social.face.rate.alt = (df$social.face.alt+1)/df$num.rows
  df$social.face.rate.alt.tree = (df$social.face.alt.tree+1)/df$num.rows
  df[,face.rates] = df[,face.rates]/lapply(df[,face.rates], sum)
  return(df)
}

diurnal.faces %>%
  group_by(city) %>%
  do(diurnal.probability(.)) %>%
  as.data.frame() -> faces.diurnal.probs


## compute entropies for each city
entropy.hours.rows = function(df){  
  entrops = as.data.frame(lapply(as.data.frame(df[,face.rates]), FUN = function(x) -sum(x*log(x))))
  return(entrops)
}

faces.diurnal.probs %>%
  group_by(city) %>%
  do(entropy.hours.rows(.)) %>%
  as.data.frame() -> city.entropies


##------------------------------------------------------------------------------------------
## save results
##------------------------------------------------------------------------------------------

write.csv(cities.faces, file = "./data/city_faces.csv", row.names = FALSE, quote = FALSE)
write.csv(city.entropies, file = "./data/city_diurnal_faces_entropies.csv", row.names = FALSE, quote = FALSE)

##------------------------------------------------------------------------------------------
## visualize results
##------------------------------------------------------------------------------------------
py <- plotly()
# ggplot(cities.faces, aes(x = num.rows, y = solo.face.alt, label = city)) + geom_text() -> p
# out <- py$ggplotly(p, kwargs=list(filename="city-solo-faces", fileopt="overwrite"))
# plotly_url <- out$response$url
# 
# 
# ggplot(cities.faces, aes(x = num.rows, y = social.face.alt, label = city)) + geom_text() -> p
# out <- py$ggplotly(p, kwargs=list(filename="city-social-faces", fileopt="overwrite"))
# plotly_url <- out$response$url
# 
# ggplot(cities.faces, aes(x = solo.face.alt, y = social.face.alt, label = city)) + geom_text() -> p
# out <- py$ggplotly(p, kwargs=list(filename="city-social-solo-faces", fileopt="overwrite"))
# plotly_url <- out$response$url

# ggplot(cities.faces, aes(x = solo.face.alt/num.rows, y = num.faces.alt/num.rows, label = city)) + geom_text() -> p
# out <- py$ggplotly(p, kwargs=list(filename="city-social-solo-faces-normalized", fileopt="overwrite"))
# plotly_url <- out$response$url
# 
# ggplot(cities.faces, aes(x = solo.face.alt/num.rows, y = num.people.social.faces.alt/num.rows, label = city)) + geom_text() -> p
# out <- py$ggplotly(p, kwargs=list(filename="city-really-social-solo-faces-normalized", fileopt="overwrite"))
# plotly_url <- out$response$url
# 
# 
# ggplot(cities.faces, aes(x = solo.face.alt/num.rows, y = social.face.alt/num.rows, label = city)) + geom_text() -> p
# out <- py$ggplotly(p, kwargs=list(filename="city-actual-social-solo-faces-normalized", fileopt="overwrite"))
# plotly_url <- out$response$url