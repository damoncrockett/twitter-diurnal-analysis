## prepping-boston-data-for-image-hist.R
##

setwd("~/Documents/twitter-diurnal-analysis/")

boston = read.csv("~/Documents/twitter-diurnal-analysis/data/boston_bombing.csv", header = TRUE, stringsAsFactors = FALSE)

# > unique(boston$day)
# [1] "2013-04-15" "2013-04-14" "2013-04-13" "2013-04-16" "2013-04-17" "2013-04-18" "2013-04-19"

get.shifted.hours = function(i){
  if (boston$day[i] == "2013-04-13"){
    hour.shift = 0*24
  }
  if (boston$day[i] == "2013-04-14"){
    hour.shift = 1*24 
  }
  if (boston$day[i] == "2013-04-15"){
    hour.shift = 2*24
  }
  if (boston$day[i] == "2013-04-16"){
    hour.shift = 3*24
  }
  if (boston$day[i] == "2013-04-17"){
    hour.shift = 4*24
  }
  if (boston$day[i] == "2013-04-18"){
    hour.shift = 5*24
  }
  if (boston$day[i] == "2013-04-19"){
    hour.shift = 6*24
  }
  return(boston$hour[i] + hour.shift)
}

boston$hour.shifted = sapply(c(1:nrow(boston)), get.shifted.hours)

local.image.path = "/Users/myazdaniUCSD/Documents/twitter_data_grant/images/boston_bombin/"
boston$image.path = paste0(local.image.path, boston$filename)

library(data.table)
dt = fread("./data/Top_60_faces_alt_and_alt_tree_HSV_modes.csv", header = TRUE)
dt.boston = as.data.frame(subset(dt, city == "Boston"))

boston.HSV = merge(boston, dt.boston[,c("file_path", "H.mode", "S.mode", "V.mode")], all.x = TRUE)

boston.HSV = boston.HSV[,c("image.path", "hour.shifted", "H.mode")]
dummy.df = data.frame(image.path = 
                        rep("/Users/myazdaniUCSD/Desktop/black-thumb.png", length(c(min(boston.HSV$hour.shifted): max(boston.HSV$hour.shifted)))),
                      hour.shifted = c(min(boston.HSV$hour.shifted): max(boston.HSV$hour.shifted)),
                      H.mode = -1)
boston.HSV = rbind(boston.HSV, dummy.df)
write.table(boston.HSV, file = "./data/boston_image_hist.csv", row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ",")
