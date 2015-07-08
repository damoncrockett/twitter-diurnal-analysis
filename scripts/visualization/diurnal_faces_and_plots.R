## diurnal_faces_and_plots
##

library(data.table)
library(plyr)
library(ggplot2)
dt = fread("~/Documents/twitter_data_grant/processedData/Top_60_faces_alt_and_alt_tree_HSV_modes.csv", header = TRUE)
dt.clean = na.omit(dt)

normalized.num.faces = function(df){
  return(data.frame(hour = df$hour[1], city = df$city[1],num.rows = nrow(df), num.faces = sum(df$num_faces_alt), 
                    face.present = length(which(df$num_faces_alt > 0)), solo.face = length(which(df$num_faces_alt == 1)),
                    social.face = length(which(df$num_faces_alt > 1))))
}

faces.hours = ddply(dt.clean, .(hour, city), normalized.num.faces)

p = ggplot(faces.hours, aes(x = as.factor(hour), y = num.faces/num.rows)) + 
  geom_point(size = .8) + facet_wrap(~city, scales = "free", nrow = 6) + 
  theme(strip.text.x = element_text(size = 6),
        strip.background = element_blank(),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

p = ggplot(faces.hours, aes(x = as.factor(hour), y = solo.face/num.rows)) + geom_point() + facet_wrap(~city, scales = "free") + ylab("Number of solo faces normalized by number of images")

p = ggplot(faces.hours, aes(x = as.factor(hour), y = social.face/num.rows)) + geom_point() + facet_wrap(~city, scales = "free") + ylab("Number of social faces normalized by number of images")

p = ggplot(faces.hours, aes(x = as.factor(hour), y = social.face/face.present)) + geom_point() + facet_wrap(~city, scales = "free") + ylab("Number of social faces normalized by number of images with faces")

p = ggplot(faces.hours, aes(x = as.factor(hour), y = face.present/num.rows)) + geom_point() + facet_wrap(~city, scales = "free") + ylab("Number of images with faces normalized by number of images")

p = ggplot(faces.hours, aes(x = as.factor(hour), y = face.present/sum(face.present))) + geom_point() + facet_wrap(~city, scales = "free") + ylab("Images with faces porportioned by hour")

p = ggplot(faces.hours, aes(x = as.factor(hour), y = face.present/sum(face.present))) + geom_point() + facet_wrap(~city, scales = "free") + ylab("Images with faces porportioned by hour")