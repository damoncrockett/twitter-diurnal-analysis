## 1-weather_and_faces.R
##
## basic analysis of modes and faces 
##

setwd("~/Documents/twitter-diurnal-analysis/")

social.df = read.csv("./data/social_weather_aggs.csv", header = TRUE, stringsAsFactors = FALSE)

social.df$humidity[which(social.df$humidity == "M")] = NA
social.df$humidity = as.numeric(social.df$humidity)

social.df$hour = as.factor(social.df$hour)

model.faces = function(dependent.var, filename){
  poisson.model = glm(as.formula(paste(dependent.var, "~",independent.vars)), family="poisson", data=social.df)
  write.csv(coef(summary(poisson.model)), 
            file = paste0("~/Dropbox/TwitterPaper/results/weather-faces/coefficients/", filename, ".csv"), 
            row.names = TRUE, quote = FALSE)
}

dependent.vars = names(social.df)[6:10]


independent.vars = "city + season + week.day + hour + num.rows +
                         temp + humidity + wind + rain + sky"

for (i in c(1:length(dependent.vars))){
  print(paste("working on", dependent.vars[i]))
  model.faces(dependent.vars[i], gsub("\\.", "_", dependent.vars[i]))
}
