load_weather <- function() {
  # processing weather data
  weather_file <- "F:/Program Files/RStudio/DM_Proj/weather.csv"
  weather <- read.csv(weather_file)
  colnames(weather) <- c("Year","Month","Day","Hour",
                           "Weather.type","Visibility","Temperature","Wind.speed")
  weather$Year <- factor(weather$Year, levels=2015:2016)
  weather$Month <- factor(weather$Month, levels=1:12)
  weather$Day <- factor(weather$Day, levels=1:31)
  weather$Hour <- factor(weather$Hour, levels=0:23)
  # str(weather)
  # summary(weather)
  # head(weather)
  
  # removing NA's
  for (i in c("Weather.type","Visibility","Temperature","Wind.speed")){
    index <- is.na(weather[,i])
    index <- which(index==TRUE)
    for (j in index){
      if (!is.na(weather[j-1,i])){# j-1 th value is wrong for NA
        weather[j,i] <- weather[j-1,i]
      }
      else{# assign j+1 th value
        weather[j,i] <- weather[j+1,i]
      }
    }
  }
  
  return (weather)
}