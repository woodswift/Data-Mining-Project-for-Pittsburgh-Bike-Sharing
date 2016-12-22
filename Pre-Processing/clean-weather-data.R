# pre-request
# install.packages("RCurl")
# install.packages("XML")

# script
library(RCurl)
library(XML)
library(stringr)

## setup env
setwd("D:\\Projects\\hw\\IS\\2160\\final\\data\\raw")


## helper method
download.weather.file <- function(year,month) {
  filename = paste(year,month,"csv",sep = ".")
  url = paste("http://www.ncdc.noaa.gov/qclcd/QCLCD?reqday=E&stnid=n%2Fa&prior=N&qcname=VER2&VARVALUE=14762",year,month,"&which=ASCII+Download+%28Hourly+Obs.%29+%2810A%29&reqday=",sep = "")
  raw.content <- getURL(url)
  data <- xpathSApply(htmlParse(raw.content),"//pre",fun = xmlValue)[[1]]
  lines = strsplit(data,split = "\n")[[1]]
  
  con = file(filename,"w")
  for (i in seq(7,length(lines))) {
    if(lines[i] != ""){
      writeLines(con=con, text = lines[i])
    }
  }
  close.connection(con)
}

batch.download.by.quater <- function(year,quater){
  for (month in seq(quater*3-2,quater*3)) {
    download.weather.file(year,str_pad(month,2,"left",pad = "0"))
  }
}

## download raw data
# batch.download.by.quater(2015,3)
# batch.download.by.quater(2015,4)
# batch.download.by.quater(2016,1)


## clean and merge data


## process one row

YYYYmmDD <- function(date.num) {
  DD = date.num %% 100
  mm = ((date.num-DD) / 100) %% 100
  YYYY = (date.num - DD - mm * 100) / 10000
  return(c(YYYY,mm,DD))
}

HHMM <- function(time){
  MM = time %% 100
  HH = (time - MM)/100
  return(c(HH,MM))
}

weather.weight.table = read.csv("weather-type-weight.csv",stringsAsFactors = F)
weather.weight.map = weather.weight.table[,3]
names(weather.weight.map) = weather.weight.table[,1]


weather.codes = list()

weather <- function(w){
  w = str_trim(w)
  if(w == "" || is.na(w)){
    return(0)
  }
  ws = str_match_all(w,"(?:\\+|-)?\\w{2}")[[1]][,1]
  
  highest_level = -1
  highest_level_offset = -2
  
  for (w in ws) {
    weather_code = ""
    
    if(str_length(w) == 3){
      offset = ifelse(str_sub(w,1,1)=="+",1,-1)
      weather_code = str_sub(w,2)
      level = weather.weight.map[str_sub(w,2)]
    }
    else{
      offset = 0
      weather.code = w
      level = weather.weight.map[w]
    }
    
    weather.codes[weather_code] = 1
    
    if(level > highest_level || (level == highest_level && offset > highest_level_offset)){
      highest_level = level
      highest_level_offset = offset
    }
  }
  
  if(highest_level == 0){
    return(0)
  }
  else{
    return(highest_level*3-1+highest_level_offset)
  }
}

## #test weather
# all(
#   weather("") == 0,
#   weather("RA") == 5,
#   weather("+RA") == 6,
#   weather("-RA") == 4,
#   weather("+RA -DZ") == 6,
#   weather("-DZ +RA") == 6
# )

process.one.row <- function(row){
  ymd = YYYYmmDD(row$Date)
  hm = HHMM(row$Time)
  w = weather(row$WeatherType)
  vis = row$Visibility
  temp = row$DryBulbCelsius
  windspeed = row$WindSpeed
  return(list(ymd[1],ymd[2],ymd[3],hm[1],hm[2],w,vis,temp,windspeed))
}

## merge all data



process.one.month <- function(year,month) {
  
  year.col = list()
  month.col = list()
  day.col = list()
  hour.col = list()
  weather.type.score.col = list()
  visibility.col = list()
  temperature.col = list()
  wind.speed.col = list()
                                              
  
  data = read.csv(paste(year,str_pad(month, width = 2, pad = "0") ,"csv",sep = "."),stringsAsFactors = F)
  data$Visibility = as.numeric(data$Visibility)
  data$DryBulbCelsius  = as.numeric(data$DryBulbCelsius)
  data$WindSpeed = as.numeric(data$WindSpeed)
  
  
  row.count = dim(data)[1]
  day.cursor = -1
  hour.cursor = -1
  count = -1
  weather.type.score.sum = 0
  visibility.sum = 0
  temperature.sum = 0
  wind.speed.sum = 0
  
  is.first = TRUE
  
  for (i in seq(row.count)) {
    one.record = process.one.row(data[i,])
    
    
    if (is.first) {
      day.cursor = one.record[[3]]
      hour.cursor = one.record[[4]]
      count = 0
    }
    
    if (!is.na(one.record[[4]]) && one.record[[4]] != hour.cursor) {
      year.col[[length(year.col)+1]] = year
      month.col[[length(month.col)+1]] = month
      day.col[[length(day.col)+1]] = day.cursor
      hour.col[[length(hour.col)+1]] = hour.cursor
      weather.type.score.col[[length(weather.type.score.col)+1]] = weather.type.score.sum / count
      visibility.col[[length(visibility.col)+1]] = visibility.sum / count
      temperature.col[[length(temperature.col)+1]] = temperature.sum / count 
      wind.speed.col[[length(wind.speed.col)+1]] = wind.speed.sum / count 
      
      day.cursor = one.record[[3]]
      hour.cursor = one.record[[4]]
      count = 0
      
      weather.type.score.sum = 0
      visibility.sum = 0
      temperature.sum = 0
      wind.speed.sum = 0
    }
    
    count = count + 1
    weather.type.score.sum = weather.type.score.sum + one.record[[6]]
    visibility.sum = visibility.sum + one.record[[7]]
    temperature.sum = temperature.sum + one.record[[8]]
    wind.speed.sum = wind.speed.sum + one.record[[9]]
    
    is.first = FALSE
  }
  
  if (count > 0) {
    year.col[[length(year.col)+1]] = year
    month.col[[length(month.col)+1]] = month
    day.col[[length(day.col)+1]] = day.cursor
    hour.col[[length(hour.col)+1]] = hour.cursor
    weather.type.score.col[[length(weather.type.score.col)+1]] = weather.type.score.sum / count
    visibility.col[[length(visibility.col)+1]] = visibility.sum / count
    temperature.col[[length(temperature.col)+1]] = temperature.sum / count 
    wind.speed.col[[length(wind.speed.col)+1]] = wind.speed.sum / count
  }
  
  data.frame(year=sapply(year.col, function(x){x[[1]]}),month=sapply(month.col, function(x){x[[1]]}),day=sapply(day.col, function(x){x[[1]]}),hour = sapply(hour.col, function(x){x[[1]]}),weather.type=sapply(weather.type.score.col, function(x){x[[1]]}),visibility=sapply(visibility.col, function(x){x[[1]]}),temperature=sapply(temperature.col, function(x){x[[1]]}),wind.speed=sapply(wind.speed.col, function(x){x[[1]]}))
}

## merge
#process.one.month(2015,8)
generate.full <- function() {
  write.table(process.one.month(2015,6),file = "weather.csv",row.names = F, sep = ",")
  write.table(process.one.month(2015,7),file = "weather.csv",row.names = F, sep = ",", col.names = F, append = T)
  write.table(process.one.month(2015,8),file = "weather.csv",row.names = F, sep = ",", col.names = F, append = T)
  write.table(process.one.month(2015,9),file = "weather.csv",row.names = F, sep = ",", col.names = F, append = T)
  write.table(process.one.month(2015,10),file = "weather.csv",row.names = F, sep = ",", col.names = F, append = T)
  write.table(process.one.month(2015,11),file = "weather.csv",row.names = F, sep = ",", col.names = F, append = T)
  write.table(process.one.month(2015,12),file = "weather.csv",row.names = F, sep = ",", col.names = F, append = T)
  write.table(process.one.month(2016,1),file = "weather.csv",row.names = F, sep = ",", col.names = F, append = T)
  write.table(process.one.month(2016,2),file = "weather.csv",row.names = F, sep = ",", col.names = F, append = T)
  write.table(process.one.month(2016,3),file = "weather.csv",row.names = F, sep = ",", col.names = F, append = T)
  write.table(process.one.month(2016,4),file = "weather.csv",row.names = F, sep = ",", col.names = F, append = T)
}

generate.Q3 <- function() {
  write.table(process.one.month(2015,7),file = "weather-2015-Q3.csv",row.names = F, sep = ",")
  write.table(process.one.month(2015,8),file = "weather-2015-Q3.csv",row.names = F, sep = ",", col.names = F, append = T)
  write.table(process.one.month(2015,9),file = "weather-2015-Q3.csv",row.names = F, sep = ",", col.names = F, append = T)
}


generate.full()