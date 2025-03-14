# function to load stations and calendar
load_station_calendar <- function(){
  # load stations dataset
  # stations datasets are the same for Q3,Q4 2015 and Q1 2016 after checking
  station <- read.csv("F:/Program Files/RStudio/DM_Proj/HealthyRideStations2015.csv")
  # remove unused variable
  station$StationName <- NULL
  # rename variable "StationNum" to "StationId"
  colnames(station)[which(names(station)=="StationNum")] <- "StationId"
  # str(station)
  # summary(station)
  
  # load calendar dataset
  calendar <- read.csv("F:/Program Files/RStudio/DM_Proj/calendar.csv", header = FALSE)
  # processing calendar data
  colnames(calendar) <- c("Date")
  calendar$Date <- as.character(calendar$Date)
  
  cal <- DateSplit(calendar$Date)
  # set factor level
  cal$Year <- factor(cal$Year, levels=2015:2016)
  cal$Month <- factor(cal$Month, levels=1:12)
  cal$Day <- factor(cal$Day, levels=1:31)
  
  # add a new variable "Weekend"
  cal$Weekend <- dim(nrow(cal))
  for (i in 1:nrow(cal)){
    if (i%%7==4 || i%%7==5){
      cal$Weekend[i] <- 1
    }
    else{
      cal$Weekend[i] <- 0
    }
  }
  
  # add a new variable "Holiday"
  cal$Holiday <- rep(0,nrow(cal))
  # Labor Day
  cal$Holiday[cal$Month==9 & cal$Day==7 & cal$Year==2015] <- 1
  # Thanksgiving
  cal$Holiday[cal$Month==11 & cal$Day==26 & cal$Year==2015] <- 1
  cal$Holiday[cal$Month==11 & cal$Day==27 & cal$Year==2015] <- 1
  # Christmas
  cal$Holiday[cal$Month==12 & cal$Day==25 & cal$Year==2015] <- 1
  # New Years Day
  cal$Holiday[cal$Month==1 & cal$Day==1 & cal$Year==2016] <- 1
  # Martin Luther King Day
  cal$Holiday[cal$Month==1 & cal$Day==18 & cal$Year==2016] <- 1
  # Good Friday
  cal$Holiday[cal$Month==3 & cal$Day==25 & cal$Year==2016] <- 1
  # Easter Sunday
  cal$Holiday[cal$Month==3 & cal$Day==27 & cal$Year==2016] <- 1
  
  # add a new variable "DayofWeek"
  dw <- c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")
  
  # July 1, 2015 ("Wed") to July 4, 2015 ("Sat") is week 1
  for (i in 1:4){
    cal$DayofWeek[i] <- dw[i+3]
  }
  
  for (i in 5:275){
    cal$DayofWeek[i] <- dw[1+(i+2)%%7]
  }
  
  cal$DayofWeek <- as.factor(cal$DayofWeek)
  cal$DayofWeek <- factor(cal$DayofWeek, levels=dw)
  # str(cal)
  # summary(cal$DayofWeek)
  
  cal <- arrange(cal, Year, Month, Day)
  
  cal <- merge(cal, data.frame(Hour=as.factor(0:23)))
  # set factor level
  cal$Hour <- factor(cal$Hour, levels=0:23)
  cal <- arrange(cal, Year, Month, Day, Hour)
  # str(cal)
  # summary(cal)
  # summary(cal$Month)
  # summary(cal$Day)
  # summary(cal$Year)
  
  total <- merge(station, cal, all = TRUE)
  # str(total)
  # summary(total)
  total$Weekend <- factor(total$Weekend, levels=0:1)
  total$Holiday  <- factor(total$Holiday , levels=0:1)
  
  total <- arrange(total,StationId,Year,Month,Day,Hour)
  
  return(total)
}