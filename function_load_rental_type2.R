# function to load rental dataset, station dataset and calendar dataset
# specially for time_split_type2
# i.e. dataset in Q4 2015 an Q1 2016
load_rental_type2 <- function(rental_file){
  # load data
  # rental <- read.csv("HealthyRideRentals 2015 Q4.csv")
  # rental <- read.csv("HealthyRideRentals 2016 Q1.csv")
  rental <- read.csv(rental_file)
  # station <- read.csv("HealthyRideStations2015.csv")
  # calendar <- read.csv("calendar.csv", header = FALSE)
  
  # after checking
  # 2015 Q3 and 2015 Q4 have the same column names and order
  # 2016 Q1 has different column names, so rename them
  if(rental_file=="HealthyRideRentals 2016 Q1.csv"){
    tmp <- read.csv("HealthyRideRentals 2015 Q4.csv")
    colnames(rental) <- names(tmp)
  }
  
  # remove unused variables
  rental$TripId <- NULL
  rental$BikeId <- NULL
  rental$FromStationName <- NULL
  rental$ToStationName <- NULL
  
  # switch factor variable to character variable
  rental$StartTime <- as.character(rental$StartTime)
  rental$StopTime <- as.character(rental$StopTime)
  rental$UserType <- as.character(rental$UserType)
  
  # Find there is missing values in UserType variable
  rental$UserType[rental$UserType!="Customer" &
                    rental$UserType!="Daily" &
                    rental$UserType!="Subscriber"] <- "Unknown"
  
  # split variable StartTime
  Start <- TimeSplit_type2(rental$StartTime)
  colnames(Start) <- c("Start_Month","Start_Day","Start_Year",
                       "Start_Hour","Start_Minute")
  
  # split variable StopTime
  Stop <- TimeSplit_type2(rental$StopTime)
  colnames(Stop) <- c("Stop_Month","Stop_Day","Stop_Year",
                      "Stop_Hour","Stop_Minute")
  
  rental <- data.frame(rental, Start, Stop)
  # remove unused variables
  rental$StartTime <- NULL
  rental$StopTime <- NULL
  
  # reorder by column name
  rental <- rental[,c("FromStationId","ToStationId",
                      "Start_Month","Start_Day","Start_Year",
                      "Start_Hour","Start_Minute",
                      "Stop_Month","Stop_Day","Stop_Year",
                      "Stop_Hour","Stop_Minute",
                      "TripDuration","UserType")]
  
  library(plyr)
  
  # Count_out
  stat <- aggregate(TripDuration ~ FromStationId + 
                      Start_Month + Start_Day + 
                      Start_Year + Start_Hour, data=rental, length)
  # Sort by stat,FromStationId, Start_Year, Start_Month, Start_Day, Start_Hour
  stat <- arrange(stat,FromStationId,
                  Start_Year,
                  Start_Month,
                  Start_Day,
                  Start_Hour)
  # rename TripDuration to Freq for better representation 
  colnames(stat)[which(names(stat)=="TripDuration")] <- "Bike_out"
  
  # Count_in
  stat_0 <- aggregate(TripDuration ~ ToStationId + 
                        Stop_Month + Stop_Day + 
                        Stop_Year + Stop_Hour, data=rental, length)
  # Sort by stat,FromStationId, Start_Year, Start_Month, Start_Day, Start_Hour
  stat_0 <- arrange(stat_0,ToStationId,
                    Stop_Year,
                    Stop_Month,
                    Stop_Day,
                    Stop_Hour)
  # rename TripDuration to Freq for better representation 
  colnames(stat_0)[which(names(stat_0)=="TripDuration")] <- "Bike_in"
  
  # mean of TripDuration
  mean <- aggregate(TripDuration ~ FromStationId + 
                      Start_Month + Start_Day + 
                      Start_Year + Start_Hour, data=rental, FUN="mean")
  # Sort by stat,FromStationId, Start_Year, Start_Month, Start_Day, Start_Hour
  mean <- arrange(mean,FromStationId,
                  Start_Year,
                  Start_Month,
                  Start_Day,
                  Start_Hour)
  # rename TripDuration to Mean for better representation 
  colnames(mean)[which(names(mean)=="TripDuration")] <- "Mean"
  
  # median of TripDuration
  median <- aggregate(TripDuration ~ FromStationId + 
                        Start_Month + Start_Day + 
                        Start_Year + Start_Hour, data=rental, FUN= "median")
  # Sort by stat,FromStationId, Start_Year, Start_Month, Start_Day, Start_Hour
  median <- arrange(median,FromStationId,
                    Start_Year,
                    Start_Month,
                    Start_Day,
                    Start_Hour)
  # rename TripDuration to Freq for better representation 
  colnames(median)[which(names(median)=="TripDuration")] <- "Median"
  
  # add two columns: Mean and Median into stat
  stat <- data.frame(stat, Mean=mean$Mean, Median=median$Median)
  
  # Count_usetype
  type <- aggregate(TripDuration ~ FromStationId + 
                      Start_Month + Start_Day + 
                      Start_Year + Start_Hour + UserType, data=rental, length)
  # Sort by stat,FromStationId, Start_Year, Start_Month, Start_Day, Start_Hour
  type <- arrange(type,FromStationId,
                  Start_Year,
                  Start_Month,
                  Start_Day,
                  Start_Hour,
                  UserType)
  
  library(reshape2)
  
  # casting
  type <- dcast(type, FromStationId + Start_Year + Start_Month + 
                  Start_Day + Start_Hour ~ UserType, value.var="TripDuration")
  
  type$Customer[is.na(type$Customer)] <- 0
  type$Daily[is.na(type$Daily)] <- 0
  type$Subscriber[is.na(type$Subscriber)] <- 0
  # in Q1 2016, there is no "Unknown" usertype
  if(length(which(names(type)=="Unknown"))!=0){
    type$Unknown[is.na(type$Unknown)] <- 0
  }
  if(length(which(names(type)=="Unknown"))==0){
    type$Unknown <- 0
  }
  
  # make sure stat and type have the same length
  # add four columns: Customer, Daily, Subscriber, Unknown into stat
  stat <- data.frame(stat,Customer=type$Customer,
                     Daily=type$Daily, 
                     Subscriber=type$Subscriber, 
                     Unknown=type$Unknown)
  
  # rename column names for join 
  colnames(stat) <- c("StationId","Month","Day","Year","Hour","Bike_out",
                      "Mean","Median","Customer","Daily","Subscriber",
                      "Unknown")
  colnames(stat_0) <- c("StationId","Month","Day","Year","Hour","Bike_in")
  
  # outer join: stat and stat_0
  total <- merge(stat,stat_0)
  
  # fill NAs with 0
  for (i in c("Bike_out","Mean","Median","Customer","Daily","Subscriber",
              "Unknown","Bike_in")){
    index <- is.na(total[,i])
    total[index,i] <- 0
  }
  
#   # processing calendar data
#   colnames(calendar) <- c("Date")
#   calendar$Date <- as.character(calendar$Date)
#   
#   cal <- DateSplit(calendar$Date)
#   # set factor level
#   cal$Year <- factor(cal$Year, levels=2015:2016)
#   cal$Month <- factor(cal$Month, levels=1:12)
#   cal$Day <- factor(cal$Day, levels=1:31)
#   
#   cal$Weekend <- dim(nrow(cal))
#   for (i in 1:nrow(cal)){
#     if (i%%7==5 || i%%7==6){
#       cal$Weekend[i] <- 1
#     }
#     else{
#       cal$Weekend[i] <- 0
#     }
#   }
#   
#   cal$Holiday <- rep(0,nrow(cal))
#   # Labor Day
#   cal$Holiday[cal$Month==9 & cal$Day==7 & cal$Year==2015] <- 1
#   # Thanksgiving
#   cal$Holiday[cal$Month==11 & cal$Day==26 & cal$Year==2015] <- 1
#   cal$Holiday[cal$Month==11 & cal$Day==27 & cal$Year==2015] <- 1
#   # Christmas
#   cal$Holiday[cal$Month==12 & cal$Day==25 & cal$Year==2015] <- 1
#   # New Years Day
#   cal$Holiday[cal$Month==1 & cal$Day==1 & cal$Year==2016] <- 1
#   # Martin Luther King Day
#   cal$Holiday[cal$Month==1 & cal$Day==18 & cal$Year==2016] <- 1
#   # Good Friday
#   cal$Holiday[cal$Month==3 & cal$Day==25 & cal$Year==2016] <- 1
#   # Easter Sunday
#   cal$Holiday[cal$Month==3 & cal$Day==27 & cal$Year==2016] <- 1
#   
#   # join: cal and total
#   result <- merge(cal, total)
#   # Sort by StationId, Year, Month, Day, Hour
#   result <- arrange(result,StationId,
#                     Year,
#                     Month,
#                     Day,
#                     Hour)
#   # reorder
#   result <- result[,c("StationId","Month","Day","Year","Weekend","Holiday",
#                       "Hour","Bike_out","Mean","Median",
#                       "Customer","Daily","Subscriber","Unknown","Bike_in")]
#   
#   result$Weekend <- as.factor(result$Weekend)
#   result$Holiday <- as.factor(result$Holiday)
#   # Sort by StationId, Year, Month, Day, Hour
#   result <- arrange(result,StationId,Year,Month,Day,Hour)
#   
#   return(result)
  
  # reorder
  total <- total[,c("StationId","Month","Day","Year","Hour",
                    "Bike_out","Mean","Median",
                    "Customer","Daily","Subscriber","Unknown","Bike_in")]
  
  # Sort by StationId, Year, Month, Day, Hour
  total <- arrange(total,StationId,Year,Month,Day,Hour)
  
  return(total)
}