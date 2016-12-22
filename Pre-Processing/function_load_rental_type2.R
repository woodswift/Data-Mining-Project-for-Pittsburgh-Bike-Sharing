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
  if(rental_file=="F:/Program Files/RStudio/DM_Proj/HealthyRideRentals 2016 Q1.csv"){
    tmp <- read.csv("F:/Program Files/RStudio/DM_Proj/HealthyRideRentals 2015 Q4.csv")
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
  
  # find there is missing values in UserType variable
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
  
  # Bike_out
  stat <- aggregate(TripDuration ~ FromStationId + 
                      Start_Month + Start_Day + 
                      Start_Year + Start_Hour, data=rental, FUN = "length")
  # sort stat by FromStationId, Start_Year, Start_Month, Start_Day, Start_Hour
  stat <- arrange(stat,FromStationId,
                  Start_Year,
                  Start_Month,
                  Start_Day,
                  Start_Hour)
  # rename TripDuration to Bike_out for better representation 
  colnames(stat)[which(names(stat)=="TripDuration")] <- "Bike_out"
  
  # Bike_in
  stat_0 <- aggregate(TripDuration ~ ToStationId + 
                        Stop_Month + Stop_Day + 
                        Stop_Year + Stop_Hour, data=rental, FUN = "length")
  # Sort stat_0 by ToStationId, Stop_Year, Stop_Month, Stop_Day, Stop_Hour
  stat_0 <- arrange(stat_0,ToStationId,
                    Stop_Year,
                    Stop_Month,
                    Stop_Day,
                    Stop_Hour)
  # rename TripDuration to Bike_in for better representation 
  colnames(stat_0)[which(names(stat_0)=="TripDuration")] <- "Bike_in"
  
  # mean of TripDuration less than 1 hour
  mean_1 <- aggregate(TripDuration ~ FromStationId + 
                        Start_Month + Start_Day + 
                        Start_Year + Start_Hour, 
                      data=rental[rental$TripDuration<=3600,], FUN = "mean")
  # sort mean_1 by FromStationId, Start_Year, Start_Month, Start_Day, Start_Hour
  mean_1 <- arrange(mean_1,FromStationId,
                    Start_Year,
                    Start_Month,
                    Start_Day,
                    Start_Hour)
  # rename TripDuration to MeanOfTripDuration_1 for better representation 
  colnames(mean_1)[which(names(mean_1)=="TripDuration")] <- "MeanOfTripDuration_1"
  
  # mean_1[1:6,]
  
  # mean of TripDuration less than 2 hour
  mean_2 <- aggregate(TripDuration ~ FromStationId + 
                        Start_Month + Start_Day + 
                        Start_Year + Start_Hour, 
                      data=rental[rental$TripDuration<=7200,], FUN = "mean")
  # sort mean_2 by FromStationId, Start_Year, Start_Month, Start_Day, Start_Hour
  mean_2 <- arrange(mean_2,FromStationId,
                    Start_Year,
                    Start_Month,
                    Start_Day,
                    Start_Hour)
  # rename TripDuration to MeanOfTripDuration_2 for better representation 
  colnames(mean_2)[which(names(mean_2)=="TripDuration")] <- "MeanOfTripDuration_2"
  
  # mean_2[1:6,]
  
  # median of TripDuration less than 1 hour
  median_1 <- aggregate(TripDuration ~ FromStationId + 
                          Start_Month + Start_Day + 
                          Start_Year + Start_Hour, 
                        data=rental[rental$TripDuration<=3600,], FUN = "median")
  # Sort median_1 by FromStationId, Start_Year, Start_Month, Start_Day, Start_Hour
  median_1 <- arrange(median_1,FromStationId,
                      Start_Year,
                      Start_Month,
                      Start_Day,
                      Start_Hour)
  # rename TripDuration to MedianOfTripDuration_1 for better representation 
  colnames(median_1)[which(names(median_1)=="TripDuration")] <- "MedianOfTripDuration_1"
  
  # median_1[1:6,]
  
  # median of TripDuration less than 2 hour
  median_2 <- aggregate(TripDuration ~ FromStationId + 
                          Start_Month + Start_Day + 
                          Start_Year + Start_Hour, 
                        data=rental[rental$TripDuration<=7200,], FUN = "median")
  # Sort median_2 by FromStationId, Start_Year, Start_Month, Start_Day, Start_Hour
  median_2 <- arrange(median_2,FromStationId,
                      Start_Year,
                      Start_Month,
                      Start_Day,
                      Start_Hour)
  # rename TripDuration to MedianOfTripDuration_2 for better representation 
  colnames(median_2)[which(names(median_2)=="TripDuration")] <- "MedianOfTripDuration_2"
  
  # median_2[1:6,]
  
  # add median into mean dataset for comparison
  mean_1 <- data.frame(mean_1, MedianOfTripDuration_1=median_1$MedianOfTripDuration_1)
  mean_2 <- data.frame(mean_2, MedianOfTripDuration_2=median_2$MedianOfTripDuration_2)
  rm(median_1, median_2)
  
  # mean_1[1:6,]
  # mean_2[1:6,]
  
  # merge stat with mean_1 and mean_2
  stat <- merge(stat,mean_1,all=TRUE)
  stat <- merge(stat,mean_2,all=TRUE)
  rm(mean_1, mean_2)
  # summary(stat)
  
  # fill NAs with 0
  for (i in c("MeanOfTripDuration_1","MedianOfTripDuration_1",
              "MeanOfTripDuration_2","MedianOfTripDuration_2")){
    index <- is.na(stat[,i])
    stat[index,i] <- 0
  }
  
  # Count_usetype
  type <- aggregate(TripDuration ~ FromStationId + 
                      Start_Month + Start_Day + 
                      Start_Year + Start_Hour + UserType, data=rental, length)
  # sort by stat,FromStationId, Start_Year, Start_Month, Start_Day, Start_Hour
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
                      "MeanOfTripDuration_1","MedianOfTripDuration_1",
                      "MeanOfTripDuration_2","MedianOfTripDuration_2",
                      "Customer","Daily","Subscriber","Unknown")
  colnames(stat_0) <- c("StationId","Month","Day","Year","Hour","Bike_in")
  
  # outer join: stat and stat_0
  total <- merge(stat,stat_0,all = TRUE)
  
  # fill NAs with 0
  for (i in c("Bike_out",
              "MeanOfTripDuration_1","MedianOfTripDuration_1",
              "MeanOfTripDuration_2","MedianOfTripDuration_2",
              "Customer","Daily","Subscriber","Unknown",
              "Bike_in")){
    index <- is.na(total[,i])
    total[index,i] <- 0
  }
  
  # reorder
  total <- total[,c("StationId","Month","Day","Year","Hour",
                    "Bike_out",
                    "MeanOfTripDuration_1","MedianOfTripDuration_1",
                    "MeanOfTripDuration_2","MedianOfTripDuration_2",
                    "Customer","Daily","Subscriber","Unknown",
                    "Bike_in")]
  
  # sort by StationId, Year, Month, Day, Hour
  total <- arrange(total,StationId,Year,Month,Day,Hour)
  
  return(total)
}