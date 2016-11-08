# function to merge rental_record and station_calendar
merge_all <- function(rental_rec,station_cal){
  # merge two data into one
  total <- merge(rental_rec,station_cal,all.y = TRUE)
  # str(total)
  total$Year <- factor(total$Year, levels=2015:2016)
  total$Month <- factor(total$Month, levels=1:12)
  total$Day <- factor(total$Day, levels=1:31)
  total$Hour <- factor(total$Hour, levels=0:23)
  # summary(total)
  # total[1:6,]
  total <- arrange(total,StationId,Year,Month,Day,Hour)
  # fill NAs with 0
  for (i in c("Bike_out","Mean","Median","Customer","Daily","Subscriber",
              "Unknown","Bike_in")){
    index <- is.na(total[,i])
    total[index,i] <- 0
  }
  return(total)
}