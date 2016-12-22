# StationId (Start point), Year (yy), Month (mm), Day (dd), 
# Hour (0-23), Weekend (0 or 1), Public Holiday (0 or 1), 
# Bike-in, Bike-out, (per station per hour)
# Bike-out-customer, Bike-out-Subscriber, Bike-out-Daily, Bike-out-Unknown 
# Mean-Trip Duration (s), Median-Trip Duration (s), (per trip)
# RackQnty, Latitude, Longitude
# the above variable are included in dataset "rsc"

# dataset "rental_rec" is the rental record by day
# from 2015 Q3 to 2016 Q1
rental_rec <- bind_rental()
rental_rec[1:6,]
summary(rental_rec)
str(rental_rec)
write.csv(rental_rec, file = "rental_rec_clean.csv")

# dataset "station_cal" is the station info outer join by calendar info
station_cal <- load_station_calendar()
station_cal[1:6,]
summary(station_cal)
str(station_cal)
write.csv(station_cal, file = "station_cal_clean.csv")

# merge "rental_rec" and "station_cal"
rsc <- merge_all(rental_rec, station_cal)
# rm(rental_rec, station_cal)
str(rsc)
summary(rsc)
rsc[1:6,]
head(rsc)
tail(rsc)
write.csv(rsc, file = "rsc_clean.csv")

# dataset "weather" is the weather information by hour
weather <- load_weather()
str(weather)
summary(weather)
head(weather)
write.csv(weather, file = "weather_clean.csv")

# merge "rsc" and "weather"
rscw <- merge(rsc, weather)
rscw <- arrange(rscw,StationId,Year,Month,Day,Hour)
names(rscw)
str(rscw)
summary(rscw)
head(rscw)
tail(rscw)
write.csv(rscw, file = "rscw_clean.csv")