# function to load and bind rental datasets from 2015 Q3 to 2016 Q1
bind_rental <- function(){
  rental_file_Q3 <- "F:/Program Files/RStudio/DM_Proj/HealthyRideRentals 2015 Q3.csv"
  rental_file_Q4 <- "F:/Program Files/RStudio/DM_Proj/HealthyRideRentals 2015 Q4.csv"
  rental_file_Q1 <- "F:/Program Files/RStudio/DM_Proj/HealthyRideRentals 2016 Q1.csv"
  
  rental_Q3 <- load_rental_type1(rental_file_Q3)
  rental_Q4 <- load_rental_type2(rental_file_Q4)
  rental_Q1 <- load_rental_type2(rental_file_Q1)
  
  rental <- rbind.data.frame(rental_Q3, rental_Q4)
  rental <- rbind.data.frame(rental, rental_Q1)
  
  # remove the record on June 30 2015 and April 1 2016
  index0630 <- which(rental$Month==6 & rental$Day==30 & rental$Year==2015)
  index0401 <- which(rental$Month==4 & rental$Day==1 & rental$Year==2016)
  index <- c(index0630, index0401)
  rental <- rental[-index,]
  
  return(rental)
}