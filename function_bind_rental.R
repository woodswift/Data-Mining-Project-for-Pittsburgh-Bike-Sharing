# function to load and bind rental datasets from 2015 Q3 to 2016 Q1
bind_rental <- function(){
  rental_file_Q3 <- "HealthyRideRentals 2015 Q3.csv"
  rental_file_Q4 <- "HealthyRideRentals 2015 Q4.csv"
  rental_file_Q1 <- "HealthyRideRentals 2016 Q1.csv"
  
  rental_Q3 <- load_rental_type1(rental_file_Q3)
  rental_Q4 <- load_rental_type2(rental_file_Q4)
  rental_Q1 <- load_rental_type2(rental_file_Q1)
  
  rental <- rbind.data.frame(rental_Q3, rental_Q4)
  rental <- rbind.data.frame(rental, rental_Q1)
  
  return(rental)
}