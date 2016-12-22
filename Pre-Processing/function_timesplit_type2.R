# function to split Time (year/mm/dd hh:mm)
# specially for processing rental records in Q4 2015 and Q1 2016
TimeSplit_type2 <- function(Time){
  # a is a list having 2 components
  # the 1st component is Date (year/mm/dd)
  # the 2nd component is Time (hh:mm)
  a <- strsplit(Time, " ", fixed = TRUE)
  
  # Date extracts the 1st component
  Date <- NULL
  # Time extracts the 2nd component
  Time <- NULL
  for (i in 1:length(a)){
    Date <- c(Date, a[[i]][1])
    Time <- c(Time, a[[i]][2])
  }
  
  # b is a list having 3 components
  # the 1st component is Year
  # the 2nd component is Month
  # the 3rd component is Day
  b <- strsplit(Date, "/", fixed = TRUE)
  
  # c is list having 2 components
  c <- strsplit(Time, ":", fixed = TRUE)
  
  Month <- NULL
  Day <- NULL
  Year <- NULL
  Hour <- NULL
  Minute <- NULL
  for (i in 1:length(b)){
    # list b and c have the same length
    Month <- c(Month, b[[i]][2])
    Day <- c(Day, b[[i]][3])
    Year <- c(Year, b[[i]][1])
    Hour <- c(Hour, c[[i]][1])
    Minute <- c(Minute, c[[i]][2])
  }
  
  # print(length(Month))
  # print(length(Day))
  # print(length(Year))
  # print(length(Hour))
  # print(length(Minute))
  
  result <- data.frame(Month, Day, Year, Hour, Minute)
  return (result)
}