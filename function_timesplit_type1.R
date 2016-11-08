# function to split Time (mm/dd/year hh:mm)
# specially for processing rental records in Q3 2015
TimeSplit_type1 <- function(Time){
  # a is a list having 2 components
  # the 1st component is Date (mm/dd/year)
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
  # the 1st component is Month
  # the 2nd component is Day
  # the 3rd component is Year
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
    Month <- c(Month, b[[i]][1])
    Day <- c(Day, b[[i]][2])
    Year <- c(Year, b[[i]][3])
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