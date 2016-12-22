# function to split Date (mm/dd/year)
# specially for processing calendar.csv
DateSplit <- function(Date){
  # b is a list having 3 components
  # the 1st component is Month
  # the 2nd component is Day
  # the 3rd component is Year
  b <- strsplit(Date, "/", fixed = TRUE)
  
  Month <- NULL
  Day <- NULL
  Year <- NULL
  
  for (i in 1:length(b)){
    Month <- c(Month, b[[i]][1])
    Day <- c(Day, b[[i]][2])
    Year <- c(Year, b[[i]][3])
  }
  
  # print(length(Month))
  # print(length(Day))
  # print(length(Year))
  
  result <- data.frame(Month, Day, Year)
  return (result)
}