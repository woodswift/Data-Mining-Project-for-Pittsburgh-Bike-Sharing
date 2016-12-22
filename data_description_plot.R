# load the cleaned dataset "rscw_clean.csv"
rscw <- read.csv("F:/Program Files/RStudio/DM_Proj/Clean dataset/rscw_clean.csv")

# load the cleaned dataset "rscw_m9_clean.csv"
rscw <- read.csv("rscw_m9_clean.csv")
str(rscw)
summary(rscw)

rscw$X <- NULL

# set factor levels
rscw$Month <- factor(rscw$Month,levels=1:12)
rscw$Day <- factor(rscw$Day,levels=1:31)
rscw$Year <- factor(rscw$Year,levels=2015:2016)
rscw$Hour <- factor(rscw$Hour,levels=0:23)
rscw$Weekend <- factor(rscw$Weekend,levels=0:1)
rscw$Holiday <- factor(rscw$Holiday,levels=0:1)
rscw$DayofWeek <- factor(rscw$DayofWeek,
                         levels=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))

length(rscw$Bike_out[rscw$Bike_out==0])
length(rscw$Bike_out[rscw$Bike_out==1])
length(rscw$Bike_out[rscw$Bike_out==2])
length(rscw$Bike_out[rscw$Bike_out==3])
length(rscw$Bike_out[rscw$Bike_out>3])

length(rscw$Bike_out[rscw$Bike_out==0])/330000
length(rscw$Bike_out[rscw$Bike_out==1])/330000
length(rscw$Bike_out[rscw$Bike_out==2])/330000
length(rscw$Bike_out[rscw$Bike_out==3])/330000
length(rscw$Bike_out[rscw$Bike_out>3])/330000

# ------ Data Description ------
library(plyr)


# Bike_out sum group by Hour
out_hour <- aggregate(Bike_out ~ Hour, data=rscw, FUN = "sum")
# Sort by Hour
out_hour <- arrange(out_hour,Hour)
# rename to sum for better representation 
colnames(out_hour)[which(names(out_hour)=="Bike_out")] <- "Bike_out_sum"
out_hour[1:10,]
str(out_hour)
library(ggplot2)
ggplot(data=out_hour, aes(x=Hour, y=Bike_out_sum)) +
  geom_bar(stat="identity", width=0.5) + 
  # ggtitle("Sum(Bike_out) group by Hour") +
  theme(text = element_text(size=20))
rm(out_hour)

# Bike_out sum group by Month
out_month <- aggregate(Bike_out ~ Month, data=rscw, FUN = "sum")
# Sort by Month
out_month <- arrange(out_month,Month)
# rename to sum for better representation 
colnames(out_month)[which(names(out_month)=="Bike_out")] <- "Bike_out_sum"
out_month[1:10,]
str(out_month)
library(ggplot2)
theme_set(theme_bw())
ggplot(data=out_month, aes(x=Month, y=Bike_out_sum)) +
  geom_bar(stat="identity", width=0.5) + 
  # ggtitle("Sum(Bike_out) group by Month") +
  theme(text = element_text(size=20))
rm(out_month)

# Bike_out sum group by StationId, Hour
out_hour <- aggregate(Bike_out ~ StationId + Hour, data=rscw, FUN = "sum")
str(out_hour)
out_hour$Hour <- as.numeric(out_hour$Hour)-1
# Daytime encoded as 1
out_hour$Period[out_hour$Hour<=20 & out_hour$Hour>=9] <- 1
# Daytime encoded as 0
out_hour$Period[out_hour$Hour>20 | out_hour$Hour<9] <- 0
out_hour$StationId <- as.factor(out_hour$StationId)
out_hour$Hour <- as.factor(out_hour$Hour)
out_hour$Period <- as.factor(out_hour$Period)
# set factor Hour's level
out_hour$Hour <- factor(out_hour$Hour, levels = 0:23)
# Sort by Hour, StationId
out_hour <- arrange(out_hour,Hour,StationId)
# rename for better representation 
colnames(out_hour)[which(names(out_hour)=="Bike_out")] <- "Bike_out_sum"
summary(out_hour)
out_hour[1:10,]
library(ggplot2)
theme_set(theme_bw())
# scatterplot
ggplot(data=out_hour, aes(x=Hour, y=Bike_out_sum, color=Period, 
                          group=StationId)) + 
  geom_point(shape=1) + 
  geom_line() + 
  # ggtitle("Sum(Bike_out) group by StationId, Hour") +
  theme(text = element_text(size=20))
rm(out_hour)


# Bike_out mean group by StationId, Hour, Weekend
out_hour <- aggregate(Bike_out ~ StationId + Hour + Weekend, 
                      data=rscw, FUN = "mean")
str(out_hour)
out_hour$Hour <- as.numeric(out_hour$Hour)-1
# Daytime encoded as 1
out_hour$Period[out_hour$Hour<=20 & out_hour$Hour>=9] <- 1
# Daytime encoded as 0
out_hour$Period[out_hour$Hour>20 | out_hour$Hour<9] <- 0
out_hour$StationId <- as.factor(out_hour$StationId)
out_hour$Hour <- as.factor(out_hour$Hour)
out_hour$Period <- as.factor(out_hour$Period)
# set factor Hour's level
out_hour$Hour <- factor(out_hour$Hour, levels = 0:23)
# Sort by Hour, StationId
out_hour <- arrange(out_hour,Hour,StationId)
# rename for better representation 
colnames(out_hour)[which(names(out_hour)=="Bike_out")] <- "Bike_out_mean"
summary(out_hour)
out_hour[1:10,]
library(ggplot2)
theme_set(theme_bw())
# weekday case is at the top
# weekend case is at the bottom
ggplot(data=out_hour, aes(x=Hour, y=Bike_out_mean, color=Period, 
                          group=StationId)) + 
  geom_point(shape=1) + 
  geom_line() + 
  # ggtitle("mean(Bike_out) group by StationId, Hour, Weekend") +
  theme(text = element_text(size=20)) +
  facet_grid(Weekend ~ .)
rm(out_hour)


# Bike_out mean group by StationId, Hour, Holiday
out_hour <- aggregate(Bike_out ~ StationId + Hour + Holiday, 
                      data=rscw, FUN = "mean")
str(out_hour)
out_hour$Hour <- as.numeric(out_hour$Hour)-1
# Daytime encoded as 1
out_hour$Period[out_hour$Hour<=20 & out_hour$Hour>=9] <- 1
# Daytime encoded as 0
out_hour$Period[out_hour$Hour>20 | out_hour$Hour<9] <- 0
out_hour$StationId <- as.factor(out_hour$StationId)
out_hour$Hour <- as.factor(out_hour$Hour)
out_hour$Period <- as.factor(out_hour$Period)
# set factor Hour's level
out_hour$Hour <- factor(out_hour$Hour, levels = 0:23)
# Sort by Hour, StationId
out_hour <- arrange(out_hour,Hour,StationId)
# rename for better representation 
colnames(out_hour)[which(names(out_hour)=="Bike_out")] <- "Bike_out_mean"
summary(out_hour)
out_hour[1:10,]
library(ggplot2)
theme_set(theme_bw())
# non-Holiday case is at the top
# Holiday case is at the bottom
ggplot(data=out_hour, aes(x=Hour, y=Bike_out_mean, color=Period, 
                          group=StationId)) + 
  geom_point(shape=1) + 
  geom_line() + 
  # ggtitle("mean(Bike_out) group by StationId, Hour, Holiday") +
  theme(text = element_text(size=20)) + 
  facet_grid(Holiday ~ .)
rm(out_hour)


# Bike_out mean group by StationId, Hour, DayofWeek
out_hour <- aggregate(Bike_out ~ StationId + Hour + DayofWeek, 
                      data=rscw, FUN = "mean")
str(out_hour)
out_hour$Hour <- as.numeric(out_hour$Hour)-1
# Daytime encoded as 1
out_hour$Period[out_hour$Hour<=20 & out_hour$Hour>=9] <- 1
# Daytime encoded as 0
out_hour$Period[out_hour$Hour>20 | out_hour$Hour<9] <- 0
out_hour$StationId <- as.factor(out_hour$StationId)
out_hour$Hour <- as.factor(out_hour$Hour)
out_hour$Period <- as.factor(out_hour$Period)
# set factor Hour's level
out_hour$Hour <- factor(out_hour$Hour, levels = 0:23)
# Sort by Hour, StationId
out_hour <- arrange(out_hour,Hour,StationId)
# rename for better representation 
colnames(out_hour)[which(names(out_hour)=="Bike_out")] <- "Bike_out_mean"
summary(out_hour)
out_hour[1:10,]
library(ggplot2)
theme_set(theme_bw())
ggplot(data=out_hour, aes(x=Hour, y=Bike_out_mean, color=Period, 
                          group=StationId)) + 
  geom_point(shape=1) + 
  geom_line() + 
  # ggtitle("mean(Bike_out) group by StationId, Hour, DayofWeek") +
  theme(text = element_text(size=20)) + 
  facet_grid(DayofWeek ~ .)
rm(out_hour)


# scatterplot for numerical variables
library(ggplot2)
theme_set(theme_bw())
ggplot(data=rscw, aes(x=MeanOfTripDuration_1, y=Bike_out, color=Weekend)) + 
  geom_point(shape=1, alpha=0.3) + 
  # ggtitle("scatter plot of MeanOfTripDuration_1 and Bike_out") +
  theme(text = element_text(size=20))


library(ggplot2)
theme_set(theme_bw())
ggplot(data=rscw, aes(x=MeanOfTripDuration_2, y=Bike_out, color=Weekend)) + 
  geom_point(shape=1, alpha=0.3) + 
  # ggtitle("scatter plot of MeanOfTripDuration_2 and Bike_out") +
  theme(text = element_text(size=20))


library(ggplot2)
theme_set(theme_bw())
ggplot(data=rscw, aes(x=MedianOfTripDuration_1, y=Bike_out, color=Weekend)) + 
  geom_point(shape=1, alpha=0.3) + 
  # ggtitle("scatter plot of MedianOfTripDuration_1 and Bike_out") +
  theme(text = element_text(size=20))


library(ggplot2)
theme_set(theme_bw())
ggplot(data=rscw, aes(x=MedianOfTripDuration_2, y=Bike_out, color=Weekend)) + 
  geom_point(shape=1, alpha=0.3) + 
  # ggtitle("scatter plot of MedianOfTripDuration_2 and Bike_out") +
  theme(text = element_text(size=20))


library(ggplot2)
theme_set(theme_bw())
ggplot(data=rscw, aes(x=Weather.type, y=Bike_out, color=Weekend)) + 
  geom_point(shape=1, alpha=0.3) + 
  # ggtitle("scatter plot of Weather.type and Bike_out") +
  theme(text = element_text(size=20))


library(ggplot2)
theme_set(theme_bw())
ggplot(data=rscw, aes(x=Visibility, y=Bike_out, color=Weekend)) + 
  geom_point(shape=1, alpha=0.3) + 
  # ggtitle("scatter plot of Visibility and Bike_out") +
  theme(text = element_text(size=20))


library(ggplot2)
theme_set(theme_bw())
ggplot(data=rscw, aes(x=Temperature, y=Bike_out, color=Weekend)) + 
  geom_point(shape=1, alpha=0.3) + 
  # ggtitle("scatter plot of Temperature and Bike_out") +
  theme(text = element_text(size=20))


library(ggplot2)
theme_set(theme_bw())
ggplot(data=rscw, aes(x=Wind.speed, y=Bike_out, color=Weekend)) + 
  geom_point(shape=1, alpha=0.3) + 
  # ggtitle("scatter plot of Wind.speed and Bike_out") +
  theme(text = element_text(size=20))


t <- quantile(rscw$Temperature)
t <- as.vector(t)
# new variable temperature type "t_type"
rscw$t_type <- -1
rscw$t_type[rscw$Temperature<=t[1]] <- 0
rscw$t_type[rscw$Temperature>t[1] & rscw$Temperature<=t[2]] <- 1
rscw$t_type[rscw$Temperature>t[2] & rscw$Temperature<=t[3]] <- 2
rscw$t_type[rscw$Temperature>t[3]] <- 3
rscw$t_type <- factor(rscw$t_type,levels=0:3)
summary(rscw$t_type)

# Bike_out sum group by temperature type
out_hour <- aggregate(Bike_out ~ t_type, data=rscw, FUN = "mean")
# Sort by t_type
out_hour <- arrange(out_hour,t_type)
# rename to sum for better representation 
colnames(out_hour)[which(names(out_hour)=="Bike_out")] <- "Bike_out_mean"
out_hour[1:10,]
str(out_hour)
library(ggplot2)
theme_set(theme_bw())
ggplot(data=out_hour, aes(x=t_type, y=Bike_out_mean)) +
  geom_bar(stat="identity", width=0.5) + 
  # ggtitle("mean(Bike_out) group by temperature quantile") +
  theme(text = element_text(size=20))
rm(out_hour)