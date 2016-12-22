library(MASS) # for the example dataset 
library(plyr) # for recoding data
library(textir) # for standardizing the data


## the function to the cleaned dataset "rscw_mX_clean.csv" first
## AR_step=1: choose generate AR(1) type train and test dataset
## AR_step=2: choose generate AR(2) type train and test dataset
generate_train_test <- function(monthly_file, AR_step=1){
  rscw_mX <- read.csv(monthly_file)
  rscw_mX <- arrange(rscw_mX,StationId,Year,Month,Day,Hour)
  # str(rscw_mX)
  # summary(rscw_mX)
  # rscw_mX[1:6,]
  
  ## (0) remove the first column which are useless
  rscw_mX$X <- NULL
  
  ## (1) new variable: "Y"
  ## encoded from Bike_out
  ## "=0":0, ">0":1
  rscw_mX$Y <- -1
  rscw_mX$Y[rscw_mX$Bike_out==0] <- 0
  rscw_mX$Y[rscw_mX$Bike_out>0] <- 1
  rscw_mX$Y <- factor(rscw_mX$Y,levels=0:1)
  # summary(rscw_mX$Y)
  
  ## (2) turn Hour,Weekend,Holiday to type "factor"
  rscw_mX$Hour <- factor(rscw_mX$Hour,levels=0:23)
  # summary(rscw_mX$Hour)
  rscw_mX$Weekend <- factor(rscw_mX$Weekend,levels=0:1)
  # summary(rscw_mX$Weekend)
  rscw_mX$Holiday <- factor(rscw_mX$Holiday,levels=0:1)
  # summary(rscw_mX$Holiday)
  
  ## (3) set DayofWeek factor level
  rscw_mX$DayofWeek <- factor(rscw_mX$DayofWeek,
                             levels=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))
  # summary(rscw_mX$DayofWeek)
  
  ## (4) generate AR(1) or AR(2) dataset
  rscw_mX_sample <- NULL
  # AR_step <- 1
  
  for (id in unique(rscw_mX$StationId)){
    rscw_mX_id <- rscw_mX[rscw_mX$StationId==id,]
    rscw_mX_id <- arrange(rscw_mX_id,StationId,Year,Month,Day,Hour)
    if (AR_step == 1){# AR(1)
      rscw_mX_AR <- data.frame(rscw_mX_id[(1+AR_step):nrow(rscw_mX_id),
                                        c("Cluster",
                                          "Month","Day","Year","Hour",
                                          "Bike_out","Y",
                                          "Weekend","Holiday","DayofWeek")],
                              rscw_mX_id[1:(nrow(rscw_mX_id)-AR_step),
                                        c("MeanOfTripDuration_1",
                                          "MedianOfTripDuration_1",
                                          "Bike_out","Bike_in",
                                          "Customer","Daily","Subscriber","Unknown",
                                          "Weather.type","Visibility",
                                          "Temperature","Wind.speed")])
    }
    else{# AR(2)
      rscw_mX_AR <- data.frame(rscw_mX_id[(1+AR_step):nrow(rscw_mX_id),
                                        c("Cluster",
                                          "Month","Day","Year","Hour",
                                          "Bike_out","Y",
                                          "Weekend","Holiday","DayofWeek")],
                              rscw_mX_id[AR_step:(nrow(rscw_mX_id)-1),
                                        c("MeanOfTripDuration_1",
                                          "MedianOfTripDuration_1",
                                          "Bike_out","Bike_in",
                                          "Customer","Daily","Subscriber","Unknown",
                                          "Weather.type","Visibility",
                                          "Temperature","Wind.speed")],
                              rscw_mX_id[1:(nrow(rscw_mX_id)-AR_step),
                                        c("MeanOfTripDuration_2",
                                          "MedianOfTripDuration_2",
                                          "Bike_out","Bike_in",
                                          "Customer","Daily","Subscriber","Unknown",
                                          "Weather.type","Visibility",
                                          "Temperature","Wind.speed")])
    }
    rscw_mX_sample <- rbind(rscw_mX_sample,rscw_mX_AR)
  }
  
  # str(rscw_mX_sample)
  # summary(rscw_mX_sample)
  # rscw_mX_sample[1:12,c("Bike_out","Y")]
  # rscw_mX_sample[1:3,]
  
  ## rename the columns
  if (AR_step == 1){#AR(1)
    colnames(rscw_mX_sample)[which(names(rscw_mX_sample)=="Bike_out.1")] <- 
      "Bike_out_1"
    colnames(rscw_mX_sample)[which(names(rscw_mX_sample)=="Bike_in")] <- 
      "Bike_in_1"
    colnames(rscw_mX_sample)[which(names(rscw_mX_sample)=="Customer")] <- 
      "Customer_1"
    colnames(rscw_mX_sample)[which(names(rscw_mX_sample)=="Daily")] <- 
      "Daily_1"
    colnames(rscw_mX_sample)[which(names(rscw_mX_sample)=="Subscriber")] <- 
      "Subscriber_1"
    colnames(rscw_mX_sample)[which(names(rscw_mX_sample)=="Unknown")] <- 
      "Unknown_1"
    colnames(rscw_mX_sample)[which(names(rscw_mX_sample)=="Weather.type")] <- 
      "Weather.type_1"
    colnames(rscw_mX_sample)[which(names(rscw_mX_sample)=="Visibility")] <- 
      "Visibility_1"
    colnames(rscw_mX_sample)[which(names(rscw_mX_sample)=="Temperature")] <- 
      "Temperature_1"
    colnames(rscw_mX_sample)[which(names(rscw_mX_sample)=="Wind.speed")] <- 
      "Wind.speed_1"
  }
  if(AR_step == 2){#AR(2)
    colnames(rscw_mX_sample)[which(names(rscw_mX_sample)=="Bike_out.1")] <- 
      "Bike_out_1"
    colnames(rscw_mX_sample)[which(names(rscw_mX_sample)=="Bike_in")] <- 
      "Bike_in_1"
    colnames(rscw_mX_sample)[which(names(rscw_mX_sample)=="Customer")] <- 
      "Customer_1"
    colnames(rscw_mX_sample)[which(names(rscw_mX_sample)=="Daily")] <- 
      "Daily_1"
    colnames(rscw_mX_sample)[which(names(rscw_mX_sample)=="Subscriber")] <- 
      "Subscriber_1"
    colnames(rscw_mX_sample)[which(names(rscw_mX_sample)=="Unknown")] <- 
      "Unknown_1"
    colnames(rscw_mX_sample)[which(names(rscw_mX_sample)=="Weather.type")] <- 
      "Weather.type_1"
    colnames(rscw_mX_sample)[which(names(rscw_mX_sample)=="Visibility")] <- 
      "Visibility_1"
    colnames(rscw_mX_sample)[which(names(rscw_mX_sample)=="Temperature")] <- 
      "Temperature_1"
    colnames(rscw_mX_sample)[which(names(rscw_mX_sample)=="Wind.speed")] <- 
      "Wind.speed_1"
    colnames(rscw_mX_sample)[which(names(rscw_mX_sample)=="Bike_out.2")] <- 
      "Bike_out_2"
    colnames(rscw_mX_sample)[which(names(rscw_mX_sample)=="Bike_in.1")] <- 
      "Bike_in_2"
    colnames(rscw_mX_sample)[which(names(rscw_mX_sample)=="Customer.1")] <- 
      "Customer_2"
    colnames(rscw_mX_sample)[which(names(rscw_mX_sample)=="Daily.1")] <- 
      "Daily_2"
    colnames(rscw_mX_sample)[which(names(rscw_mX_sample)=="Subscriber.1")] <- 
      "Subscriber_2"
    colnames(rscw_mX_sample)[which(names(rscw_mX_sample)=="Unknown.1")] <- 
      "Unknown_2"
    colnames(rscw_mX_sample)[which(names(rscw_mX_sample)=="Weather.type.1")] <- 
      "Weather.type_2"
    colnames(rscw_mX_sample)[which(names(rscw_mX_sample)=="Visibility.1")] <- 
      "Visibility_2"
    colnames(rscw_mX_sample)[which(names(rscw_mX_sample)=="Temperature.1")] <- 
      "Temperature_2"
    colnames(rscw_mX_sample)[which(names(rscw_mX_sample)=="Wind.speed.1")] <- 
      "Wind.speed_2"
  }
  
  # names(rscw_mX_sample)
  # str(rscw_mX_sample)
  
  # (4.1) extract the categorical variables: Y, Hour, Weekend, Holiday, DayofWeek
  categ.val <- c("Y", "Hour", "Weekend", "Holiday", "DayofWeek")
  categ <- rscw_mX_sample[,categ.val];
  
  ## (4.2) extract the variables are numerical and then normalize
  if(AR_step==1){#AR(1)
    num <- rscw_mX_sample[,c("MeanOfTripDuration_1","MedianOfTripDuration_1",
                            "Bike_out_1","Bike_in_1",
                            "Customer_1","Daily_1","Subscriber_1","Unknown_1",
                            "Weather.type_1","Visibility_1","Temperature_1",
                            "Wind.speed_1")];
    num <- scale(num)
  }
  if(AR_step==2){#AR(2)
    num <- rscw_mX_sample[,c("MeanOfTripDuration_1","MedianOfTripDuration_1",
                            "Bike_out_1","Bike_in_1",
                            "Customer_1","Daily_1","Subscriber_1","Unknown_1",
                            "Weather.type_1","Visibility_1","Temperature_1",
                            "Wind.speed_1",
                            "MeanOfTripDuration_2","MedianOfTripDuration_2",
                            "Bike_out_2","Bike_in_2",
                            "Customer_2","Daily_2","Subscriber_2","Unknown_2",
                            "Weather.type_2","Visibility_2","Temperature_2",
                            "Wind.speed_2")];
    num <- scale(num)
  }
  
  ## (4.3) create design matrix; indicators for categorical variables (factors)
  Xcateg <- model.matrix(Y~.,data=categ)[,-1];
  # Xcateg[1:3,]
  
  ## (4.4) build dataset used in classification and regression later
  rscw_mX_sample_cla <- data.frame(Cluster=rscw_mX_sample$Cluster,
                                   Bike_out=rscw_mX_sample$Bike_out, 
                                   Y=rscw_mX_sample$Y, 
                                   Day=rscw_mX_sample$Day, 
                                   num, Xcateg)
  
  # str(rscw_mX_sample_cla)
  # summary(rscw_mX_sample_cla$Day)
  
  ## (5) generate train dataset and test dataset 
  rscw_mX_train <- rscw_mX_sample_cla[rscw_mX_sample_cla$Day<21,]
  rscw_mX_test <- rscw_mX_sample_cla[rscw_mX_sample_cla$Day>=21,]
  
  ## (6) remove "Day" which is not useful anymore
  rscw_mX_train$Day <- NULL
  rscw_mX_test$Day <- NULL
  
  # str(rscw_mX_train)
  # str(rscw_mX_test)
  
  # write.csv(rscw_mX_train, file = "rscw_mX_AR1_c2_train.csv")
  # write.csv(rscw_mX_test, file = "rscw_mX_AR1_c2_test.csv")
  
  write.csv(rscw_mX_train, file = "rscw_mX_AR2_c2_train.csv")
  write.csv(rscw_mX_test, file = "rscw_mX_AR2_c2_test.csv")
}


# set the function parameters
monthly_file <- "F:/Program Files/RStudio/DM_Proj/Clean dataset/Monthly/rscw_m3_c2_clean.csv"
AR_step <- 2
generate_train_test(monthly_file, AR_step)
