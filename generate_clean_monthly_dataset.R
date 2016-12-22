# load the cleaned dataset "rscw_clean.csv"
rscw <- read.csv("F:/Program Files/RStudio/DM_Proj/Clean dataset/rscw_clean.csv")

# remove unused "X"
rscw$X <- NULL

m <- 3
rscwm <- rscw[rscw$Month==m,]
rscwm <- arrange(rscwm,StationId,Year,Month,Day,Hour)
names(rscwm)
str(rscwm)
summary(rscwm)
head(rscwm)
tail(rscwm)

# load the cleaned dataset "cluster_station.csv"
clu.result <- read.csv("F:/Program Files/RStudio/DM_Proj/Clean dataset/cluster_station.csv")

# remove unused "X"
clu.result$X <- NULL

names(clu.result)
str(clu.result)
summary(clu.result)
head(clu.result)
tail(clu.result)

# add cluster variable
rscwm <- merge(rscwm, clu.result[,c("StationId","Cluster")])

# change the index corresponding to m
write.csv(rscwm, file = "rscw_m3_c2_clean.csv")
