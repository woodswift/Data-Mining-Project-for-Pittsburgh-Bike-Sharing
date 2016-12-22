dataset <- read.csv("F:/Program Files/RStudio/DM_Proj/Clean dataset/rsc_clean.csv", header=T, sep=",")
dim(dataset)
str(dataset)

library(plyr)
set.seed(123)

## aggregate by StationId, Hour
bike_out_aggr <- aggregate(Bike_out ~ StationId + Hour, data=dataset, FUN="sum")
bike_out_aggr <- arrange(bike_out_aggr, StationId, Hour)
bike_out_aggr[1:6,]

bike_in_aggr <- aggregate(Bike_in ~ StationId + Hour, data=dataset, FUN="sum")
bike_in_aggr <- arrange(bike_in_aggr, StationId, Hour)
bike_in_aggr[1:6,]

meanOfTripDuration_1_aggr <- aggregate(MeanOfTripDuration_1 ~ StationId + Hour, data=dataset, FUN="sum")
meanOfTripDuration_1_aggr <- arrange(meanOfTripDuration_1_aggr, StationId, Hour)
meanOfTripDuration_1_aggr[1:6,]

meanOfTripDuration_2_aggr <- aggregate(MeanOfTripDuration_2 ~ StationId + Hour, data=dataset, FUN="sum")
meanOfTripDuration_2_aggr <- arrange(meanOfTripDuration_2_aggr, StationId, Hour)
meanOfTripDuration_2_aggr[1:6,]

dataset_aggr <- data.frame(bike_out_aggr$StationId,
                           bike_out_aggr$Hour,
                           bike_out_aggr$Bike_out,
                           bike_in_aggr$Bike_in,
                           meanOfTripDuration_1_aggr$MeanOfTripDuration_1,
                           meanOfTripDuration_2_aggr$MeanOfTripDuration_2)

colnames(dataset_aggr) = c("StationId","Hour","Bike_out","Bike_in",
                           "MeanOfTripDuration_1","MeanOfTripDuration_2")
dataset_aggr[1:6,]

rm(bike_out_aggr,bike_in_aggr,
   meanOfTripDuration_1_aggr,meanOfTripDuration_2_aggr)

library(reshape2)
## casting
dataset_cast1 = dcast(dataset_aggr, StationId ~ Hour, value.var="Bike_out")
colnames(dataset_cast1) = c("StationId","bikeout_0","bikeout_1","bikeout_2","bikeout_3","bikeout_4","bikeout_5","bikeout_6",
                            "bikeout_7","bikeout_8","bikeout_9","bikeout_10","bikeout_11","bikeout_12","bikeout_13",
                            "bikeout_14","bikeout_15","bikeout_16","bikeout_17","bikeout_18","bikeout_19","bikeout_20",
                            "bikeout_21","bikeout_22","bikeout_23")
dataset_cast1 <- arrange(dataset_cast1, StationId)
dataset_cast1[1,]

dataset_cast2 = dcast(dataset_aggr, StationId ~ Hour, value.var="Bike_in")
colnames(dataset_cast2) = c("StationId","bikein_0","bikein_1","bikein_2","bikein_3","bikein_4","bikein_5","bikein_6",
                            "bikein_7","bikein_8","bikein_9","bikein_10","bikein_11","bikein_12","bikein_13",
                            "bikein_14","bikein_15","bikein_16","bikein_17","bikein_18","bikein_19","bikein_20",
                            "bikein_21","bikein_22","bikein_23")
dataset_cast2 <- arrange(dataset_cast2, StationId)
dataset_cast2[1,]

dataset_cast3 = dcast(dataset_aggr, StationId ~ Hour, value.var="MeanOfTripDuration_1")
colnames(dataset_cast3) = c("StationId","tripduration1_0","tripduration1_1","tripduration1_2","tripduration1_3","tripduration1_4",
                            "tripduration1_5","tripduration1_6","tripduration1_7","tripduration1_8","tripduration1_9","tripduration1_10",
                            "tripduration1_11","tripduration1_12","tripduration1_13","tripduration1_14","tripduration1_15",
                            "tripduration1_16","tripduration1_17","tripduration1_18","tripduration1_19","tripduration1_20",
                            "tripduration1_21","tripduration1_22","tripduration1_23")
dataset_cast3 <- arrange(dataset_cast3, StationId)
dataset_cast3[1,]

dataset_cast4 = dcast(dataset_aggr, StationId ~ Hour, value.var="MeanOfTripDuration_2")
colnames(dataset_cast4) = c("StationId","tripduration2_0","tripduration2_1","tripduration2_2","tripduration2_3","tripduration2_4",
                            "tripduration2_5","tripduration2_6","tripduration2_7","tripduration2_8","tripduration2_9","tripduration2_10",
                            "tripduration2_11","tripduration2_12","tripduration2_13","tripduration2_14","tripduration2_15",
                            "tripduration2_16","tripduration2_17","tripduration2_18","tripduration2_19","tripduration2_20",
                            "tripduration2_21","tripduration2_22","tripduration2_23")
dataset_cast4 <- arrange(dataset_cast4, StationId)
dataset_cast4[1,]

## merging
dataset_merge = cbind(dataset_cast1,dataset_cast2[,-1],
                      dataset_cast3[,-1],dataset_cast4[,-1])
rm(dataset_cast1,dataset_cast2,
   dataset_cast3,dataset_cast4)
dim(dataset_merge)

dataset_merge[1,]

## load station information
dataset_station <- read.csv("F:/Program Files/RStudio/DM_Proj/HealthyRideStations2015.csv")
str(dataset_station)

## add three new variable "RackQnty", "Latitude", "Longtitude"
dataset_merge <- merge(dataset_merge, dataset_station[,c(1,3,4,5)],
                       by.x="StationId", by.y="StationNum")

## extract stationId
station <- as.character(dataset_merge$StationId)
## normalize each variable in the dataset
data_norm = scale(dataset_merge[-1])
rownames(data_norm) <- station
labels = rownames(data_norm)
labels

data_norm[1:3,]

## ------ k-means evaluation ------
clustering_result = matrix(0, nrow = 9, ncol = 50)
for (y in 2:10) {
  cluster_result = kmeans(data_norm, centers=y)
  clustering_result[y-1,] = cluster_result$cluster
}

colnames(clustering_result) = as.character(c(1000:1049))
rownames(clustering_result) = as.character(c(2:10))
clustering_result

## Dunn index & Silhouette index & Davies-Bouldin's index
library(fpc) # for cluster.stats (Dunn index & Silhouette index)
library(clusterSim) # for index.DB (Davies-Bouldin's index)

dmtx <- dist(data_norm) # build dist matrix before k mean cluster
cluster_compare = matrix(0, nrow = 3, ncol = 9)
row.names(cluster_compare) = c("Davies-Bouldin","Dunn","Silhouette")
colnames(cluster_compare) = c("k=2","k=3","k=4","k=5","k=6",
                              "k=7","k=8","k=9","k=10")

for (kk in 2:10) {
  cluster_result = kmeans(data_norm, centers=kk) # k mean cluster
  index_result = cluster.stats(dmtx, cluster_result$cluster) # generate dunn index
  DB_result = index.DB(data_norm, cluster_result$cluster, d=NULL, centrotypes="centroids", p=2, q=2)
  cluster_compare[1,kk-1] = DB_result$DB # Davies-Bouldin's value
  cluster_compare[2,kk-1] = index_result$dunn # Dunn value
  cluster_compare[3,kk-1] = index_result$avg.silwidth # Silhouette value
}
cluster_compare

## MDS scatterplot
data.mds = do.mds(data_norm, labels, do.scatter = T)

## k-means clustering
kcount = 2

clu = do.kmeans(data_norm, labels, k = kcount)$cluster
clu

## plot clustering results from kmeans
plot(data.mds, type = "n"
     # , main = 'MDS maps for the stations'
     )
text(data.mds, labels, col = rainbow(kcount)[clu])

clu.result <- data.frame(StationId=as.numeric(names(clu)),
                         Cluster=factor(clu, levels=1:2))
str(clu.result)
summary(clu.result)

## output the dataset for the map highlighting
## add three new variable "RackQnty", "Latitude", "Longtitude"
clu.result <- merge(clu.result, dataset_station[,c(1,3,4,5)],
                       by.x="StationId", by.y="StationNum")

write.csv(clu.result, file="cluster_station.csv")
