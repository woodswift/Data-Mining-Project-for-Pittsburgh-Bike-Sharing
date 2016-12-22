## ------ load training an test dataset ------
rscw_m_AR_train <- read.csv("rscw_m1_AR1_c2_train.csv")
rscw_m_AR_test <- read.csv("rscw_m1_AR1_c2_test.csv")

## set cluster
clu <- 1

rscw_m_AR_clu_train <- rscw_m_AR_train[rscw_m_AR_train$Cluster==clu,-2]
rscw_m_AR_clu_test <- rscw_m_AR_test[rscw_m_AR_test$Cluster==clu,-2]

## ------ parameter setting ------
## choose the lag
AR <- 1
## choose classification method
cl.method <- 'lr'
## choose classification cutoff
cutoff <- 0.2
## choose regression method
reg.method <- 'svr'

## ------ slight pre-processing and data-type casting ------
rscw_m_AR_clu_train$X <- NULL # remove index variable "X"
rscw_m_AR_clu_train$Y <- factor(rscw_m_AR_clu_train$Y, level=0:1)
str(rscw_m_AR_clu_train)
summary(rscw_m_AR_clu_train)

rscw_m_AR_clu_test$X <- NULL # remove index variable "X"
rscw_m_AR_clu_test$Y <- factor(rscw_m_AR_clu_test$Y, level=0:1)

names(rscw_m_AR_clu_train[,-1])

## if month == 1,2,3
## remove unknown_1 and unknown_2
rscw_m_AR_clu_train$Unknown_1 <- NULL
rscw_m_AR_clu_test$Unknown_1 <- NULL

rscw_m_AR_clu_train$Unknown_2 <- NULL
rscw_m_AR_clu_test$Unknown_2 <- NULL

## ------ classification stage ------
## classification method cross validation
# my.classifier(rscw_m_AR_clu_train[,-1], 
#               cl.name=cl.method, do.cv=T)

## run classification
prob = do.classification(rscw_m_AR_clu_train[,-1], 
                         rscw_m_AR_clu_test[,-1], 
                         cl.method)

## get confusion matrix
predicted = as.numeric(prob > cutoff)
actual = rscw_m_AR_clu_test$Y
confusion.matrix = table(actual,factor(predicted,levels=c(0,1)))
confusion.matrix

## run classification plus
# predicted = do.classification.plus(rscw_m_AR_clu_train[,-1], rscw_m_AR_clu_test[,-1], "rbf")

## calculate the specificity
specificity = confusion.matrix[2,2]/sum(confusion.matrix[2,])
cat('Specificity:',specificity,'\n')

## add "predicted" naming "cla" into test dataset
rscw_m_AR_clu_test <- data.frame(rscw_m_AR_clu_test, cla=predicted)
# set factor level
rscw_m_AR_clu_test$cla <- factor(rscw_m_AR_clu_test$cla, levels=0:1)

str(rscw_m_AR_clu_train)
summary(rscw_m_AR_clu_train)

str(rscw_m_AR_clu_test)
summary(rscw_m_AR_clu_test)

names(rscw_m_AR_clu_train)
names(rscw_m_AR_clu_test)

## ------ regression stage ------

## run regression
reg.result <- do.regression(rscw_m_AR_clu_train, rscw_m_AR_clu_test,
                            reg.name=reg.method, AR_type=AR)

ceiling_err <- RMSE(reg.result$actual,ceiling(reg.result$prediction))
cat('\n')
cat('RMSE after ceiling:',ceiling_err,'\n')

floor_err <- RMSE(reg.result$actual,floor(reg.result$prediction))
cat('\n')
cat('RMSE after flooring:',floor_err,'\n')

round_err <- RMSE(reg.result$actual,round(reg.result$prediction))
cat('\n')
cat('RMSE after rounding:',round_err,'\n')

## ------ fitting result visualiztion ------
library(reshape2)
cmp.m <- melt(reg.result[,c("Hour","actual","prediction")], 
              id.vars = "Hour")

# scatterplot of bike_out across hour
library(ggplot2) # for ploting
theme_set(theme_bw())
ggplot(cmp.m, aes(Hour, value, colour = variable)) +
  geom_point(shape=1, alpha = 0.3) +
  theme(text = element_text(size=20))

library(plyr)
# Bike_out sum group by Hour, variable
out_hour <- aggregate(value ~ Hour + variable, data=cmp.m, FUN = "sum")

names(out_hour) <- c("Hour", "Result", "The_sum_of_bikes_rented")

# scatterplot of sum(bike_out) across hour
library(ggplot2) # for ploting
theme_set(theme_bw())
ggplot(data=out_hour, aes(x=Hour, y=The_sum_of_bikes_rented, color=Result, 
                          group=Result)) + 
  geom_point(shape=1, alpha = 0.3) + 
  geom_line() + 
  theme(text = element_text(size=20))