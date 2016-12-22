rscw_m9_AR1_train <- read.csv("rscw_m9_AR1_train.csv")
rscw_m9_AR1_test <- read.csv("rscw_m9_AR1_test.csv")

rscw_m9_AR2_train <- read.csv("rscw_m9_AR2_train.csv")
rscw_m9_AR2_test <- read.csv("rscw_m9_AR2_test.csv")


## ------ AR1 ------
rscw_m9_AR1_train$X <- NULL # remove index variable "X"
rscw_m9_AR1_train$Y <- factor(rscw_m9_AR1_train$Y, level=0:1)
str(rscw_m9_AR1_train)
summary(rscw_m9_AR1_train)

rscw_m9_AR1_test$X <- NULL # remove index variable "X"
rscw_m9_AR1_test$Y <- factor(rscw_m9_AR1_test$Y, level=0:1)

names(rscw_m9_AR1_train[,-1])

## classification method
cl.method <- 'lr'
my.classifier(rscw_m9_AR1_train[,-1], cl.name=cl.method, do.cv=T)

## run classification
prob = do.classification(rscw_m9_AR1_train[,-1], rscw_m9_AR1_test[,-1], cl.method)

## get confusion matrix
cutoff=0.2
predicted = as.numeric(prob > cutoff)
actual = rscw_m9_AR1_test$Y
confusion.matrix = table(actual,factor(predicted,levels=c(0,1)))
confusion.matrix

## calculate the specificity
specificity = confusion.matrix[2,2]/sum(confusion.matrix[2,])
cat('Specificity:',specificity,'\n')

## add "predicted" naming "cla" into test dataset
rscw_m9_AR1_test <- data.frame(rscw_m9_AR1_test, cla=predicted)
# set factor level
rscw_m9_AR1_test$cla <- factor(rscw_m9_AR1_test$cla, levels=0:1)

str(rscw_m9_AR1_test)
summary(rscw_m9_AR1_test)

names(rscw_m9_AR1_train)
names(rscw_m9_AR1_test)
names(rscw_m9_AR1_train[rscw_m9_AR1_train$Y==1,-2])
names(rscw_m9_AR1_test[rscw_m9_AR1_test$cla==1,c(-2,-46)])

## run regression
reg.method <- 'lr'
reg.result <- do.regression(rscw_m9_AR1_train,rscw_m9_AR1_test,
                            reg.name=reg.method, AR_type=1)

library(reshape2)
cmp.m <- melt(reg.result[,c("Hour","actual","prediction")], 
              id.vars = "Hour")

# scatterplot
library(ggplot2) # for ploting
ggplot(cmp.m, aes(Hour, value, colour = variable)) +
  geom_point(shape=1, alpha = 0.3) +
  theme(text = element_text(size=20))

library(plyr)
# Bike_out sum group by Hour, variable
out_hour <- aggregate(value ~ Hour + variable, data=cmp.m, FUN = "sum")

# scatterplot
library(ggplot2) # for ploting
ggplot(data=out_hour, aes(x=Hour, y=value, color=variable, 
                          group=variable)) + 
  geom_point(shape=1, alpha = 0.3) + 
  geom_line() + 
  theme(text = element_text(size=20))


## ------ AR2 ------
rscw_m9_AR2_train$X <- NULL # remove index variable "X"
rscw_m9_AR2_train$Y <- factor(rscw_m9_AR2_train$Y, level=0:1)
str(rscw_m9_AR2_train)
summary(rscw_m9_AR2_train)

rscw_m9_AR2_test$X <- NULL # remove index variable "X"
rscw_m9_AR2_test$Y <- factor(rscw_m9_AR2_test$Y, level=0:1)

names(rscw_m9_AR2_train[,-1])

## classification method
cl.method <- 'lr'
my.classifier(rscw_m9_AR2_train[,-1], cl.name=cl.method, do.cv=T)

## run classification
prob = do.classification(rscw_m9_AR2_train[,-1], rscw_m9_AR2_test[,-1], cl.method)

## get confusion matrix
cutoff=0.2
predicted = as.numeric(prob > cutoff)
actual = rscw_m9_AR2_test$Y
confusion.matrix = table(actual,factor(predicted,levels=c(0,1)))
confusion.matrix

## calculate the specificity
specificity = confusion.matrix[2,2]/sum(confusion.matrix[2,])
cat('Specificity:',specificity,'\n')

## add "predicted" naming "cla" into test dataset
rscw_m9_AR2_test <- data.frame(rscw_m9_AR2_test, cla=predicted)
# set factor level
rscw_m9_AR2_test$cla <- factor(rscw_m9_AR2_test$cla, levels=0:1)

str(rscw_m9_AR2_test)
summary(rscw_m9_AR2_test)

names(rscw_m9_AR2_train)
names(rscw_m9_AR2_test)
names(rscw_m9_AR2_train[rscw_m9_AR1_train$Y==1,-2])
names(rscw_m9_AR1_test[rscw_m9_AR1_test$cla==1,c(-2,-58)])

## run regression
reg.method <- 'lr'
reg.result <- do.regression(rscw_m9_AR2_train,rscw_m9_AR2_test,
                            reg.name=reg.method, AR_type=2)

library(reshape2)
cmp.m <- melt(reg.result[,c("Hour","actual","prediction")], 
              id.vars = "Hour")

# scatterplot
library(ggplot2) # for ploting
ggplot(cmp.m, aes(Hour, value, colour = variable)) +
  geom_point(shape=1, alpha = 0.3) +
  theme(text = element_text(size=20))

library(plyr)
# Bike_out sum group by Hour, variable
out_hour <- aggregate(value ~ Hour + variable, data=cmp.m, FUN = "sum")

# scatterplot
library(ggplot2) # for ploting
ggplot(data=out_hour, aes(x=Hour, y=value, color=variable, 
                          group=variable)) + 
  geom_point(shape=1, alpha = 0.3) + 
  geom_line() + 
  theme(text = element_text(size=20))