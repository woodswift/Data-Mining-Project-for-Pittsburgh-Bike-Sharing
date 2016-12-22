library(MASS) # for the example dataset 
library(plyr) # for recoding data
library(e1071) # for SVR
library(rpart) # for decision tree
library(reshape2) # for melting and casting

## set the seed so you can get exactly the same results whenever you run the code
set.seed(12345)

RMSE <- function(x, y){
  tmp <- (x-y)*(x-y)
  r <- sum(tmp)/length(x)
  r <- sqrt(r)
  return (r)
}

# train.set <- rscw_m_AR_clu_train
# test.set <- rscw_m_AR_clu_test

do.regression <- function(train.set, test.set, 
                          reg.name="lr", AR_type=1) {
  switch(reg.name, 
         lr={ ## linear regression
           if (AR_type==1){
             fit = lm(Bike_out ~ ., data=train.set[train.set$Y==1,-2]);
             # summary(fit)
            
             pred = predict(fit, newdat=test.set[test.set$cla==1,c(-1,-2,-46)]);
             
             index <- as.numeric(names(pred))
             
             ## add new variable "reg" into test dataset
             test.set$reg <- -1
             
             # if classfication "pred" indicates 0, then "reg" is 0
             test.set$reg[test.set$cla==0] <- 0
             
             for (i in 1:length(index)){
               j <- as.character(index[i])
               test.set$reg[which(rownames(test.set)==j)] <- pred[[i]]
             }
             # summary(test.set$reg)
             
             # add a new variable "Hour"
             test.set$Hour <- 0
             
             for (i in 15:37){
               index <- which(test.set[,i]==1)
               test.set$Hour[index] <- i-14
             }
             
             # test.set[1:12,c("Y","Bike_out","cla","reg")]
             
             cmp <- data.frame(Hour=test.set$Hour,
                               Y=test.set$Y,
                               cla=test.set$cla,
                               actual=test.set$Bike_out,
                               prediction=test.set$reg)
             
             error <- RMSE(cmp$actual,cmp$prediction)
             cat('\n')
             cat('RMSE:',error,'\n')
             
             return (cmp)
           }
           if (AR_type==2) {
             fit = lm(Bike_out ~ ., data=train.set[train.set$Y==1,-2]);
             # summary(fit)
             
             pred = predict(fit, newdat=test.set[test.set$cla==1,c(-1,-2,-58)]);
             
             index <- as.numeric(names(pred))
             
             ## add new variable "reg" into test dataset
             test.set$reg <- -1
             
             # if classfication "pred" indicates 0, then "reg" is 0
             test.set$reg[test.set$cla==0] <- 0
             
             for (i in 1:length(index)){
               j <- as.character(index[i])
               test.set$reg[which(rownames(test.set)==j)] <- pred[[i]]
             }
             # summary(test.set$reg)
             
             # add a new variable "Hour"
             test.set$Hour <- 0
             
             for (i in 27:49){
               index <- which(test.set[,i]==1)
               test.set$Hour[index] <- i-26
             }
             
             # test.set[1:12,c("Y","Bike_out","cla","reg")]
             
             cmp <- data.frame(Hour=test.set$Hour,
                               Y=test.set$Y,
                               cla=test.set$cla,
                               actual=test.set$Bike_out,
                               prediction=test.set$reg)
             
             error <- RMSE(cmp$actual,cmp$prediction)
             cat('\n')
             cat('RMSE:',error,'\n')
             
             return (cmp)
           }
         },
         dtree={ ## decision tree
           if (AR_type==1){
             # grow tree 
             fit <- rpart(Bike_out~., method="anova", 
                          data=train.set[train.set$Y==1,-2])
             # printcp(fit) # display the results 
             # plotcp(fit) # visualize cross-validation results 
             # summary(fit) # detailed summary of splits
             
             # create additional plots 
             # par(mfrow=c(1,2)) # two plots on one page 
             # rsq.rpart(fit) # visualize cross-validation results
             
             # plot tree 
             # plot(fit, uniform=TRUE, main="Regression Tree for Bike_out ")
             # text(fit, use.n=TRUE, all=TRUE, cex=.8)
             
             # prune the tree 
             pfit<- prune(fit, cp=0.01160389) # from cptable   
             
             # plot the pruned tree 
             # plot(pfit, uniform=TRUE, main="Pruned Regression Tree")
             # text(pfit, use.n=TRUE, all=TRUE, cex=.8)
             
             pred = predict(pfit, 
                            newdat=test.set[test.set$cla==1,c(-1,-2,-46)]);
             
             index <- as.numeric(names(pred))
             
             ## add new variable "reg" into test dataset
             test.set$reg <- -1
             # if classfication "pred" indicates 0, then "reg" is 0
             test.set$reg[test.set$cla==0] <- 0
             for (i in 1:length(index)){
               j <- as.character(index[i])
               test.set$reg[which(rownames(test.set)==j)] <- pred[[i]]
             }
             # summary(test.set$reg)
             
             # add a new variable "Hour"
             test.set$Hour <- 0
             
             for (i in 15:37){
               index <- which(test.set[,i]==1)
               test.set$Hour[index] <- i-14
             }
             
             # test.set[1:12,c("Y","Bike_out","cla","reg")]
             
             cmp <- data.frame(Hour=test.set$Hour,
                               Y=test.set$Y,
                               cla=test.set$cla,
                               actual=test.set$Bike_out,
                               prediction=test.set$reg)
             
             error <- RMSE(cmp$actual,cmp$prediction)
             cat('\n')
             cat('RMSE:',error,'\n')
             
             return (cmp)
           }
           if (AR_type==2) {
             # grow tree 
             fit <- rpart(Bike_out~., method="anova", 
                          data=train.set[train.set$Y==1,-2])
             # printcp(fit) # display the results 
             # plotcp(fit) # visualize cross-validation results 
             # summary(fit) # detailed summary of splits
             
             # create additional plots 
             # par(mfrow=c(1,2)) # two plots on one page 
             # rsq.rpart(fit) # visualize cross-validation results
             
             # plot tree 
             # plot(fit, uniform=TRUE, main="Regression Tree for Bike_out ")
             # text(fit, use.n=TRUE, all=TRUE, cex=.8)
             
             # prune the tree 
             pfit<- prune(fit, cp=0.01160389) # from cptable   
             
             # plot the pruned tree 
             # plot(pfit, uniform=TRUE, main="Pruned Regression Tree")
             # text(pfit, use.n=TRUE, all=TRUE, cex=.8)
             
             pred = predict(pfit, 
                            newdat=test.set[test.set$cla==1,c(-1,-2,-58)]);
             
             index <- as.numeric(names(pred))
             
             ## add new variable "reg" into test dataset
             test.set$reg <- -1
             # if classfication "pred" indicates 0, then "reg" is 0
             test.set$reg[test.set$cla==0] <- 0
             for (i in 1:length(index)){
               j <- as.character(index[i])
               test.set$reg[which(rownames(test.set)==j)] <- pred[[i]]
             }
             # summary(test.set$reg)
             
             # add a new variable "Hour"
             test.set$Hour <- 0
             
             for (i in 27:49){
               index <- which(test.set[,i]==1)
               test.set$Hour[index] <- i-26
             }
             
             # test.set[1:12,c("Y","Bike_out","cla","reg")]
             
             cmp <- data.frame(Hour=test.set$Hour,
                               Y=test.set$Y,
                               cla=test.set$cla,
                               actual=test.set$Bike_out,
                               prediction=test.set$reg)
             
             error <- RMSE(cmp$actual,cmp$prediction)
             cat('\n')
             cat('RMSE:',error,'\n')
             
             return (cmp)
           }
         },
         svr={ ## support vector regression
           if (AR_type==1){
             fit <- svm(Bike_out~., method="anova", 
                        data=train.set[train.set$Y==1,-2])
             pred = predict(fit, 
                            newdat=test.set[test.set$cla==1,c(-1,-2,-46)]);
             
             index <- as.numeric(names(pred))
             
             ## add new variable "reg" into test dataset
             test.set$reg <- -1
             # if classfication "pred" indicates 0, then "reg" is 0
             test.set$reg[test.set$cla==0] <- 0
             for (i in 1:length(index)){
               j <- as.character(index[i])
               test.set$reg[which(rownames(test.set)==j)] <- pred[[i]]
             }
             # summary(test.set$reg)
             
             # add a new variable "Hour"
             test.set$Hour <- 0
             
             for (i in 15:37){
               index <- which(test.set[,i]==1)
               test.set$Hour[index] <- i-14
             }
             
             # test.set[1:12,c("Y","Bike_out","cla","reg")]
             
             cmp <- data.frame(Hour=test.set$Hour,
                               Y=test.set$Y,
                               cla=test.set$cla,
                               actual=test.set$Bike_out,
                               prediction=test.set$reg)
             
             error <- RMSE(cmp$actual,cmp$prediction)
             cat('\n')
             cat('RMSE:',error,'\n')
             
             return (cmp)
           }
           if (AR_type==2) {
             fit <- svm(Bike_out~., method="anova", 
                        data=train.set[train.set$Y==1,-2])
             pred = predict(fit, 
                            newdat=test.set[test.set$cla==1,c(-1,-2,-58)]);
             
             index <- as.numeric(names(pred))
             
             ## add new variable "reg" into test dataset
             test.set$reg <- -1
             # if classfication "pred" indicates 0, then "reg" is 0
             test.set$reg[test.set$cla==0] <- 0
             for (i in 1:length(index)){
               j <- as.character(index[i])
               test.set$reg[which(rownames(test.set)==j)] <- pred[[i]]
             }
             # summary(test.set$reg)
             
             # add a new variable "Hour"
             test.set$Hour <- 0
             
             for (i in 27:49){
               index <- which(test.set[,i]==1)
               test.set$Hour[index] <- i-26
             }
             
             # test.set[1:12,c("Y","Bike_out","cla","reg")]
             
             cmp <- data.frame(Hour=test.set$Hour,
                               Y=test.set$Y,
                               cla=test.set$cla,
                               actual=test.set$Bike_out,
                               prediction=test.set$reg)
             
             error <- RMSE(cmp$actual,cmp$prediction)
             cat('\n')
             cat('RMSE:',error,'\n')
             
             return (cmp)
           }
         }
  ) 
}