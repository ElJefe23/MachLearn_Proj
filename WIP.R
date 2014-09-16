## Change to the working directory
setwd("~/GitHub/MachLearn_Proj")
require(ada)
require(caret)
require(rpart)
require(gbm)
require(ipred)
trainDat<-read.csv("pml-training.csv")
## Delete columns with NAs
filteredTrain<-trainDat[,(colSums(is.na(trainDat))==0)]
## Take away the variables related to username and time; these are non-sensical in the final
## model.
secFilterDat<-filteredTrain[,-c(1:7)]
## Need to exclude columns with missing data, as well.
tertFilterDat<-secFilterDat[,-c(5:13,36:41,45:53,67:75)]
## A couple of short classification tree algorithms.
treeModel<-train(classe~.,data=tertFilterDat,method="rpart")
confusionMatrix(predict(treeModel,tertFilterDat),tertFilterDat$classe)
## The other algorithm.
treeModel2<-train(classe~.,data=tertFilterDat,method="rpart2")
confusionMatrix(predict(treeModel2,tertFilterDat),tertFilterDat$classe)
## Why not one more with a larger explicit depth.
treeModel3<-train(classe~.,data=tertFilterDat,method="rpart2",maxdepth=8)
## Didn't do anything different.
## How about a bagging model?
treeModel4<-train(classe~.,data=tertFilterDat,method="treebag")
## This is damn near perfect.  So ... it takes a few hours (~2) but it definitely works.
## I'm sure the predictions will be perfect as well.
## But ... how do I get more information from the model?