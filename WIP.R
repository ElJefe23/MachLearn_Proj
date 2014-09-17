## Change to the working directory
setwd("~/GitHub/MachLearn_Proj")
require(ada)
require(caret)
require(rpart)
require(gbm)
require(ipred)
require(randomForest)
trainDat<-read.csv("pml-training.csv")
testDat<-read.csv("pml-testing.csv")
## Delete columns with NAs
filteredTrain<-trainDat[,((colSums(is.na(trainDat))==0) & (colSums(trainDat=="")==0))]
filteredTest<-testDat[,((colSums(is.na(testDat))==0) & (colSums(testDat=="")==0))]
## Take away the variables related to username and time; these are non-sensical in the final
## model.
parsedTrain<-filteredTrain[,-c(1:7)]
parsedTest<-filteredTest[,-c(1:7)]
## Split the training data into two sets to perform validation.
randomSelect<-createDataPartition(parsedTrain$classe, p=0.7, list=FALSE)
myTrain<-parsedTrain[randomSelect,]
myVal<-parsedTrain[-randomSelect,]
## A couple of short classification tree algorithms.
treeModel<-train(classe~.,data=myTrain,method="rpart")
## The other algorithm.
treeModel2<-train(classe~.,data=myTrain,method="rpart2")
#These probably suck.

## How about a random forest model?
treeModelRF<-randomForest(classe~.,data=myTrain,ntree=100)
treeModelRF2<-randomForest(classe~.,data=myTrain,ntree=200)
treeModelRF4<-randomForest(classe~.,data=myTrain,ntree=400)
## Which are the important variables?
varImpPlot(treeModelRF, sort=TRUE, n.var=15, main="Random Forest - 100 trees")
varImpPlot(treeModelRF2, sort=TRUE, n.var=15, main="Random Forest - 200 trees")
varImpPlot(treeModelRF4, sort=TRUE, n.var=15, main="Random Forest - 400 trees")

## We need to figure out how to present the confusion matriz results more succinctly.
RFMatrix<-confusionMatrix(predict(treeModelRF,myVal), myVal$classe)
RF2Matrix<-confusionMatrix(predict(treeModelRF2,myVal), myVal$classe)
RF4Matrix<-confusionMatrix(predict(treeModelRF4,myVal), myVal$classe)
