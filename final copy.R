library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)
library(corrplot)
library(gbm)
library(randomForest)
library(RColorBrewer)
library(rattle)
set.seed(1234)
train_model <- read.csv("D:/r survey/my new project/caret r ml/pml-training.csv")
test_model <- read.csv("D:/r survey/my new project/caret r ml/pml-training.csv")
train_Data <- train_model
test_Data <- test_model
train_Data <- train_model[, colSums(is.na(train_model)) == 0]
test_Data <- test_model[, colSums(is.na(test_model)) == 0]
classe <- train_Data$classe
train_garb <- grepl("^X|timestamp|window", names(train_Data))
train_Data <- train_Data[, !train_garb]
train_Clean <- train_Data[, sapply(train_Data, is.numeric)]
train_Clean$classe <- classe
test_garb <- grepl("^X|timestamp|window", names(test_Data))
test_Data <- test_Data[, !test_garb]
testCleaned <- test_Data[, sapply(test_Data, is.numeric)]

inTrain <- createDataPartition(train_Clean$classe, p=0.50, list=F)
trainData <- train_Clean[inTrain, ]
testData <- train_Clean[-inTrain, ]


treeModel <- rpart(classe ~ ., data=trainData, method="class")
prp(treeModel, box.palette = "auto", compress = TRUE, ycompress = TRUE)
rpart.plot(treeModel, type = 4, branch.lty = 1,box.palette = "auto", cex = .5)
predict_DT <- predict(treeModel, testData, type = "class")
DT_CM <- confusionMatrix(testData$classe, predict_DT)
DT_CM
accuracy_DT <- postResample(predict_DT, testData$classe)
accuracy_DT
plot(DT_CM$table, col = DT_CM$byClass, main = paste("CM for Decision Tree"))

controlRf <- trainControl(method="cv", 5)
model_Rforest <- train(classe ~ ., data=trainData, method="rf", trControl=controlRf, ntree=250)
model_Rforest
RFModel <- randomForest(classe ~ ., data=trainData)
predict_RF <- predict(RFModel, testData, type = "class")
RF_CM <- confusionMatrix(predict_RF, testData$classe)
RF_CM
plot(RF_CM$table, col = RF_CM$byClass, main = paste("CM for Random Forest"))
plot(RFModel)
Results <- predict(RFModel, newdata=train_Data)
Results
Results <- predict(RFModel, newdata=testData)
Results
