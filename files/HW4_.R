require(penalized)
library(ISLR)
library(glmnet)
library(dplyr)
library(tidyr)
install.packages('caret', dependencies=T)
library(caret)

data=read.csv(file="C:/Users/07830/Desktop/train_data.csv", header = TRUE, sep = ",")
data= as.data.frame(data)

smp_size = floor(0.70*nrow(data))

set.seed(123)
train_ind=sample(seq_len(nrow(data)), size = smp_size)

train= data[train_ind,]
test= data[-train_ind,]

trainclass=train[,28]
traindata=train[,2:27]
testclass=test[,28]
testdata=test[,2:27]
a=mean(testclass)
b=mean(trainclass)

d =cbind(trainclass,traindata)
model_pra <- train(trainclass~., data =d,  method = "rqlasso",
                  trControl = trainControl("cv", number = 10))

model_pra$bestTune
predictions_pra_test = predict(model_pra,testdata)
MSE_test_pra = sqrt(mean(predictions_pra_test - testclass)^2)

predictions_pra_train = predict(model_pra,traindata)
MSE_train_pra = sqrt(mean(predictions_pra_train - trainclass)^2)

# DECISION TREE

model_dt <- train(trainclass~., data =d,  method = "rpart",
                  trControl = trainControl("cv", number = 10))

model_dt$bestTune
predictions_dt_test = predict(model_dt,testdata)
MSE_test_dt = sqrt(mean(predictions_dt_test - testclass)^2)

predictions_dt_train = predict(model_dt,traindata)
MSE_train_dt = sqrt(mean(predictions_dt_train - trainclass)^2)

# SVM

install.packages("e1071")
library("e1071")

# Radial 
model_svm_radial <- train(trainclass~., data =d,  method = "svmRadial",
                   trControl = trainControl("cv", number = 10))

model_svm_radial$bestTune
predictions_svm_radial_test = predict(model_svm_radial,testdata)
MSE_test_svm_radial = sqrt(mean(predictions_svm_radial_test - testclass)^2)

predictions_svm_radial_train = predict(model_svm_radial,traindata)
MSE_train_svm_radial = sqrt(mean(predictions_svm_radial_train - trainclass)^2)

# Polynomial
model_svm_poly <- train(trainclass~., data =d,  method = "svmPoly",
                    trControl = trainControl("cv", number = 10))

model_svm_poly$bestTune
predictions_svm_poly_test = predict(model_svm_poly,testdata)
MSE_test_svm_poly = sqrt(mean(predictions_svm_poly_test - testclass)^2)

predictions_svm_poly_train = predict(model_svm_poly,traindata)
MSE_train_svm_poly = sqrt(mean(predictions_svm_poly_train - trainclass)^2)

#RANDOM FOREST

model_rf <- train(trainclass~., data =d,  method = "rf",
               trControl = trainControl("cv", number = 10))

model_rf$bestTune
predictions_rf_test = predict(model_rf,testdata)
MSE_test_rf = sqrt(mean(predictions_rf_test - testclass)^2)

predictions_rf_train = predict(model_rf,traindata)
MSE_train_rf = sqrt(mean(predictions_rf_train - trainclass)^2)


# XGBOOST


model_xgb <- train(trainclass~., data =d,  method = "xgbTree",
  trControl = trainControl("cv", number = 10)
)
model_xgb$bestTune
predictions_xgb_test = predict(model_xgb,testdata)
MSE_test_xgb = sqrt(mean(predictions_xgb_test - testclass)^2)

predictions_xgb_train = predict(model_xgb,traindata)
MSE_train_xgb = sqrt(mean(predictions_xgb_train - trainclass)^2)
