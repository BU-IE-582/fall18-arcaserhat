
data=read.csv(file="C:/Users/07830/Desktop/german.data-numeric.csv", header = TRUE, sep = ";")
data= as.data.frame(data)


data[,1] = as.factor(data[,1])
data[,3] = as.factor(data[,3])
data[,5] = as.factor(data[,5])
data[,6] = as.factor(data[,6])
data[,7] = as.factor(data[,7])
data[,8] = as.factor(data[,8])
data[,9] = as.factor(data[,9])
data[,11] = as.factor(data[,11])
data[,12] = as.factor(data[,12])
data[,13] = as.factor(data[,13])
data[,14] = as.factor(data[,14])
data[,15] = as.factor(data[,15])
data[,16] = as.factor(data[,16])
data[,17] = as.factor(data[,17])
data[,18] = as.factor(data[,18])
data[,19] = as.factor(data[,19])
data[,20] = as.factor(data[,20])
data[,21] = as.factor(data[,21])

smp_size = floor(0.70*nrow(data))

set.seed(123)
train_ind=sample(seq_len(nrow(data)), size = smp_size)

train= data[train_ind,]
test= data[-train_ind,]

trainclass=train[,1]
traindata=train[,2:21]
testclass=test[,1]
testdata=test[,2:21]

#install.packages('caret', dependencies=T)
require(caret)

d =cbind(trainclass,traindata)

# LASSO

require(mda)
model_pra <- train(trainclass~., data =d,  method = "pda", metric = "Accuracy", maximize = TRUE,  
                   trControl = trainControl("cv", number = 10))

model_pra$bestTune
predictions_pra_test = predict(model_pra,testdata)

tbl_pra_test=table(predictions_pra_test, testclass)
acc_pra_test=sum(diag(tbl_pra_test))/length(testclass)

predictions_pra_train = predict(model_pra,traindata)

tbl_pra_train=table(predictions_pra_train, trainclass)
acc_pra_train=sum(diag(tbl_pra_train))/length(trainclass)

# DECISION TREE

model_dt <- train(trainclass~., data =d,  method = "rpart",
                  trControl = trainControl("cv", number = 10))

model_dt$bestTune
predictions_dt_test = predict(model_dt,testdata)
tbl_dt_test=table(predictions_dt_test, testclass)
acc_dt_test=sum(diag(tbl_dt_test))/length(testclass)

predictions_dt_train = predict(model_dt,traindata)
tbl_dt_train=table(predictions_dt_train, trainclass)
acc_dt_train=sum(diag(tbl_dt_train))/length(trainclass)


# SVM

# Radial 
model_svm_radial <- train(trainclass~., data =d,  method = "svmRadial",
                    trControl = trainControl("cv", number = 10))

model_svm_radial$bestTune
predictions_svm_radial_test = predict(model_svm_radial,testdata)


tbl_svm_radial_test=table(predictions_svm_radial_test, testclass)
acc_svm_radial_test=sum(diag(tbl_svm_radial_test))/length(testclass)

predictions_svm_radial_train = predict(model_svm_radial,traindata)
tbl_svm_radial_train=table(predictions_svm_radial_train, trainclass)
acc_svm_radial_train=sum(diag(tbl_svm_radial_train))/length(trainclass)

# Polynomial
model_svm_poly <- train(trainclass~., data =d,  method = "svmPoly",
                    trControl = trainControl("cv", number = 10))

model_svm_poly$bestTune
predictions_svm_poly_test = predict(model_svm_poly,testdata)

tbl_svm_poly_test=table(predictions_svm_poly_test, testclass)
acc_svm_poly_test=sum(diag(tbl_svm_poly_test))/length(testclass)

predictions_svm_poly_train = predict(model_svm_poly,traindata)
tbl_svm_poly_train=table(predictions_svm_poly_train, trainclass)
acc_svm_poly_train=sum(diag(tbl_svm_poly_train))/length(trainclass)


#RANDOM FOREST
model_rf <- train(trainclass~., data =d,  method = "rf",
                  trControl = trainControl("cv", number = 10))

model_rf$bestTune
predictions_rf_test = predict(model_rf,testdata)
tbl_rf_test=table(predictions_rf_test, testclass)
acc_rf_test=sum(diag(tbl_rf_test))/length(testclass)

predictions_rf_train = predict(model_rf,traindata)
tbl_rf_train=table(predictions_rf_train, trainclass)
acc_rf_train=sum(diag(tbl_rf_train))/length(trainclass)


# XGBOOST

model_xgb <- train(trainclass~., data =d,  method = "xgbTree",
               trControl = trainControl("cv", number = 10)
)
model_xgb$bestTune


predictions_xgb_test = predict(model_xgb,testdata)
tbl_xgb_test=table(predictions_xgb_test, testclass)
acc_xgb_test=sum(diag(tbl_xgb_test))/length(testclass)

predictions_xgb_train = predict(model_xgb,traindata)
tbl_xgb_train=table(predictions_xgb_train, trainclass)
acc_xgb_train=sum(diag(tbl_xgb_train))/length(trainclass)
