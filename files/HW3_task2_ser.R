require(penalized)
require(e1071)
install.packages("caret", repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com",dependencies=TRUE)) 
require(caret)


train_data=read.table('C:/Users/07830/Desktop/ecgTRAIN')
test_data=read.table('C:/Users/07830/Desktop/ecgTEST')

trainclass=train_data[,1] # takes -1 and 1
trainclass[trainclass==-1]=0

testclass=test_data[,1] # takes -1 and 1
testclass[testclass==-1]=0

opt_lambda1 = optL1(trainclass, train_data[,2:ncol(train_data)],lambda2=0, fold=10)

model_lasso=penalized(trainclass, penalized= train_data[,2:ncol(train_data)],lambda1=2.123625 ,
           lambda2=0, positive = FALSE, data=train_data, fusedl=TRUE,
           model = "logistic",
           standardize = FALSE, trace = TRUE)
# str(pen)
# pen@fitted
# pen@penalized
# pen@converged
# pen@residuals
# pen@formula$unpenalized

test_predict= predict(model_lasso, test_data[,2:ncol(test_data)])
test_predict=round(test_predict)


rounded=round(pen@fitted)
unique(rounded)
all.equal(as.factor(trainclass),as.factor(rounded))

confusionMatrix(as.factor(test_predict), as.factor(testclass), dnn = c("Prediction", "Reference"))


#Part C
data.frame(train_data)
train_transformed=train_data[,3:97]
for(i in 3:ncol(train_data)) {
  train_transformed[,i-2]=train_data[,i]-train_data[,i-1]
}

test_transformed=test_data[,1:95]
for(i in 3:ncol(test_data)) {
  test_transformed[,i-2]=test_data[,i]-test_data[,i-1]
}

opt_lambda1_transformed = optL1(trainclass, train_transformed,lambda2=0, fold=10)

model_lasso_transformed=penalized(trainclass, penalized= train_transformed,lambda1=1.010164 ,
              lambda2=0, positive = FALSE, data=train_transformed, fusedl=TRUE,
              model = "logistic",
              standardize = FALSE, trace = TRUE)

test_predict_transformed= predict(model_lasso_transformed, test_transformed)
test_predict_transformed=round(test_predict_transformed)

confusionMatrix(as.factor(test_predict_transformed), as.factor(testclass), dnn = c("Prediction", "Reference"))
