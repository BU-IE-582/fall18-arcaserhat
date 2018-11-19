require(distances)
require(FNN)
require(glmnet)
require(TunePareto)
require(class)
require(ggplot2)
require(caret)
require(tictoc)

x_train=read.table('C:/Users/07830/Desktop/HW3_1a/UWave/uWaveGestureLibrary_X_TRAIN')
y_train=read.table('C:/Users/07830/Desktop/HW3_1a/UWave/uWaveGestureLibrary_Y_TRAIN')
z_train=read.table('C:/Users/07830/Desktop/HW3_1a/UWave/uWaveGestureLibrary_Z_TRAIN')

x_test=read.table('C:/Users/07830/Desktop/HW3_1a/UWave/uWaveGestureLibrary_X_TEST')
y_test=read.table('C:/Users/07830/Desktop/HW3_1a/UWave/uWaveGestureLibrary_Y_TEST')
z_test=read.table('C:/Users/07830/Desktop/HW3_1a/UWave/uWaveGestureLibrary_Z_TEST')

combined_train=cbind(x_train[,1:316],y_train[,2:316],z_train[,2:316])
combined_test=cbind(x_test[,1:316],y_test[,2:316],z_test[,2:316])
combined=rbind(combined_train,combined_test)

traindata = as.matrix(combined_train)
traindata=combined_train[,2:ncol(combined_train)]
trainclass=as.factor(combined_train[,1])

#10-fold cross-validation

k_levels=c(1,2,3,4,5,10,20,30,50)
thresHold=0.5
nofReplications=1
nFolds=10
indices=generateCVRuns(trainclass,nofReplications,nFolds,stratified=TRUE)

require(data.table)
cvresult=data.table()
for(i in 1:nofReplications) {
  thisReplication=indices[[i]]
  for(j in 1:nFolds){
    testindices=thisReplication[[j]]
    
    cvtrain=traindata[-testindices,]        
    cvtest=traindata[testindices,]
    
    cvtrain=data.frame(Class=trainclass[-testindices],cvtrain)
    logreg=glm(Class~.,cvtrain,family='binomial')
    pred_logreg=predict(logreg,data.frame(cvtest),type='response')
    
    if(nrow(cvresult)==0){
      cvresult=data.table(Replication=i,Fold=j,Method='Logistic',Klev=NA,TestId=testindices,
                          Predictions=as.numeric(pred_logreg>thresHold),Real=trainclass[testindices])
    } else {
      cvresult=rbind(cvresult,data.table(Replication=i,Fold=j,Method='Logistic',Klev=NA,TestId=testindices,
                                         Predictions=as.numeric(pred_logreg>thresHold),Real=trainclass[testindices]))
    }
    cvtrain=traindata[-testindices,]        
    cvtest=traindata[testindices,]
    
    for(y in 1:length(k_levels)){
      param_k=k_levels[y]
      predict_knn=knn(cvtrain, cvtest,trainclass[-testindices], k = param_k)
      cvresult=rbind(cvresult,data.table(Replication=i,Fold=j,Method='knn',Klev=param_k,TestId=testindices,
                                         Predictions=as.numeric(as.character(predict_knn)),Real=trainclass[testindices]))
    }   
  }    
}


cvresult[,list(Accu=mean(Predictions==Real)),by=list(Method,Klev)]

tic()
pred_euclidean=knn(combined[1:896,],combined[897:4478,],combined[1:896,1],k=3)
table_euclidean=table(pred_euclidean,combined[897:4478,1])
toc()

accuracy_euclidean=sum(diag(table_euclidean))/nrow(x_test)

#NN for Manhattan distance

require(kknn)
require(matrixStats)

tic()
pred_manhattan= kknn(formula = combined_train[1:896,1]~., combined_train[,2:ncol(combined_train)], combined_test[,2:ncol(combined_test)], k = 3, distance = 1)
manhattan=rowMedians(pred_manhattan$CL) 
table_manhattan=table(manhattan,combined[897:4478,1])
toc()

accuracy_manhattan=sum(diag(table_euclidean))/nrow(x_test)
