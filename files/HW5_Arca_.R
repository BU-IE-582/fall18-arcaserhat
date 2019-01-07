require(stats)
require(TSdist)
require(cluster)
require(ROCR)
library(caret)

set.seed(123)

data=read.csv(file="C:/Users/07830/Desktop/Musk1.csv", header = FALSE, sep = ",")

d=c(3,6,9,12,15,18,21,25,30,40)
acc_Euc=array()
acc_Manhat=array()
acc_Euc_hier=array()
acc_Manhat_hier=array()

# function to find medoid in cluster i
clust.centroid = function(z, dat, clusters) {
  ind = (clusters == z)
  colMeans(dat[ind,])
}

for (i in 1:length(d)) {
  
k=d[i]
distEuc=data.frame()
distManhat=data.frame()
distEuc_hier=data.frame()
distManhat_hier=data.frame()

n=nrow(data)

# Euclidean  - kmeans

cluster_Euc=pam(data[,3:length(data)],k,metric = "euclidean")
center_Euc=cluster_Euc$medoids
#clusters=cluster$cluster

# Manhattan - kmeans

cluster_Manhat=pam(data[,3:length(data)],k,metric = "manhattan")
center_Manhat=cluster_Manhat$medoids

# Euclidean  - hierarchical
cluster_Euc_hier = hclust(dist(data[,3:length(data)], method = "euclidean"), method = "average")
cluster_Euc_h <- cutree(cluster_Euc_hier, k = k)

hier_center_Euc=NULL

for (s in 1:k) {
  hier_center_Euc <- rbind(hier_center_Euc, colMeans(data[,3:length(data)][cluster_Euc_h == s, , drop = FALSE]))
}

# Manhattan  - hierarchical
cluster_Manhat_hier = hclust(dist(data[,3:length(data)], method = "manhattan"), method = "average")
clust_Manhat_h <- cutree(cluster_Manhat_hier, k = k)

hier_center_Manhat=NULL

for (s in 1:k) {
  hier_center_Manhat <- rbind(hier_center_Manhat, colMeans(data[,3:length(data)][clust_Manhat_h == s, , drop = FALSE]))
}


for (j in 1:n) {
    for (p in 1:k){
      distEuc[j,p]=EuclideanDistance(as.double(data[j,3:length(data)]), as.vector(center_Euc[p,]))
      distManhat[j,p]=ManhattanDistance(as.double(data[j,3:length(data)]), as.vector(center_Manhat[p,]))
      distEuc_hier[j,p]=EuclideanDistance(as.double(data[j,3:length(data)]), as.vector(hier_center_Euc[p,]))
      distManhat_hier[j,p]=ManhattanDistance(as.double(data[j,3:length(data)]), as.vector(hier_center_Manhat[p,]))
         
  }}


dat_Euc = cbind(data[,1:2], distEuc)
dat_Manhat = cbind(data[,1:2], distManhat)
dat_Euc_hier = cbind(data[,1:2], distEuc_hier)
dat_Manhat_hier = cbind(data[,1:2], distManhat_hier)


dat_Euc_2= aggregate(dat_Euc, by=list(dat_Euc[,2]), mean)
dat_Manhat_2= aggregate(dat_Manhat, by=list(dat_Manhat[,2]), mean)
dat_Euc_2_hier= aggregate(dat_Euc_hier, by=list(dat_Euc_hier[,2]), mean)
dat_Manhat_2_hier= aggregate(dat_Manhat_hier, by=list(dat_Manhat_hier[,2]), mean)

# Lasso regression
dat_Euc_2[,c(1,3)]=NULL
dat_Manhat_2[,c(1,3)]=NULL
dat_Euc_2_hier[,c(1,3)]=NULL
dat_Manhat_2_hier[,c(1,3)]=NULL
# 
class_Euc=as.factor(dat_Euc_2[,1])
class_Manhat=as.factor(dat_Manhat_2[,1])
class_Euc_hier=as.factor(dat_Euc_2_hier[,1])
class_Manhat_hier=as.factor(dat_Manhat_2_hier[,1])

m=k+1
colnames(dat_Euc_2) <- c(paste0("v", 1:m))
colnames(dat_Manhat_2) <- c(paste0("v", 1:m))
colnames(dat_Euc_2_hier) <- c(paste0("v", 1:m))
colnames(dat_Manhat_2_hier) <- c(paste0("v", 1:m))

# Euclidean  - kmeans - accuracy 
model_Euc = train(as.factor(v1)~.,data=dat_Euc_2, method="pda", metric="Accuracy", maximize = TRUE,
             trControl=trainControl("cv",number=10))

predicted_Euc = predict(model_Euc, dat_Euc_2)
tbl_Euc=table(predicted_Euc, class_Euc)
acc_Euc[i]=sum(diag(tbl_Euc))/length(class_Euc)

# Manhattan - kmeans - accuracy 
model_Manhat = train(as.factor(v1)~.,data=dat_Manhat_2, method="pda", metric="Accuracy", maximize = TRUE,
                trControl=trainControl("cv",number=10))

predicted_Manhat = predict(model_Manhat, dat_Manhat_2)
tbl_Manhat=table(predicted_Manhat, class_Manhat)
acc_Manhat[i]=sum(diag(tbl_Manhat))/length(class_Manhat)

# Euclidean  - hierarchical - accuracy 
model_Euc_hier = train(as.factor(v1)~.,data=dat_Euc_2_hier, method="pda", metric="Accuracy", maximize = TRUE,
                trControl=trainControl("cv",number=10))

predicted_Euc_hier = predict(model_Euc_hier, dat_Euc_2_hier)
tbl_Euc_hier=table(predicted_Euc_hier, class_Euc_hier)
acc_Euc_hier[i]=sum(diag(tbl_Euc_hier))/length(class_Euc_hier)


# Manhattan  - hierarchical - accuracy 
model_Manhat_hier = train(as.factor(v1)~.,data=dat_Manhat_2_hier, method="pda", metric="Accuracy", maximize = TRUE,
                  trControl=trainControl("cv",number=10))

predicted_Manhat_hier = predict(model_Manhat_hier, dat_Manhat_2_hier)
tbl_Manhat_hier=table(predicted_Manhat_hier, class_Manhat_hier)
acc_Manhat_hier[i]=sum(diag(tbl_Manhat_hier))/length(class_Manhat_hier)
}

# Plots
plot(d, acc_Euc, col="gray", type="b", ylab="Accuracy", xlab="k",
     xlim=c(0, 40), ylim=c(0.60, 1))
par(new=TRUE)
lines(d,acc_Manhat, ylab="Accuracy", xlab="k",type = "b",
     xlim=c(0, 40), ylim=c(0.60, 1))
par(new=TRUE)
lines(d, acc_Euc_hier, col="orange", ylab="Accuracy", xlab="k",type="b",
     xlim=c(0, 40), ylim=c(0.60, 1))
par(new=TRUE)
lines(d, acc_Manhat_hier, col="blue", ylab="Accuracy", xlab="k",type="b",
     xlim=c(0, 40), ylim=c(0.60, 1))
legend("bottomright", legend=c("k-means/Eucledian", "k-means/Manhattan", "hierarchical clust/Eucledian", "hierarchical clust/Manattan"),
       col=c("gray", "black", "orange","blue"), lty=1:2, cex=0.7)


# kmeans, k=21 with Manhattan distance
k= 21
m=k+1

cluster_final=pam(data[,3:length(data)],k,metric = "manhattan")
center_final=cluster_final$medoids

dist_final=data.frame()

for (t in 1:n) {
  for (g in 1:k){
    dist_final[t,g]=ManhattanDistance(as.double(data[t,3:length(data)]), as.vector(center_final[g,]))
  }}
dat_final = cbind(data[,1:2], dist_final)
dat_final_2= aggregate(dat_final, by=list(dat_final[,2]), mean)
dat_final_2[,c(1,3)]=NULL
class_final=as.factor(dat_final_2[,1])
colnames(dat_final_2) <- c(paste0("v", 1:m))

model_final = train(as.factor(v1)~.,data=dat_final_2, method="pda", metric="Accuracy", maximize = TRUE,
                trControl=trainControl("cv",number=10))
  
predicted_final = predict(model_final, dat_final_2, type = "prob")


pred <- prediction(predicted_final[,2], class_final)
perf <- performance(pred,"tpr","fpr")
plot(perf)
