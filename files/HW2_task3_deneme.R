library(jpeg)
library(graphics)

str(img)
dim(img)
img = readJPEG("C:/Users/asus/Desktop/dog.jpg", native = FALSE)
plot(c(0, 225), c(0, 225), type = "n", xlab = "", ylab = "")
rasterImage(img,0,0, 200, 200, interpolate = FALSE)



#Channels
red=img[,,1]
green=img[,,2]
blue=img[,,3]

par(mfrow=c(1,3))
image(t(red[nrow(red):1L,]),main="Red Channel")
image(t(green[nrow(green):1L,]),main="Green Channel")
image(t(blue[nrow(blue):1L,]),main="Blue Channel")

#Noise addition

noisy_img=img
for(i in 1:3){noisy_img[,,i]=noisy_img[,,i]+matrix(runif(200*200,0,0.1),nrow=200)}
# gives the error of larger than 1 value
noisy_img[noisy_img>1]=1

plot(c(0, 225), c(0, 225), type = "n", xlab = "", ylab = "")
rasterImage(noisy_img,0,0, 200, 200, interpolate = FALSE)


#Noisy channels
noisy_red=noisy_img[,,1]
noisy_green=noisy_img[,,2]
noisy_blue=noisy_img[,,3]
noisy_gray=noisy_red+noisy_green+noisy_blue
noisy_gray=noisy_gray/max(noisy_gray)

par(mfrow=c(1,3))
image(t(noisy_red[nrow(noisy_red):1L,]),main="Noisy Red Channel")
image(t(noisy_green[nrow(noisy_green):1L,]),main="Noisy Green Channel")
image(t(noisy_blue[nrow(noisy_blue):1L,]),main="Noisy Blue Channel")


# Construct patches
matrix=matrix(0,40000,9)

for (i in 2:199) {
  for(j in 2:199) {
    matrix[198*(i-2)+j-1,1]=noisy_gray[i-1,j-1]
    matrix[198*(i-2)+j-1,2]=noisy_gray[i-1,j]
    matrix[198*(i-2)+j-1,3]=noisy_gray[i-1,j+1]
    matrix[198*(i-2)+j-1,4]=noisy_gray[i,j-1]
    matrix[198*(i-2)+j-1,5]=noisy_gray[i,j]
    matrix[198*(i-2)+j-1,6]=noisy_gray[i,j+1]
    matrix[198*(i-2)+j-1,7]=noisy_gray[i+1,j-1]
    matrix[198*(i-2)+j-1,8]=noisy_gray[i+1,j]
    matrix[198*(i-2)+j-1,9]=noisy_gray[i+1,j+1]
  }
}
data=matrix

pca = princomp(data, cor=TRUE)
plot(pca)

par(mfrow=c(1,1))

#Principal Component 1 

PC1 = pca$scores[,1]

PC1_img = matrix(0,198,198)

for(k in 1:198){
  for(l in 1:198){
    PC1_img[k,l]=PC1[198*(k-1)+l]
  }
} 
library(RSNNS)
PC1_img_n=normalizeData(PC1_img,type="0_1")

plot(c(0, 200), c(0, 200), type = "n", xlab = "", ylab = "")
rasterImage(PC1_img_n,0,0, 198, 198, interpolate = FALSE)

#Principal Component 2 

PC2 = pca$scores[,2]

PC2_img = matrix(0,198,198)

for(k in 1:198){
  for(l in 1:198){
    PC2_img[k,l]=PC2[198*(k-1)+l]
  }
} 
library(RSNNS)
PC2_img_n=normalizeData(PC2_img,type="0_1")

plot(c(0, 200), c(0, 200), type = "n", xlab = "", ylab = "")
rasterImage(PC2_img_n,0,0, 198, 198, interpolate = FALSE)

#Principal Component 3 

PC3 = pca$scores[,3]

PC3_img = matrix(0,198,198)

for(k in 1:198){
  for(l in 1:198){
    PC3_img[k,l]=PC3[198*(k-1)+l]
  }
} 
library(RSNNS)
PC3_img_n=normalizeData(PC3_img,type="0_1")

plot(c(0, 200), c(0, 200), type = "n", xlab = "", ylab = "")
rasterImage(PC3_img_n,0,0, 198, 198, interpolate = FALSE)