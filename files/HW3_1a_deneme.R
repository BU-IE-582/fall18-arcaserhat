require(scatterplot3d)
#bonus
dataPath='C:/Users/07830/Desktop/HW3_1a/UWave/'
xData=read.table(paste0(dataPath,'uWaveGestureLibrary_X_TRAIN'))
yData=read.table(paste0(dataPath,'uWaveGestureLibrary_Y_TRAIN'))
zData=read.table(paste0(dataPath,'uWaveGestureLibrary_Z_TRAIN'))

classInfo=xData[,1]
uniqueClasses=unique(classInfo)
par(mfrow=c(2,4))
for(cls in uniqueClasses){
  indices=which(classInfo==cls)
  selected=sample(indices,1)
  #plot selected
  #transform to acceleration to position information by cumulative sum
  x=cumsum(xData[selected,2:ncol(xData)])
  y=cumsum(yData[selected,2:ncol(yData)])
  z=cumsum(zData[selected,2:ncol(zData)])
  scatterplot3d(x,y,z, pch=16, highlight.3d=TRUE, main=paste("Class",cls))
}
