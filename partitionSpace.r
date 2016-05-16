library(cluster)
library(grDevices)
load("C:\\Users\\mtalbert\\Desktop\\Climate\\ParkOutput\\UnitedStates\\USProjectionMaps")
PastTemp<-BaseLst[[1]][[1]][[1]]@Layer
PastPrecp<-BaseLst[[2]][[1]][[1]]@Layer
fitDat<-cbind(as.vector(PastTemp),as.vector(PastPrecp))
colnames(fitDat)<-c("Temp","Ppt")
fitDat<-fitDat[complete.cases(fitDat),]
#Clara works best for large data
#==============================================
#testing the effect of setting sample size vs number of samples
# sample size here
ClaraPart<-clara(fitDat,samples=5,sampsize=1000,k=300,rngR=TRUE)
par(mfrow=c(2,2))
for(i in 1:4){
ClaraPart<-clara(fitDat,samples=5,sampsize=1000,k=300,rngR=TRUE)
 Cols<-c("blue","blue3")
       color.box<-col2rgb(Cols,alpha=TRUE)
                           color.box[4,]<-20
                           temp.fct<-function(a){return(rgb(red=a[1],green=a[2],blue=a[3],alpha=a[4]))}
                           Cols<-apply(color.box/255,2,temp.fct)
plot(fitDat[,1],fitDat[,2],pch=21,bg=Cols[1],col=Cols[2],cex=.4)
points(ClaraPart$medoids[,1],ClaraPart$medoids[,2],cex=.7,pch=21,col="black",bg="red")
}

par(mfrow=c(2,2))
for(i in 1:4){
ClaraPart<-clara(fitDat,samples=50,k=300,rngR=TRUE)
 Cols<-c("blue","blue3")
       color.box<-col2rgb(Cols,alpha=TRUE)
                           color.box[4,]<-20
                           temp.fct<-function(a){return(rgb(red=a[1],green=a[2],blue=a[3],alpha=a[4]))}
                           Cols<-apply(color.box/255,2,temp.fct)
plot(fitDat[,1],fitDat[,2],pch=21,bg=Cols[1],col=Cols[2],cex=.4)
points(ClaraPart$medoids[,1],ClaraPart$medoids[,2],cex=.7,pch=21,col="black",bg="red")
}