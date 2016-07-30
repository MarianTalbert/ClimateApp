
ImagePlot<-function(InputDat,Ylab="",Xlab="",Plot=TRUE,Months,Colors,Baseline=c(1890,1980),Main,ColVar="TempChng",cexMult,
DisplayOutput=DisplayOutput,OutputGraphics=OutputGraphics,writeMain){
     Call<-match.call()
 
      
    if(!DisplayOutput){ png(file.path(OutputGraphics,
       paste(Call$InputDat,"Baseline",min(Baseline),"to",max(Baseline),"Image.png",sep="_")),height=1000,width=500)
        on.exit(dev.off())
        }
    if(missing(Main)) Main= paste(LongName(InputDat@Var),"normalized with\n",min(Baseline),"to",max(Baseline),"Baseline",sep=" ")
     if(!writeMain) Main="" #overwrite any value with an empty strinf if we don't want a main
     
    Dat<-matrix(data=InputDat@Ts,nrow=12,byrow=FALSE)    
    #This should also work for the GDO with a specified model 
    BsLnDat<-InputDat@Ts[InputDat@Year>=Baseline[1] &InputDat@Year>Baseline[2]]
    BsLnMn<-apply(matrix(data=BsLnDat,nrow=12,byrow=FALSE),1,mean) #we calculate the mean this way so it's monthly 
    BsLnSD<-apply(matrix(data=BsLnDat,nrow=12,byrow=FALSE),1,sd)  
 
    ID<-(Dat-BsLnMn)/BsLnSD
    if(missing(Colors)) Colors<-GenerateColors(ColVar)
    Colors=two.colors(n=256, start=Colors[1], end=Colors[length(Colors)], middle="gray89",alpha=1.0)
   
    Breaks<-seq(from=-2.5,
        to=2.5,length=length(Colors)+1)
    ID[ID<min(Breaks)]<-min(Breaks)
    ID[ID>max(Breaks)]<-max(Breaks)    
    #SetBreaks(ID,"diff",Colors)
    par(mar=c(5, 6, 4, 2))
    image.plot(z=t(ID),y=seq(1:12),x=unique(InputDat@Year),ylab="Month",las=1,
    xlab=Ylab,col=Colors,main=Main,cex.lab=cexMult,cex.axis=.8*cexMult,cex.main=cexMult,legend.mar=7.1,breaks=Breaks)    
}

