MonthlyLine<-function(Observational,Projected=NA,Baseline,ProjectedTime=NA,RCP=NA,
                      SubtractMonthlyMean=FALSE,cexMult,plotLegend=TRUE,Watermark){

#check that all observational datasets cover the specified range
if(any(c(lapply(Observational,FUN=function(x) max(x@Year))<Baseline[2],
    lapply(Observational,FUN=function(x) min(x@Year))>Baseline[1])))
    stop("Not all observational datasets have complete records within baseline")
     Ylab=GenerateLab(Observational[[1]])
     if(Observational[[1]]@Var=="Precip" & toupper(Observational[[1]]@PlotUnits)=="IN") 
            Ylab="Total Precip (Inches/Month)"
     if(Observational[[1]]@Var=="Precip" & toupper(Observational[[1]]@PlotUnits)=="mm") 
            Ylab="Total Precip (mm/Month)"
   ExtractandAgg<-function(x,Baseline){
      #Extract the correct years and aggregate to monthly
      #using median in case any vars have a skewed dist or outliers
       Dat<-x@Ts[x@Year>=Baseline[1] & x@Year<=Baseline[2],]
       Month<-x@Month[x@Year>=Baseline[1] & x@Year<=Baseline[2]]
       aggregate(Dat,list(Month),mean)
   }
   
BaselineSubset<-lapply(Observational,FUN=ExtractandAgg,Baseline=Baseline)
  if(class(Projected)=="ClimateTS") FutureSubset<-ExtractandAgg(Projected,Baseline=ProjectedTime)
    
    if(!SubtractMonthlyMean){
        plotRange<-as.vector(lapply(BaselineSubset,FUN=function(x) range(x[,2],na.rm=TRUE)))
        if(class(Projected)=="ClimateTS") plotRange<-c(plotRange,range(FutureSubset[,-c(1)],na.rm=TRUE)) 
    }          
    #need to subtract off monthly baseline
    #if we have one observational dataset and one projected 
    #or two observational we can look at the difference
    if(SubtractMonthlyMean){
        canSubtractMean=FALSE
        if(length(Observational)==2 & !(class(Projected)=="ClimateTS")){
            canSubtractMean=TRUE
            BaselineSubset[[2]][,2]<- BaselineSubset[[2]][,2]-BaselineSubset[[1]][,2]
            plotRange=range(BaselineSubset[[2]][,2],na.rm=TRUE)
        }
        if(length(Observational)==1 & (class(Projected)=="ClimateTS")){
            canSubtractMean=TRUE
            FutureSubset[,2:ncol(FutureSubset)]<- FutureSubset[,2:ncol(FutureSubset)]-BaselineSubset[[1]][,2]
            plotRange=range(FutureSubset[,-c(1)],na.rm=TRUE)
        }
        if(!canSubtractMean) stop("could not subtract baseline -more than two models being compared")
    }
   par(mar=c(4,5,4,12),xpd=TRUE,xaxs="i")
 plot(c(1,12),extendrange(plotRange),xaxt="n",xlab="",ylab=Ylab,cex.axis=cexMult,
 cex=cexMult,cex.lab=cexMult,bty="l",las=1,type="n")
 if(!missing(Watermark)){
     yRng<-extendrange(plotRange,f=.3)
     rasterImage(Watermark,xleft=11.3,ybottom=min(yRng),xright=16,ytop=min(yRng)+.25*diff(yRng))
     
  }
   axis(1, at = 1:12,labels=FALSE)
   #====================================
   #         Plot Observational        #
  PastCol<-factor(names(BaselineSubset),levels=c("PRISM","Maurer","TopoWx"),ordered=TRUE)
  Lev <-unclass(PastCol)
  Cols<-c("indianred3","forestgreen","blue")

 for(i in 1:length(BaselineSubset)) lines(seq(1:12),BaselineSubset[[i]][,2],col=Cols[Lev[i]],lwd=2)

 if(class(Projected)!="ClimateTS"){ legCol<-(Cols[Lev])
                                    legNames<- (c(names(Observational)))
     }
   if(class(Projected)=="ClimateTS"){ legCol=(c(Cols[Lev],Col))
                                      legNames=(c(names(Observational),RCP))
      }

  if(plotLegend){

    legend(x=12,y=quantile(unlist(plotRange),probs=.7),legend=legNames,lty=1,lwd=3,
       col=legCol,cex=cexMult,bty="n",title="Dataset",seg.len=1)
             }

  mtext(month.abb,side=1,at=1:12,las=2,cex=cexMult,line=1)
}
