 

BoxplotRCP<-function(InputDat,BaseDat,Baseline,subtractB=FALSE,BarAvg=20,AllOnePlot=TRUE,Col=NA,Main,Ylab,DisplayOutput,
      OutputGraphics,cexMult,writeMain,PlotBase=TRUE,RCP,Watermark){
    #  InputDat =
    #  BarAvg = length of time to represent each bar 10 InputDat@Years by default but can be none
    #  HeatBy = "med"(median) or "recent" how the color scale is organized
    
    if(missing(Ylab)) Ylab=GenerateLab(InputDat)
    if(InputDat@Var=="Precip") Ylab="Total Precip (Inches/Season)" 
    if(InputDat@Var=="Precip" & InputDat@PlotUnits=="mm") Ylab="Total Precip (mm/Season)"
    if(!DisplayOutput){ png(file.path(OutputGraphics,
           paste(InputDat@Var,min(Baseline),"to",max(Baseline),"season","BoxbyRCP.png",sep="_")),height=1000,width=1300)
           on.exit(dev.off()) 
            } 
     par(oma=c(4,4,1,7))
            Groups = list(Winter=c(12,1,2),Spring=c(3,4,5),Summer=c(6,7,8),Autumn=c(9,10,11))
             layout(matrix(c(1,2,5,3,4,6),2,3,byrow=TRUE),c(4,4,2),c(2,2))

   
   for(i in 1:length(Groups)){
     par(mar=c(1,3,3,1))
    
         Main<-names(Groups)[i]
          if(!writeMain) Main="" #overwrite any value with an empty strinf if we don't want a main 
         Keep <- InputDat@Month%in%Groups[i][[1]]

         InputDat2<-InputDat
         InputDat2@Ts <- InputDat@Ts[Keep,]
         InputDat2@Year<-InputDat@Year[Keep]
         #Because this is subset to the 3 months in the season
         #yr agg gives us the mean for the season rather than 3 values per year
         InputDat2<-YrAgg(InputDat2)
         YrKeep<-InputDat2@Year
        
          Yr<-cut(YrKeep,seq(from=1950,to=2110,by=BarAvg),labels=seq(from=1960,to=2100,by=BarAvg),include.lowest=TRUE)
         Yr<-as.numeric(as.character(Yr))
          InputDat2<-InputDat2@Ts

          plot(extendrange(YrKeep),range(InputDat2,na.rm=TRUE),ylab="",main=Main,
             xlab="",cex.main=1.4*cexMult,cex.lab=cexMult,cex.axis=cexMult,
             xaxt="n",bty="l",las=1,type="n")

           Present<-as.numeric(format(Sys.time(),"%Y")) 
           if(length(RCP)==1){ 
             OffSet<-0
             boxw<-9
           }
           if(length(RCP)==2){ 
              OffSet<-c(-3,3)
              boxw<-5.5
           }
           if(length(RCP)==3){ 
             OffSet<-c(-4,0,4)
             boxw=4
           }
           if(length(RCP)==4){ 
             OffSet<-c(-6,-2,2,6)
             boxw=3.5
           }
           Col<-c("seagreen4","goldenrod1","darkorange3","red4")
           Colfade =  changeAlpha(Col,alpha=.5)
           Col[2]<-"goldenrod3"
           rcps<-c("RCP 2.6","RCP 4.5","RCP 6.0","RCP 8.5")
             Col<-Col[rcps%in%RCP] #get the right colors
             Colfade<-Colfade[rcps%in%RCP]
             Col<-c("grey28",Col)
             Colfade=c(changeAlpha("grey28",alpha=.5),Colfade)
           
           LegTxt<-c("\nModeled\nHistoric","RCP 2.6","RCP 4.5","RCP 6.0","RCP 8.5")[c(TRUE,rcps%in%RCP)]

      #These are the projections
     for(j in 1:length(RCP))
        boxplot(InputDat2[Yr>Present,InputDat@Rcp==RCP[j]]~Yr[Yr>Present],na.action=na.omit,
        col=Colfade[j+1],add=TRUE,at=unique(Yr[Yr>Present])+OffSet[j],boxwex=boxw,
          xaxt="n",yaxt="n",frame=F,border=Col[j+1])
     
     #This is the past
      boxplot(InputDat2[Yr<=Present,]~Yr[Yr<=Present],
      col=Colfade[1],border=Col[1],add=TRUE,at=unique(Yr[Yr<=Present]),boxwex=7,
        xaxt="n",yaxt="n",frame=F)
        
        if(i%in%c(3,4)) axis(side = 1, at=seq(from=1960,to=2100,by=BarAvg),cex.axis=.8*cexMult)
        mtext("Year                       ",side=1,line=2,outer=TRUE,cex=1.2*cexMult)
       
             }

         par(xpd=TRUE,mar=c(0,0,0,0))
         plot(c(0,1),c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",type="n",bty="n")
           legend("topleft",title=expression(bold("Emissions")),legend=rev(LegTxt),pch=22,col=rev(Col),
           pt.bg=rev(Col),cex=cexMult,bty="n",y.intersp=0,pt.cex=2.6*cexMult)

      mtext(Ylab,side=2,outer=TRUE,cex=1.2*cexMult,line=0)
     if(!missing(Watermark)){

      plot(0:1,0:1,type="n",bty="n",xlab="",ylab="",xaxt="n",yaxt="n")
     rasterImage(Watermark,xleft=-.33,ybottom=-.1,xright=1.2,ytop=.5)

  }
}


