  scatterMargins<-function(Tavg,Pr,Baseline=c(1950,1980),PlotTime=c(2040,2060),rcp=c("RCP 4.5","RCP 8.5"),DisplayOutput,
  OutputGraphics,Main,cexMult,xLab,yLab,writeMain,
  addLegend=TRUE,Xlim,Ylim,PlotMargins=FALSE,Text=TRUE){
 # compare the Average temperature and preciptation for a historic baseline to projections
 # under the selected RCPs

      if(missing(Main)) Main=paste("Projected Change from",Baseline[1],"to",Baseline[2], "\nbaseline to",
             PlotTime[1],"to",PlotTime[2])
       if(!writeMain) Main="" #overwrite any value with an empty string if we don't want a main
       if(missing(yLab)) yLab="Change in Precipitation (%)"
       Tavg@Var<-"TempChng"
       if(missing(xLab)) xLab=GenerateLab(Tavg)

       if(!DisplayOutput){ png(file.path(OutputGraphics,
             paste(paste(Baseline,collapse="_"),"to",paste(PlotTime,collapse="_"),length(rcp),"RCPs","Scatter.png",sep="_")),height=1000,width=1000)
             on.exit(dev.off())
        }
      # aggregate to years
      Tavg<-YrAgg(Tavg)
      Pr<-YrAgg(Pr)

      BasePeriod<-Pr@Year>=Baseline[1] & Pr@Year<Baseline[2]
      PlotPeriod<-Pr@Year>=PlotTime[1] & Pr@Year<PlotTime[2]

       # aggregate the data to the PlotTime for each model and subtract the average of everything
       # in the baseline period but look at percent change in precip
       PrDat<-100*(apply(Pr@Ts[PlotPeriod,Pr@Rcp%in%rcp],2,mean)-
               apply(Pr@Ts[BasePeriod,Pr@Rcp%in%rcp],2,mean))/apply(Pr@Ts[BasePeriod,Pr@Rcp%in%rcp],2,mean)
       TDat<-apply(Tavg@Ts[PlotPeriod,Tavg@Rcp%in%rcp],2,mean)-apply(Tavg@Ts[BasePeriod,Tavg@Rcp%in%rcp],2,mean)

      Rcp<-Pr@Rcp[Pr@Rcp%in%rcp]
      Proj<-Pr@Proj[Pr@Rcp%in%rcp]
     
      #put a cross for the median and quantiles for each
      Pquants <- aggregate(PrDat,list(RCP=Rcp),quantile,c(.25,.5,.75),na.rm=TRUE)
      Tquants <- aggregate(TDat,list(RCP=Rcp),quantile,c(.25,.5,.75),na.rm=TRUE)
     
      par(mar=c(5,7,5,2))
       EmissionsCol<-c("seagreen4","goldenrod1","darkorange3","red3","black")
       color.box<-col2rgb(EmissionsCol,alpha=TRUE)
                           color.box[4,]<-190
                           temp.fct<-function(a){return(rgb(red=a[1],green=a[2],blue=a[3],alpha=a[4]))}
                           EmissionsCol<-apply(color.box/255,2,temp.fct)
       outline<-EmissionsCol[5]
       EmissionsCol<-EmissionsColAll<-EmissionsCol[1:4]
      
       EmissionsCol<-EmissionsCol[c("RCP 2.6","RCP 4.5","RCP 6.0","RCP 8.5")%in%rcp]
       Dat<-data.frame(Temp=TDat,Precp=PrDat,Emissions=Rcp)
        Quants<-data.frame(Emissions=Tquants$RCP,Tmin=Tquants[[2]][,1],TMedian=Tquants[[2]][,2],Tmax=Tquants[[2]][,3],
                                           Pmin=Pquants[[2]][,1],PMedian=Pquants[[2]][,2],Pmax=Pquants[[2]][,3])

#=======================================
#   Plotting Points will all the legend info
if(!Text){
Pnum<-as.numeric(factor(Proj))
if(PlotMargins) layout(matrix(c(3,0,2,1,4,2), 2, 3, byrow = TRUE),width=c(3,1,1),heights=c(1,3))
else layout(matrix(c(1,2),nrow=1,ncol=2),width=c(3,1))
par(mar=c(5,1,5,0),oma=c(0,7,0,0))
 plot(extendrange(range(Dat$Temp,na.rm=TRUE)),extendrange(range(Dat$Precp,na.rm=TRUE)),ylab="",
      xlab=xLab,main=Main,cex.main=1.5*cexMult,cex.lab=1.4*cexMult,cex.axis=.9*cexMult,bty="l",
      type="n",las=1)
      mtext(yLab,2,line=2,outer=TRUE,cex=1.5*cexMult)
      abline(h=0) # zero change in precip

      text(TDat,PrDat,labels=Pnum,
      col= EmissionsColAll[Dat$Emissions],cex=1.2*cexMult)
  Pnum<-unique(Pnum)
  Ptxt<-unique(Proj)[order(Pnum,decreasing=TRUE)]
  Pnum<-Pnum[order(Pnum,decreasing=TRUE)]
    segments(x0=Tquants[,2][,1],y0=Pquants[,2][,2],x1=Tquants[,2][,3],y1=Pquants[,2][,2],lty=1,lwd=6,col= outline) 
     segments(x0=Tquants[,2][,2],y0=Pquants[,2][,1],x1=Tquants[,2][,2],y1=Pquants[,2][,3],lty=1,lwd=6,col= outline) 
     segments(x0=Tquants[,2][,1],y0=Pquants[,2][,2],x1=Tquants[,2][,3],y1=Pquants[,2][,2],
         col=EmissionsCol,lwd=2) 
     segments(x0=Tquants[,2][,2],y0=Pquants[,2][,1],x1=Tquants[,2][,2],y1=Pquants[,2][,3],
         col=EmissionsCol,lwd=2)                         
     points(x=Tquants[,2][,2],y=Pquants[,2][,2],cex=2.5,pch=21,
     bg=EmissionsCol)

 legMult<-ifelse(PlotMargins,1.6,1)
  #plot the text margian 
  plot(c(0,1),c(0,1.28),type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
  text(x=.25,y=1,"Model",cex=1.5*cexMult)
  text(x=.02,y=seq(from=0,to=.95,length=length(Ptxt)),label=Pnum,,cex=.8*cexMult*legMult)
  text(x=.09,y=seq(from=0,to=.95,length=length(Ptxt)),label=Ptxt,pos=4,cex=.8*cexMult*legMult)
 
  #emissions legend
 
  legend(-.1,1.3,legend=rev(rcp),title="",fill=rev(EmissionsCol),
  cex=1.3*legMult,bty="n")
  text(x=0,y=1.26,labels="Emissions",cex=1.8*legMult,pos=4)
  if(PlotMargins){
     # browser()
      r<-unique(Dat$Emissions)
      for(var in 1:2){
        if(var==1) Var=TDat
        else Var=PrDat
        
        Dens<-aggregate(Var,FUN=density,
                        by=list(Emissions=Dat$Emissions))
        rng<-0
        for(i in 1:length(Dens[[1]])) rng<-c(rng,range(Dens[[2]][i,]$y))
        #switch the axis for the two variables
        if(var==1){ 
          xrng<-range(TDat)
          yrng<-range(rng)
          xindx<-1
          yindx<-2
          
        }else{
          yrng<-range(PrDat)
          xrng<-range(rng)
          xindx<-2
          yindx<-1
        }
        
        #plot the Tavg margins
        plot(x=xrng,y=yrng,
             type="n",xaxt="n",yaxt="n",
             xlab="",ylab="",bty="n")
        
        # now we can add the density for each group
        for(i in 1:length(r)){
          polygon(Dens[[2]][i,][xindx][[1]],Dens[[2]][i,][yindx][[1]],
                  col=changeAlpha(EmissionsCol[i],alpha=.4),
                                   border=EmissionsCol[i])  
        } 
      }
  }
  return()
}

p1 <- ggplot(Dat,aes(x=Temp,y=Precp,colour=Emissions)) + geom_point(size=.01,alpha=.2) +
  theme_classic() + scale_colour_manual(values=EmissionsCol,guide="none")+
  theme(plot.margin=unit(c(0,0,0,0),"points"))+
  annotate("text", label=Proj, x=Dat$Temp, y=Dat$Precp,colour=EmissionsColAll[Dat$Emissions],size=4*cexMult)+
  geom_segment(data=Quants,mapping=aes(x=Tmin,y=PMedian,xend=Tmax,yend=PMedian),size=2,colour="black")+
  geom_segment(data=Quants,mapping=aes(x=TMedian,y=Pmin,xend=TMedian,yend=Pmax),size=2,colour="black")+
  geom_segment(data=Quants,mapping=aes(x=Tmin,y=PMedian,xend=Tmax,yend=PMedian,colour=Emissions),size=1)+
  geom_segment(data=Quants,mapping=aes(x=TMedian,y=Pmin,xend=TMedian,yend=Pmax,colour=Emissions),size=1)+
  geom_point(data=Quants,mapping=aes(x=TMedian,y=PMedian,fill=Emissions),size=6,pch=21,colour="black")+ 
  scale_fill_manual(values=EmissionsCol)+
  guides(fill=guide_legend(reverse=TRUE))+theme(axis.text.y = element_text(size = rel(cexMult))) +
    		theme(axis.title = element_text(size = 1.2*rel(cexMult))) +	
    		theme(plot.title =element_text(size=1.4*rel(cexMult)))+
        theme(axis.text.x = element_text(size = rel(cexMult)))+
        theme(legend.title=element_text(size=1.3*rel(cexMult)))+
        theme(legend.text=element_text(size=1.3*rel(cexMult)))+ylab(yLab)+xlab(xLab)
if(!PlotMargins){
  p1<-p1+ggtitle(Main)
  if(!addLegend) p1<-p1 + theme(legend.position = 'none')
  return(p1)
  } 
 
p4<-g_legend(p1) 

theme0 <- function(...) theme( legend.position = "none",
                               panel.background = element_blank(),
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.margin = unit(0,"null"),
                               axis.ticks = element_blank(),
                               axis.text.x = element_blank(),
                               axis.text.y = element_blank(),
                               axis.title.x = element_blank(),
                               axis.title.y = element_blank(),
                               axis.ticks.length = unit(0,"null"),
                               axis.ticks.margin = unit(0,"null"),
                               panel.border=element_rect(color=NA),...)

p2 <- ggplot(Dat,aes(x=Temp,colour=Emissions,fill=Emissions)) +
scale_fill_manual(values=EmissionsCol)+
scale_colour_manual(values=EmissionsCol)+
  geom_density(alpha=0.3) +
  theme_bw() +
  theme0(plot.margin = unit(c(1,0,0,2.2),"lines"))+labs(title=Main) +
    		theme(plot.title =element_text(size=1.4*rel(cexMult)))
    		
p3 <- ggplot(Dat,aes(x=Precp,colour=Emissions,fill=Emissions)) +
scale_fill_manual(values=EmissionsCol)+
scale_colour_manual(values=EmissionsCol)+
  geom_density(alpha=0.3) +
  coord_flip()  +
  theme_bw() +
  theme0(plot.margin = unit(c(0,1,1.2,0),"lines"))

#Main=textGrob(Main,gp=gpar(fontsize=10*cexMult))
grid.arrange(arrangeGrob(p2,p4,ncol=2,widths=c(3,1)),
             arrangeGrob(p1+ theme(legend.position = 'none'),p3,ncol=2,widths=c(3,1)),
             heights=c(1,3))
  }
  g_legend<-function(a.gplot){ 
    tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
    legend <- tmp$grobs[[leg]] 
    return(legend)
  } 
