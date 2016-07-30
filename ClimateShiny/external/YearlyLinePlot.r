YearlyLinePlot<-function(InputDat,MovAvgPeriod=10,MovAvg=FALSE,LM=TRUE,LMPeriod,Ylab,Xlab,Plot=TRUE,maCol,Main,cexMult,DisplayOutput,OutputGraphics,writeMain,Watermark){
    # in order to produce some line plots we need to do a couple things
    # remove incomplete years
    # clip each month to the correct shape
    # aggregate over year
    # then we can plot the data with our moving average
    # InputDat     =  a vector with the value names ar year with decimal 0:11/12
    # MovAvgPeriod = the number of years for which to produce a lagging average
    # LM           = Boolean indicating if a linear trend with error bars should be added
    # Ylab         = Character string y lab
    # Xlab         = Character string x lab
    # Plot         = boolean indicating if we'd like to plot it now
    # maCol        = color for the moving avg line
    # Months       = Produce the plot only on the specified months c(1,2,3) for example
    
    # @data contains the data of the clipped a
       #now average over the months for each year and over the pixels

    if(missing(Ylab)) Ylab=GenerateLab(InputDat[[1]])
    if(missing(Xlab)) Xlab="Year"
    if(missing(Main)) Main=paste(ParkName,"\n Historic ",LongName(InputDat[[1]]@Var),sep=" ")
       if(!writeMain) Main="" #overwrite any value with an empty strinf if we don't want a main 

       YrRange<-range(unlist(lapply(InputDat,FUN=function(x){range(x@Year)})))
    if(!DisplayOutput){ png(file.path(OutputGraphics,
       paste(paste(names(InputDat),collapse="_"),InputDat[[1]]@Var,YrRange[1],"to",YrRange[2],"Line.png",sep="_")),height=1000,width=1000)
       on.exit(dev.off())
       }
   
       InputDat<-lapply(InputDat,YrAgg)
    for(i in 1:length(InputDat)){
        YearlyPkAvg <- InputDat[[i]]@Ts
        YearMean<-cbind(InputDat[[i]]@Year,InputDat[[i]]@Ts)
        rollAvg <- c(rep(NA,times=floor((MovAvgPeriod-1)/2)),rollmean(YearlyPkAvg, MovAvgPeriod),rep(NA,times=ceiling((MovAvgPeriod-1)/2)))
        if(i==1) YearDat<-data.frame(Year=unique(InputDat[[i]]@Year),PkAvg=YearlyPkAvg,rollAvg=rollAvg,
              Dataset=rep(names(InputDat)[i],times=length(rollAvg)))
        else YearDat<-rbind(YearDat,data.frame(Year=unique(InputDat[[i]]@Year),PkAvg=YearlyPkAvg,rollAvg=rollAvg,
              Dataset=rep(names(InputDat)[i],times=length(rollAvg))))
     }

     PastCol<-c("indianred1","steelblue1","springgreen3")
      YearDat$DatSet<-factor(YearDat$Dataset,levels=rev(c("PRISM","Maurer","TopoWx")),ordered=TRUE)
      Lev <-c("PRISM","Maurer","TopoWx")%in%levels(YearDat$Dataset)

    PlotOut <- ggplot(aes(Year, PkAvg), data=YearDat) +
          geom_line(aes(colour = Dataset),size=.4) + 
          theme_classic() + scale_color_manual(values=c("indianred3","forestgreen","blue")[Lev])+
          theme(panel.border= element_blank(),
        		  axis.text.y = element_text(size = rel(cexMult)),
              axis.text.x = element_text(size = rel(cexMult)),
              axis.title.y = element_text(size = rel(cexMult), angle = 90),
              axis.title.x = element_text(size = rel(cexMult)),
              plot.title =element_text(size=rel(1.2*cexMult)),
          		plot.margin =unit(c(5,15,4,4),"mm"),
          		legend.title=element_text(size=rel(1.2*cexMult)),
              legend.text=element_text(size=rel(1.2*cexMult)))+
        #scale_colour_manual(values=PastCol[Lev])+
    		ylab(Ylab) + xlab(Xlab) + ggtitle(Main)+
    		scale_x_continuous(breaks = c(1900, 1920, 1940, 1960, 1980, 2000)) 
    		
    		if(LM) PlotOut <- PlotOut + geom_smooth(method="lm",aes(colour=Dataset,fill=Dataset),
    		                                        linetype="dashed",size=.3)+
                              scale_fill_manual(values=c("indianred3","forestgreen","blue")[Lev])
        if(MovAvg) PlotOut <- PlotOut + geom_line(aes(y=rollAvg,colour=Dataset),size=1.2)

        if(!missing(Watermark)){
         Watermark <- rasterGrob(Watermark, interpolate=TRUE)
         Yrng<-extendrange(YearDat$PkAvg,f=.28)
         Xrng<-range(YearDat$Year)
         PlotOut<- PlotOut + annotation_custom(Watermark,xmin=max(Xrng)-2,xmax=max(Xrng)+.36*diff(Xrng)+7,ymin=min(Yrng),
                                       ymax=min(Yrng)+.26*diff(Yrng))
                          PlotOut <- ggplot_gtable(ggplot_build(PlotOut))
                            PlotOut$layout$clip[PlotOut$layout$name=="panel"] <- "off"
                          #  grid.draw(gt)
        }
        if(Plot)plot(PlotOut)
    
		return(PlotOut)
}

