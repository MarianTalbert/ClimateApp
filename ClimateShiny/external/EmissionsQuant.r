EmissionSDPlot<-function(InputDat,PastClim,ParkName,Main,yLab,DisplayOutput,
  OutputGraphics,cexMult,Period=10,rcp=c("rcp26","rcp45","rcp60","rcp85"),writeMain,
  BlackAndWhite=FALSE,Watermark){
#InputDat is a dataframe 
#   rows = years for observations
#   columns = to model output for an emissions scenario and climate model
#   rownames = year
#   colnames = model.emissions emissioncs should be 5 characters long "rcp85" for example
#ParkName = The name of the park for the title
#Present = The current year for determining color break 
#Written by Marian Talbert 10/2013
        #=======================
        # aggregate to yearly
    Present=as.numeric(as.character(years(Sys.time())))
   
    if(!DisplayOutput){ tiff(file.path(OutputGraphics,
       paste(InputDat@Var,min(InputDat@Year),"to",max(InputDat@Year),"EmissionssQuant.tiff",sep="_")),
             height=7,width=7,units="in",res=600)
        on.exit(dev.off())
       }
   InputDat<-YrAgg(InputDat)
   if(missing(Main)) Main=paste("Model Projections for",ParkName) 
   if(!writeMain) Main="" #overwrite any value with an empty strinf if we don't want a main                 
   if(missing(yLab)) yLab=GenerateLab(InputDat)
      
    #Now plotting avgs by emissions scenario
   SDandAvg<-data.frame()
    if(BlackAndWhite){
         EmissionsBgCol<-rev(c("grey30","grey30","grey30","gray60","gray97","gray80","gray35","gray0"))
         EmissionsCol<-rev(c("grey30","grey30","grey30","gray60","gray90","gray97","gray35","gray0"))
   
   }else{      
   EmissionsBgCol<-rev(c("slateblue","mediumpurple1","blue","gray45","seagreen4","goldenrod1","darkorange3","red4"))
   EmissionsCol<-rev(c("slateblue","mediumpurple1","blue","gray28","seagreen4","goldenrod1","darkorange3","red4"))
   } 
  
  
   #getting the avg and sd for each emissions scenario and future time
    for(i in 1:4){
        Avg<-apply(InputDat@Ts[InputDat@Year>=Present,InputDat@Rcp==levels(InputDat@Rcp)[i]],1,median,na.rm=TRUE)
         LowQuant<-apply(InputDat@Ts[InputDat@Year>=Present,InputDat@Rcp==levels(InputDat@Rcp)[i]],1,quantile,
            prob=.05,na.rm=TRUE)
         HighQuant<-apply(InputDat@Ts[InputDat@Year>=Present,InputDat@Rcp==levels(InputDat@Rcp)[i]],1,quantile,
            prob=.95,na.rm=TRUE)
      
        if(i==1) SDandAvg<-data.frame(
                  Year=InputDat@Year[InputDat@Year>=Present],
                  Avg=Avg,LowQuant=LowQuant,HighQuant=HighQuant,
                  Emissions=rep(levels(InputDat@Rcp)[i],times=length(Avg)))
        else SDandAvg<-rbind(SDandAvg,data.frame(Year=InputDat@Year[InputDat@Year>=Present],
                  Avg=Avg,LowQuant=LowQuant,HighQuant=HighQuant,Emissions=rep(levels(InputDat@Rcp)[i],times=length(Avg))))
        }
    
  #sd and avg for past and present time
       Avg<-apply(InputDat@Ts[InputDat@Year<=Present,],1,median,na.rm=TRUE)
       LowQuant<-apply(InputDat@Ts[InputDat@Year<=Present,],1,quantile,prob=.05,na.rm=TRUE)
       HighQuant<-apply(InputDat@Ts[InputDat@Year<=Present,],1,quantile,prob=.95,na.rm=TRUE)
       
   SDandAvg<-rbind(SDandAvg,data.frame(Year=InputDat@Year[InputDat@Year<=Present],
        Avg=Avg,LowQuant=LowQuant,HighQuant=HighQuant,Emissions=rep("All",times=length(Avg))))
  
  #preparing past climate data for the plot
   if(!is.na(PastClim)){
       #aggregate past clim to year
      if(PastClim@Var!=InputDat@Var) stop("Input variables are not the same")
       PastClim<-YrAgg(PastClim)
       YearMean<-cbind(PastClim@Year,PastClim@Ts)
       YearMean<-rollapply(YearMean, Period, "mean",align="center",fill="NA",by.column=TRUE)
       YearMean<-YearMean[YearMean[,1]>=min(InputDat@Year),]
        SDandAvg<-rbind(SDandAvg,data.frame(Year=YearMean[,1],
            Avg=YearMean[,2],
            LowQuant=YearMean[,2],
            HighQuant=YearMean[,2],
            Emissions=rep(PastClim@SourcePath,times=nrow(YearMean)))
            )
   } 
   
  SDandAvg$Emissions<-factor(SDandAvg$Emissions,
     levels=rev(c("Prism","Maurer","TopoWx","All","RCP 2.6","RCP 4.5","RCP 6.0","RCP 8.5")),ordered=TRUE)
    
      PlotLevs<-c(rcp,"All")
      if(!is.na(PastClim)) PlotLevs<-c(PlotLevs,PastClim@SourcePath)
   SDandAvg<-SDandAvg[SDandAvg$Emissions%in%PlotLevs,]
   Lev <-c(levels(SDandAvg$Emissions)%in%PlotLevs)
   min45<-SDandAvg[SDandAvg$Emissions=="RCP 4.5",c(1,3,5)]

g<-ggplot(SDandAvg, aes(x = Year, y = Avg)) +
       geom_ribbon(aes(ymin = LowQuant,ymax = HighQuant,
           fill = Emissions,colour=Emissions), alpha = 0.5) +
       geom_line(aes(colour = Emissions),size = .7)+
       scale_colour_manual(values=EmissionsCol[Lev])+
       scale_fill_manual(values=EmissionsBgCol[Lev])+ylab(yLab)+
       ggtitle(Main)+
       geom_line(aes(colour = Emissions),size = .8)+
        theme(axis.text.y = element_text(size = rel(cexMult)),
        		axis.title = element_text(size = 1.2*rel(cexMult)),
        		plot.title =element_text(size=1.4*rel(cexMult)),
            axis.text.x = element_text(size = rel(cexMult)),
            legend.title=element_blank(),
            plot.margin =unit(c(8,80,4,4),"mm"),
            legend.text=element_text(size=rel(1.2*cexMult)),
            panel.background = element_rect(fill="white",colour="grey90"),
            panel.grid.major=element_line(colour="white"),
            panel.grid.minor=element_line(colour="white",size=.25),
            legend.key=element_rect(size=rel(cexMult)))+
        theme_classic(base_size =18)+
        geom_line(aes(Year, LowQuant,group=Emissions), min45,colour="gold")

        if(!missing(Watermark)){
         Watermark <- rasterGrob(Watermark, interpolate=TRUE)
         Yrng<-extendrange(na.omit(c(SDandAvg$LowQuant,SDandAvg$HighQuant)),f=.2)
         Xrng<-range(SDandAvg$Year,na.rm=TRUE)
         g<- g + annotation_custom(Watermark,xmin=max(Xrng)-16,xmax=max(Xrng)+.36*diff(Xrng)+4,ymin=min(Yrng),
                                       ymax=min(Yrng)+.23*diff(Yrng))
                          g <- ggplot_gtable(ggplot_build(g))
                            g$layout$clip[g$layout$name=="panel"] <- "off"

        }
        
plot(g)
return(g)
}


 