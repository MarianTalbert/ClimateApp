EmissionLinePlot<-function(InputDat,PastClim,ParkName,Main,yLab,DisplayOutput,
  OutputGraphics,cexMult,Period=10,rcp=c("rcp26","rcp45","rcp60","rcp85"),writeMain,BlackAndWhite=FALSE){
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

    if(!DisplayOutput){ png(file.path(OutputGraphics,
       paste(InputDat@Var,min(InputDat@Year),"to",max(InputDat@Year),"EmissionsLine.png",sep="_")),height=1000,width=1000)
        on.exit(dev.off())
       }
   InputDat<-YrAgg(InputDat)
   if(missing(Main)) Main=paste("Model Projections for",ParkName)
   if(!writeMain) Main="" #overwrite any value with an empty strinf if we don't want a main
   if(missing(yLab)) yLab=GenerateLab(InputDat)

   InputDat@Ts<-InputDat@Ts[,InputDat@Rcp%in%rcp]
   InputDat@Proj<-InputDat@Proj[InputDat@Rcp%in%rcp]
   InputDat@Rcp<-InputDat@Rcp[InputDat@Rcp%in%rcp]
    #Now plotting avgs by emissions scenario
   SDandAvg<-data.frame()
    if(BlackAndWhite){
         EmissionsBgCol<-rev(c("grey30","grey30","grey30","gray60","gray97","gray77","gray35","gray0"))
         EmissionsCol<-rev(c("grey30","grey30","grey30","gray60","gray97","gray77","gray35","gray0"))
   
   }else{      
   EmissionsBgCol<-rev(c("slateblue","mediumpurple1","blue","gray45","seagreen4","goldenrod1","darkorange3","red4"))
   EmissionsCol<-rev(c("slateblue","mediumpurple1","blue","gray28","seagreen4","goldenrod1","darkorange3","red4"))
   } 
   Ts<-data.frame(Avg=as.vector(InputDat@Ts),Emissions=factor(rep(InputDat@Rcp,each=nrow(InputDat@Ts)),
        levels=rev(c("TopoWx","Prism","Maurer","All","RCP 2.6","RCP 4.5","RCP 6.0","RCP 8.5")),ordered=TRUE),
        Year=rep(InputDat@Year,times=ncol(InputDat@Ts)),Model=rep(InputDat@Proj,each=nrow(InputDat@Ts)))

   #we need to duplicate this one year so the grey lines merge into the
   #rcp colored lines
   temp=Ts[Ts$Year==2005,]
   Ts$Emissions[Ts$Year<=2005]<-"All"
   Ts<-rbind(Ts,temp)
  #preparing past climate data for the plot
   if(!is.na(PastClim)){
     
       #aggregate past clim to year
      if(PastClim@Var!=InputDat@Var) stop("Input variables are not the same")
       PastClim<-YrAgg(PastClim)
       YearMean<-cbind(PastClim@Year,PastClim@Ts)
       YearMean<-rollapply(YearMean, Period, "mean",align="center",fill="NA",by.column=TRUE)
       YearMean<-YearMean[YearMean[,1]>=min(InputDat@Year),]
        PastDat<-data.frame(Avg=YearMean[,2],Emissions=rep(PastClim@SourcePath,times=length(YearMean)),
           Year=YearMean[,1],Model=rep(PastClim@SourcePath,times=length(YearMean)))
        Ts<-rbind(Ts,PastDat)
        Ts$Emissions<-factor(Ts$Emissions,
             levels=rev(c("TopoWx","Prism","Maurer","All","RCP 2.6","RCP 4.5","RCP 6.0","RCP 8.5")),ordered=TRUE)

   }
  
 EmissionsCol<-EmissionsCol[table(Ts$Emissions)>0]

  Ts$ModelRCP<-paste(Ts$Model,Ts$Emissions,sep="_")
g<-ggplot(Ts, aes(x = Year, y = Avg,group=ModelRCP)) + geom_line(aes(colour = Emissions),
    size = .01,alpha=.2)+scale_colour_manual(values=EmissionsCol)+
    ylab(yLab)+ ggtitle(Main)+theme(axis.text.y = element_text(size = rel(cexMult))) +
    		theme(axis.title = element_text(size = 1.2*rel(cexMult))) +
    		theme(plot.title =element_text(size=1.4*rel(cexMult)))+
        theme(axis.text.x = element_text(size = rel(cexMult)))+
        theme(legend.title=element_text(size=rel(cexMult)))+
        theme(legend.text=element_text(size=rel(cexMult)))+
        theme(panel.background = element_blank())+
        #theme(legend.background=element_rect(fill="gray90",colour="gray55"))+
        theme(plot.margin =unit(c(8,2,0,0),"mm"))+
        theme_classic(base_size =18)+
        stat_summary(aes(y = Avg,group=Emissions,colour=Emissions), fun.y=mean,geom="line",size=1.5)
plot(g)
return(g)
}