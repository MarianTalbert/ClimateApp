BatchClimateTs <- function(Dat,PlotUnits,Boundary,Clip,RCP,Bioclim=FALSE,Var=NA){
      #read in the data and save the workspace for future modifications
      
      AvailableDat <- c("Maurer","Prism","TopoWx","BCSDCMIP5","RawCMIP5","Hydro")
      if(!Dat%in%AvailableDat) stop("Invalid data name")
      if(Dat=="Hydro") stop("Hydro not implemented here yet get your r programmer to write a method")
      PlotUnits<-c(PlotUnits[2],PlotUnits[1],PlotUnits[1])
           #if we need it get the time series for the available vars
           Vars<-c("Tmax","Tmin")
           Units<-c(PlotUnits[2],PlotUnits[2])
           
           if(Dat=="BCSDCMIP5"){ 
             Vars<-c(Vars,"Tavg")
             Units<-c(Units,PlotUnits[2])
           }
           
           if(Dat!="TopoWx"){ 
             Vars<-c(Vars,"ppt")
             Units<-c(Units,PlotUnits[1])
           }
      Out<-list()
      
      for(v in 1:length(Vars)){ #loop through tmin tmax and ppt if available
          l<-length(Out)
           Out[[l+1]] <- ClimateTS(Dat,Var=Vars[v],
                  Boundary=Boundary,PlotUnits=Units[v],
                  Clip=Clip,RCP=RCP)
           names(Out)[l+1]<-Vars[v]
           
           #calculate tavg from tmin and tmax to save time
           if(!Dat %in% c("Hydro","BCSDCMIP5") & v==2){
             Out[[l+2]]<-Out$Tmax
             Out[[l+2]]@Ts<-(Out$Tmax@Ts+Out$Tmin@Ts)/2
             names(Out)[l+2]<-"Tavg"
             Out[[l+2]]@Var<-"Tavg" 
           }
            
      }
     
      if(Bioclim==TRUE & Dat!="TopoWx"){
        l<-length(Out)
         BioDat<-BioclimTs(Out$ppt,Out$Tmin,Out$Tmax)
        #I'm sure there's a more elegant way to do this which I'll find tomorrow
         for(i in 1:19){ Out[[l+i]]<-BioDat[[i]]
             names(Out)[l+i]<-paste("Bio",i,sep="")
         }
      }

  return(Out)
}
