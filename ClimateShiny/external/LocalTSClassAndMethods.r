setClass("ClimateTS",
  representation=list(Ts="matrix",Time="POSIXct",Year="vector",Month="vector",PlotUnits="character",Var="vector",
          SourcePath="character",Proj="vector",Rcp="vector"))

ClimateTS = function(Data,Var,Boundary,PlotUnits,WholeYear=TRUE,Clip=TRUE,RCP=NA,...){
	Ts = new("ClimateTS",Data,Var,Boundary,PlotUnits,WholeYear,Clip,RCP,...)
	return(Ts)
}

setMethod(f="initialize",signature="ClimateTS",
	definition=function(.Object,Data,Var,Boundary,PlotUnits,WholeYear,Clip=TRUE,RCP=NA,...){
        ClimateObj<-new(Data)
	       
	       #EVENTUALLY I'd like to make this and the ClimateTS that works on data I pull down from thredds different methods that
	       #are called on two clases both of which result in identical ClimateTS objects
         #I'll need a translation here as not all vars
         #have the same name

         on.exit(if(class(Clim)=="ncdf4") nc_close(Clim))
          
         VarFileTag<-getElement(ClimateObj,paste(Var,"FileTag",sep=""))
         VarName<-getElement(ClimateObj,paste(Var,"Name",sep=""))
         fileList<-getList(ClimateObj,VarFileTag,RCP)
         if(!Clip) warning("Clip is turned off")
         UnitIn <- toupper(getElement(ClimateObj,Var))
  
         .Object@PlotUnits <- PlotUnits
         .Object@SourcePath <- Data
         .Object@Var<-StandardUnits(Var)       
         .Object@Proj<-vector()
         .Object@Rcp<-vector() 
     #for some datasets each year is stored in a different .nc file
        #looping over the year files here
        OutputFrame<-vector();TimeVec<-vector()
          TimeList<-list(); OutList<-list(); 
          
      for (fi in 1:length(fileList)){
                  Clim<-nc_open(fileList[fi])
                     
                  if(fi==1)  ind<-GetIndicies(Clim,ClimateObj,Boundary,Clip)
                  
                  #get time sequences for looping 
                  l<-getTime(Clim,ClimateObj)
                         
             #read in just a year at a time 
             for(Time in 1:length(l$StartTime)){                        
                  tm <- ncvar_get(Clim,ClimateObj@timeName)[l$StartTime[Time]:(l$StartTime[Time]+l$CountTime[Time]-1)]
                  RastArray<-ncvar_get(Clim, varid=VarName, start=c(min(ind[,1]),min(ind[,2]),l$StartTime[Time]), 
                            count=c((max(ind[,1])-min(ind[,1])+1),(max(ind[,2])-min(ind[,2])+1),l$CountTime[Time]))
                  
                   indSmall<-cbind(ind[,1]-min(ind[,1]),ind[,2]-min(ind[,2]))+1
                      ClippedDat<-apply(RastArray, 3, function(x) x[indSmall]) #indexing in multidimensional arrays is less than intuitive
                      a<-RastArray[,,4]
                       #a[indSmall]<-1
                     
                 #now average over all of the selected pixels to get a time vector
                if(length(dim(ClippedDat))>0) ClippedDat<-apply(ClippedDat,2,mean,na.rm=TRUE)
                #for first loop for all get the output frame started
                #================================================
                # this section should probably be written in a class specific method
                if(class(ClimateObj) %in% c("Prism","Maurer","TopoWx")){
                     OutputFrame<-c(OutputFrame,as.vector(ClippedDat))
                     #for maurer each file contains different years which need to be concatenated
                     TimeVec<-c(TimeVec,tm)
                } 
              if(class(ClimateObj)%in%c("BCSDCMIP5","RawCMIP5")){  
                 #set up a time list for now because different datasets start and end at different times 
                   
                 #once per file figure out the time seq and set up a vector to hold the output
                 if(Time==1){ 
                     TimeList[[fi]]<-ncvar_get(Clim,ClimateObj@timeName)
                     OutputVector<-ClippedDat
                 }else OutputVector<-c(OutputVector,ClippedDat)
               
              }
              #==================================================   
        } #end time loop
        Clim<-nc_close(Clim)
        
         print(paste(fi, "of", length(fileList)))
         # print(OutputVector[1:20])
         if(class(ClimateObj)=="BCSDCMIP5"){ 
             OutList[[fi]]<-OutputVector
         }
    } #end fileListloop
      
    if(class(ClimateObj)=="BCSDCMIP5"){
         .Object<-combineBCSD(ClimateObj,TimeList,OutList,fileList,.Object)
         OutputFrame<-.Object@Ts
    }else .Object@Time <- as.POSIXct(TimeVec*86400, origin = ClimateObj@TimeOrigin,tz= "UTC")
        
    .Object@Year <- as.numeric(format(.Object@Time,"%Y"))
    .Object@Month <- as.numeric(format(.Object@Time,"%m")) 
    #===========================================
    # remove years for which we only have a portion of the year
    # because they cause funkiness
       
    if(WholeYear){
      if(any(table(.Object@Year)!=12)){
          IncompleteYrs <- which(.Object@Year%in%(names(table(.Object@Year))[table(.Object@Year)!=12]),arr.ind=TRUE)
          if(class(ClimateObj) %in% c("Prism","Maurer")) OutputFrame <- OutputFrame[-c(IncompleteYrs)]
          else  OutputFrame <- OutputFrame[-c(IncompleteYrs),]
          .Object@Time <- .Object@Time[-c(IncompleteYrs)]
          .Object@Year <- .Object@Year[-c(IncompleteYrs)]
          .Object@Month <- .Object@Month[-c(IncompleteYrs)]
          }
     }
     
      #NA values are differnt for each dataset but generally very large
       OutputFrame[OutputFrame>10000000]<-NA 
        
      UnitIn <- getElement(ClimateObj,Var)
      
     .Object@Ts<-as.matrix(OutputFrame)
       
     if (.Object@PlotUnits != UnitIn)
          .Object@Ts<-Convert(.Object@Ts,toupper(UnitIn),toupper(.Object@PlotUnits),.Object@Month)
           
   return(.Object)
})

YrAgg<-function(InputDat){
#takes a ClimateTs and aggregates it to yearly data from monthly
   
       if(InputDat@Var=="Precip") Ts<-aggregate(InputDat@Ts[,1],FUN=sum,by=list(Year=InputDat@Year))[,2]
       else  Ts<-aggregate(InputDat@Ts[,1],FUN=mean,by=list(Year=InputDat@Year))[,2]
    if(ncol(InputDat@Ts)>1){
        for(i in 2:ncol(InputDat@Ts)){
           if(InputDat@Var=="Precip") Ts<-cbind(Ts,aggregate(InputDat@Ts[,i],FUN=sum,by=list(Year=InputDat@Year))[,2])
           else Ts<-cbind(Ts,aggregate(InputDat@Ts[,i],FUN=mean,by=list(Year=InputDat@Year))[,2])
        }
    }else Ts<-as.matrix(Ts)
   
    InputDat@Ts<-Ts
    InputDat@Year<-unique(InputDat@Year)
    InputDat@Time<-unique(InputDat@Time)
    InputDat@Month<-vector()
    return(InputDat)
}