#These classes should help to allow methods for local datasets and those pulled down from threads
#because the storage and naming conventions of each dataset is unique this will help provide flexibility to new
#datasets
setClass("climateDefinitions",
      representation(TimeOrigin ="character",
              Calander="character",Path="character",Tavg ="character", 
              Tmin ="character",Tmax ="character",ppt="character",
              TavgFileTag="character",TminFileTag="character",
              TmaxFileTag="character",pptFileTag="character",
              TavgName ="character",TminName ="character",
              TmaxName="character",pptName="character",
              latName = "character",lonName = "character",
              timeName = "character")
)  

setClass("Maurer",
       representation(),
       contains="climateDefinitions", 
       prototype(TimeOrigin='1940-01-01T00:00:00Z',     
              Calander="Gregorian",
              Path="E:/ClimateCache/Maurer/maurer/extracted/hydro.engr.scu.edu/files/gridded_obs/monthly",
              Tavg ="C",Tmin ="C",Tmax ="C",ppt="mm_day",
              TavgFileTag="tas",TminFileTag="tasmin",TmaxFileTag="tasmax",pptFileTag="pr",
              TavgName ="tas",TminName ="tasmin",TmaxName="tasmax",pptName="pr",
              latName = "latitude",lonName = "longitude",timeName = "time"
      )
)

setClass("TopoWx",
       representation(),
       contains="climateDefinitions", 
       prototype(TimeOrigin='1948-01-01T00:00:00Z',     
              Calander="Gregorian",
              Path="E:/ClimateCache/TopoWx/Source/ftp_mirror/mco.cfc.umt.edu/resources/TopoWx-source/mthly_mosaics/by_year",
              Tavg ="C",Tmin ="C",Tmax ="C",ppt="NA",
              TavgFileTag="NA",TminFileTag="tmin",TmaxFileTag="tmax",pptFileTag="NA",
              TavgName ="NA",TminName ="tmin",TmaxName="tmax",pptName="NA",
              latName = "lat",lonName = "lon",timeName = "time"
      )
)

setClass("Prism",
       representation(),
       contains="climateDefinitions", 
       prototype(TimeOrigin= "1858-11-17T00:00:00Z",     
              Calander="Gregorian",
              Path="E:/ClimateCache/PRISM/DerivedData/MergedNetCDF",
              Tavg ="C",Tmin ="C",Tmax ="C",ppt="mm",
              TavgFileTag="tmean",TminFileTag="tmin",TmaxFileTag="tmax",pptFileTag="ppt",
              TavgName ="tmn",TminName ="tmn",TmaxName="tmn",pptName="tmn",
              latName = "lat",lonName = "lon",timeName = "time"
      )
)

setClass("Hydro",
       representation(),
       contains="climateDefinitions", 
       prototype(TimeOrigin= "1950-01-01T00:00:00Z",     
              Calander="Gregorian",
              Path="J:/GIS_Layers/Climate/ClimateCache/GDO/BCSD/ftp_mirror/gdo-dcp.ucllnl.org/pub/dcp/archive/cmip5/hydro/BCSD_mon_VIC_nc",
              Tavg ="C",Tmin ="C",Tmax ="C",ppt="mm_day",
              TavgFileTag="tas",TminFileTag="tasmin",TmaxFileTag="tasmax",pptFileTag="pr",
              TavgName ="tas",TminName ="tasmin",TmaxName="tasmax",pptName="pr",
              latName = "latitude",lonName = "longitude",timeName = "time",
              ETFileTag="et",SWEFileTag="swe",SMCFileTag="smc",
              ETName="et",SWEName="swe",SMCName="smc",
              ET="mm",SMC="mm",SWE="mm"
      )
)

setClass("BCSDCMIP5",
       representation(),
       contains="climateDefinitions", 
       prototype(TimeOrigin= "1950-01-01T00:00:00Z",     
              Calander="Gregorian",
              Path="E:/Climate/ClimateCache/GDO/BCSD/ftp_mirror/gdo-dcp.ucllnl.org/pub/dcp/archive/cmip5/bcsd/BCSD",
              Tavg ="C",Tmin ="C",Tmax ="C",ppt="mm_day",
              TavgFileTag="tas",TminFileTag="tasmin",TmaxFileTag="tasmax",pptFileTag="pr",
              TavgName ="tas",TminName ="tasmin",TmaxName="tasmax",pptName="pr",
              latName = "latitude",lonName = "longitude",timeName = "time"
      )
)

setClass("RawCMIP5",
       representation(),
       contains="climateDefinitions", 
       prototype(TimeOrigin= "1950-01-01T00:00:00Z",     
              Calander="Gregorian",
              Path="J:/GIS_Layers/Climate/ClimateCache/GDO/BCSD/ftp_mirror/gdo-dcp.ucllnl.org/pub/dcp/archive/cmip5/global_mon/regrid",
              Tavg ="C",Tmin ="C",Tmax ="C",ppt="mm_day",
              TavgFileTag="tas",TminFileTag="tasmin",TmaxFileTag="tasmax",pptFileTag="pr",
              TavgName ="tas",TminName ="tasmin",TmaxName="tasmax",pptName="pr",
              latName = "latitude",lonName = "longitude",timeName = "time"
      )
)

#================================================================
# getList method for getting the file list

if (!isGeneric("getList")) {
  setGeneric("getList", function(x,...){
  standardGeneric("getList")
 })
 }
 
setMethod("getList",signature(x="climateDefinitions"),
  function(x,VarName,...){
    FileList<-list.files(x@Path,recursive=TRUE,full.names=TRUE)
 })
 
  setMethod("getList",signature(x="Hydro"),
  function(x,VarName,RCP,...){
        
       FileList<-read.csv("C:\\GoogleDrive\\Climate\\GDOmodellist\\Hydro.csv")
      FileList<-as.character(FileList$x)
      # FileList<-list.files(x@Path,recursive=TRUE,full.names=TRUE,pattern=".nc")
     
       #only use one run per model but first figure out which are extra runs
       Base<-basename(as.character(FileList))
       SplitFile<-strsplit(basename(FileList),"_")
      
       Model<-gsub("c5.","",unlist(lapply(SplitFile,"[",2)))
       Var<-unlist(lapply(SplitFile,"[",4))
           VarYear<-strsplit(Var,split="\\.")
           Var<-unlist(lapply(VarYear,"[",3))
           Year<-as.numeric(unlist(lapply(VarYear,"[",4)))
      
       rcp<-unlist(lapply(SplitFile,"[",3)) 
       rcp<-factor(rcp,labels=c("rcp85","rcp60","rcp45","rcp26"),ordered=TRUE)
       rcp<-c("RCP 2.6","RCP 4.5","RCP 6.0","RCP 8.5")[rcp]
        
       ModelRCPVarYear<-paste(Model,rcp,Var,Year,sep="_")


      #now subset to the correct variable and RCP
       brokeNCDFs<-which((Model=="inmcm4" & rcp=="RCP 8.5" & Var=="swe") |
              (Model=="ccsm4" & rcp=="RCP 6.0" & Var=="swe") |
              (Model=="ipsl-cm5a-mr" & rcp=="RCP 4.5" & Var=="swe"),arr.ind=TRUE)
       Keep<-(Var==VarName & rcp%in%c(RCP))
       Keep[brokeNCDFs]<-FALSE
       Model=Model[Keep]
       rcp=rcp[Keep]
       rcpModel<-paste(rcp,Model,sep="_")


       return(list(FileList=FileList[Keep],Year=Year[Keep],
          ModelRCPVar=substr(ModelRCPVarYear,start=1,stop=(nchar(ModelRCPVarYear)-5))[Keep],
          rcp=rcp[!duplicated(rcpModel)],Model=Model[!duplicated(rcpModel)]))
 })
 
 setMethod("getList",signature(x="BCSDCMIP5"),
  function(x,VarName,RCP,...){
    
       FileList<-read.csv("C:\\GoogleDrive\\Climate\\GDOmodellist\\GDO.csv")
      FileList<-as.character(FileList$x)
      # FileList<-list.files(x@Path,recursive=TRUE,full.names=TRUE,pattern=".nc")
       
       #only use one run per model but first figure out which are extra runs
       Base<-basename(as.character(FileList))
       SplitFile<-strsplit(basename(FileList),"_")
      
       Model<-unlist(lapply(SplitFile,"[",5))
       Var<-unlist(lapply(SplitFile,"[",3))
       rcp<-unlist(lapply(SplitFile,"[",6))
       rcp<-factor(rcp,labels=c("rcp85","rcp60","rcp45","rcp26","historical"),ordered=TRUE)
       rcp<-c("historical","RCP 2.6","RCP 4.5","RCP 6.0","RCP 8.5")[rcp]
      
       ModelRCPVar<-paste(Model,rcp,Var,sep="_")
        Unique<-!duplicated(ModelRCPVar)
       FileList<-FileList[Unique]
       #now subset to the correct variable and RCP
       Var<-Var[Unique]
       rcp<-rcp[Unique]
      
       FileList<-FileList[Var==VarName & rcp%in%c(RCP,"historical")]
       return(FileList)
 })
 
setMethod("getList",signature(x="RawCMIP5"),
  function(x,VarName,RCP,...){
   
      FileList<-read.csv("C:\\GoogleDrive\\Climate\\GDOmodellist\\GDORaw.csv")
      FileList<-as.character(FileList$x) 
       #only use one run per model but first figure out which are extra runs
       Base<-basename(as.character(FileList))
       SplitFile<-strsplit(basename(FileList),"_")
      
       Model<-unlist(lapply(SplitFile,"[",5))
       Var<-unlist(lapply(SplitFile,"[",3))
       rcp<-unlist(lapply(SplitFile,"[",6))
       rcp<-factor(rcp,labels=c("rcp85","rcp60","rcp45","rcp26","historical"),ordered=TRUE)
       rcp<-c("historical","RCP 2.6","RCP 4.5","RCP 6.0","RCP 8.5")[rcp]
      
       ModelRCPVar<-paste(Model,rcp,Var,sep="_")
        Unique<-!duplicated(ModelRCPVar)
       FileList<-FileList[Unique]
       #now subset to the correct variable and RCP
       Var<-Var[Unique]
       rcp<-rcp[Unique]
      
       FileList<-FileList[Var==VarName & rcp%in%c(RCP,"historical")]
       return(FileList)
 })
  
setMethod("getList",signature(x="Maurer"),
  function(x,VarName,...){
  
      FileList<-list.files(x@Path,recursive=TRUE,full.names=TRUE)
       
      #restrict to files 
      FileList<-FileList[grep(paste(".",VarName,".",sep=""),FileList)]
      if(VarName=="tas"){ #clean this up someday when time is abundant 
         FileList<-FileList[-c(grep("tasmin",FileList))]
         FileList<-FileList[-c(grep("tasmax",FileList))]
      }
      Names<-basename(FileList)
      Year<-as.numeric(substr(Names,start=nchar(Names)-6,stop=nchar(Names)-3)) 
       FileList<-FileList[order(Year)] 
        return(FileList)
 })
 
setMethod("getList",signature(x="TopoWx"),
  function(x,VarName,...){
  
      FileList<-list.files(x@Path,recursive=TRUE,full.names=TRUE)
      
      #restrict to files 
      FileList<-FileList[grep(VarName,FileList)]
      Names<-basename(FileList)
      Year<-as.numeric(substr(Names,start=nchar(Names)-6,stop=nchar(Names)-3)) 
       FileList<-FileList[order(Year)] 
        return(FileList)
 }) 
  
setMethod("getList",signature(x="Prism"),
  function(x,VarName,...){
  
  FileList<-list.files(x@Path,recursive=TRUE,full.names=TRUE)
  
  #restrict to files
   
  FileList<-FileList[grep(paste(VarName,"_",sep=""),FileList)]
  return(FileList)
 })

