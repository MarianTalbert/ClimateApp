ConvertTS<-function(x,FromUnits,ToUnits,Month){

IsTsObj<-FALSE
if(inherits(x,"ClimateTS")){
   FromUnits<-toupper(x@PlotUnits)
   #save the time series obhect
   Ts<-x
   x<-x@Ts
   Ts@PlotUnits<-ToUnits
   IsTsObj<-TRUE
}

#Just some standard unit conversions 
    result<-switch(paste(FromUnits,ToUnits,sep="to"),
      FtoC = (x - 32)/1.8,
      FtoK = (x + 459.67)/1.8,
      CtoF =  x*1.8 + 32,
      CtoK = x  + 273.15,
      KtoC =  x - 273.15,
      KtoF =   x*1.8 - 459.67,
      INtoMM =  x*25.4,
      MMtoIN  = x*.03937007874
      )
   
    if(paste(FromUnits,ToUnits,sep="to")=="MM_DAYtoMM"){
    #for gdo BCSD convert mm/day to mm/month...
    DaysPerMonth<-c(31,28.25,31,30,31,30,31,31,30,31,30,31)[Month]
     result<-x*DaysPerMonth
    }
    
    if(paste(FromUnits,ToUnits,sep="to")=="MM_DAYtoIN"){
    #for gdo BCSD convert mm/day to mm/month...
    DaysPerMonth<-c(31,28.25,31,30,31,30,31,31,30,31,30,31)[Month]
     result<-x*DaysPerMonth*.03937007874 #inches/mm
    }
    
    if(is.null(result)){
      warning("Conversion was not defined")
      return(x)
    }
    if(IsTsObj){
       Ts@Ts<-result
       return(Ts)
    }
   return(result) 
 }
 
