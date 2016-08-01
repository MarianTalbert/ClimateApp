GenerateLab<-function(x,addTime=FALSE,addNCDF=FALSE,last,first){
 #this generates some nice labels for either a ClimateTS object
 #or the mapped data object (x)
 
 quant<-switch(x@Var,
              Elev   = "Elevation",
              Tavg    = "Temperature",
              Temp   ="Average~Daily~Temp",
              Tmax     = "Maximum~Daily~Temp",
              Tmin    = "Minimum~Daily~Temp",
              ppt  = "Total~Precip",
              Precip= "Total~Precip",
              GrnPnk     = "",
              PurOrn  ="",
              PrecipChng = "Precipitation~Change",
              TempChng   = "Change~'in'~Temperature")
            if(is.null(quant)) quant=x@Var
           #setting a default label and changing it in a few special cases
         
          if(x@PlotUnits%in%c("in","In")) x@PlotUnits="(Inches/Year)"
          
            Main = paste(quant, x@PlotUnits,sep="~")
           if(toupper(x@PlotUnits)%in%c("C","F","K")){
               Main<-paste(quant, "~({}^o*",x@PlotUnits,")",sep="")
            }
            if(toupper(x@PlotUnits)=="MM"){
               Main<-paste(quant, " ~(mm / Year)",sep="")
            }
            if(x@PlotUnits=="kgm2s1"){
            Main = paste(quant,(kg*m^2*s^1),sep="~")
            }
            if(addTime){ Main = paste(
                     ifelse(length(x@Month==1),paste(month.name[x@Month],"~",sep=""),""),
                     ifelse(length(x@Year==1),x@Year,paste(min(x@Year),"~to~",max(x@Year),sep="")),
                     "~",Main,
                     sep="")
                 }
            if(addNCDF) Main=paste(gsub(".nc","",x@SourcePath),Main,sep="~")
           
            if(!missing(first)) Main=paste(first,Main,sep="~")
            if(!missing(last)) Main=paste(Main,last,sep="~")

           Main = parse(text=Main)
           return(Main)
}

LongName<-function(Var){
     switch(Var,
              Elev   = "Elevation",
              Tavg="Average Temperature",
              Temp="Daily Average Temperature",
              Tmax="Daily Maximum Temperature",
              Tmin="Daily Minimum Temperature",
              ppt="Sum of Annual Precipitation",
              Precip="Sum of Annual Precipitation")
              }