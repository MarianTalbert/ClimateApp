GenerateColors<-function(mapType){
    #  These functions have some standard color ramps as suggested by Kaye
    #  it sets the break points so that when appropriate a zero change in the
    #  value maps to the neutral color.  Additionally park and state borders that
    #  look good with the color ramp are recorded
    #
    # Layer     = the layer to be mapped
    # mapType   %in% AbsTemp (yellow to red scale)
    #                AbsPrecip (white to blue)
    #                TempChng (blue to red)
    #                PrecipChng (brown to blue\green)
    #                PrplBrn    (Purple to brown)
    #                GrnPnk     (Green to pink scale)
    
    #choose the color ramp
      color<-switch(mapType,
              Elev    = terrain.colors(20),
              Tavg    = brewer.pal(brewer.pal.info["YlOrRd",]$maxcolors,"YlOrRd"),
              Tmin    = brewer.pal(brewer.pal.info["YlOrRd",]$maxcolors,"YlOrRd"),
              Tmax    = brewer.pal(brewer.pal.info["YlOrRd",]$maxcolors,"YlOrRd"),
              Precip  = brewer.pal(brewer.pal.info["GnBu",]$maxcolors,"GnBu"),
              GrnPnk  = rev(brewer.pal(brewer.pal.info["PiYG",]$maxcolors,"PiYG")),
              PurOrn  = brewer.pal(brewer.pal.info["PuOr",]$maxcolors,"PuOr"),
              PrecipChng = rev(c(colorRampPalette(c("turquoise4", "white"))(25),
                     rev(colorRampPalette(c("chocolate4", "white"))(25)))),
              TempChng   = c(colorRampPalette(c("navy", "white"))(50),rev(colorRampPalette(c("red4", "white"))(50)))
            )
     color
}
SetBreaks<-function(layer,mapType,color){    
    #set middle color if looking at change 
   
     if(!mapType%in%c("Tavg","Precip","Tmin","Tmax","Elev")){
      r<-max(abs(range(layer,na.rm=TRUE)))
      Breaks<-seq(from=-r,to=r,length=length(color)+1)
     } else Breaks=seq(from=min(layer,na.rm=TRUE),to=max(layer,na.rm=TRUE),length=length(color)+1)
     
     Breaks  
}      

