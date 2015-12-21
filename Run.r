load("C:\\Users\\mtalbert\\Desktop\\Climate\\ParkOutput\\Alabama\\studyWorkspace")
#save the image with this once I have it just how I like it
for(i in 1:length(BaseLst)){
  for(j in 1:length(BaseLst[[1]])){
    for(k in 1:length(BaseLst[[1]][[1]])){
      BaseLst[[i]][[j]][[k]]<-convertToRaster(BaseLst[[i]][[j]][[k]])
    }}}

setwd("C:\\GoogleDrive\\Interactive\\Rcode\\Shiny\\ClimateApp")
sourceList<-list("ClimateShiny/external/ChkLibs.r","ClimateShiny/external/GetParkBoundary.r")
unlist(lapply(sourceList,source))
ChkLibs(list("maptools","rgdal","shiny","leaflet","maptools","rgdal","raster","ncdf4","fields","maps",
            "ggplot2","zoo","XML","RColorBrewer","chron","gridExtra","plotrix"))
#For tomorrow get rasters to work by switching to leaflet

runApp("ClimateShiny")
