load("C:\\Users\\mtalbert\\Desktop\\Climate\\ParkOutput\\Alabama\\studyWorkspace")

setwd("C:\\GoogleDrive\\Interactive\\Rcode\\Shiny\\ClimateApp")
sourceList<-list("ClimateShiny/external/ChkLibs.r","ClimateShiny/external/GetParkBoundary.r")
unlist(lapply(sourceList,source))
ChkLibs(list("maptools","rgdal","shiny","leaflet","maptools","rgdal","raster","ncdf4","fields","maps",
            "ggplot2","zoo","XML","RColorBrewer","chron"))
#For tomorrow get rasters to work by switching to leaflet

runApp("ClimateShiny")
