load("C:\\Users\\mtalbert\\Desktop\\Climate\\ParkOutput\\UnitedStates\\USProjectionMaps")
#load("C:\\Users\\mtalbert\\Desktop\\Climate\\ParkOutput\\Alabama\\studyWorkspace")
load("C:\\Users\\mtalbert\\Desktop\\Climate\\ParkOutput\\Colorado\\studyWorkspace")
#For tomorrow consistent color scales across all maps THERE SHOULD BE FIVE HERE I FORGOT RCP


setwd("C:\\GoogleDrive\\Interactive\\Rcode\\Shiny\\ClimateApp")
sourceList<-list("ClimateShiny/external/ChkLibs.r","ClimateShiny/external/GetParkBoundary.r")
unlist(lapply(sourceList,source))

ChkLibs(list("maptools","rgdal","shiny","leaflet","maptools","rgdal","raster","ncdf4","fields","maps",
            "ggplot2","zoo","XML","RColorBrewer","chron","gridExtra","plotrix"))


runApp("ClimateShiny")
