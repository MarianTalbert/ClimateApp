load("C:\\Users\\mtalbert\\Desktop\\Climate\\ParkOutput\\UnitedStates\\USProjectionMapsNew.Rdat")

writeMain=TRUE

#FormalBackgrounds or Watermark branch of git for climate graphics code
 setwd(file.path("C:\\GoogleDrive\\Climate","Rcode"))
    sourceList<-list("ChkLibs.r","InputConstants.r","ClipToPolygon.r",
    "MyPlotSetup.r","GenerateColors.r","Convert.R","GenerateLabel.r",
    "GetParkBoundary.r","GetParkBox.r","dataClassesAndMethods.r",
    "LocalTSClassAndMethods.r","ClassesAndMethods.R",
    "YearlyLinePlot.r","AnomalyPlot.r","ImagePlot.r",
    "PlotMappedDataClass.R","BatchClimateTs.r",
    "EmissionsQuant.r","EmissionsLinegg.r","MonthlyLine.r","CurvesColorForTime.r",
    "MyBoxPlotByRCP.r","LMTable.r","ChangeTable.r","changeAlpha.R",
    "dataClassesAndMethods.r","combineBCSD.r","GetIndicies.r","getTime.r","BioclimTs.r","SubsetTs.r",
    "my.filled.contour.r","my.image.plot.r","scatterMargins.r","scatterplotProgression.R","plotProgression.r","ElevationMap.r")
    unlist(lapply(sourceList,source))


setwd("C:\\GoogleDrive\\Interactive\\Rcode\\Shiny\\ClimateApp")
sourceList<-list("ClimateShiny/external/ChkLibs.r","ClimateShiny/external/GetParkBoundary.r")
unlist(lapply(sourceList,source))

ChkLibs(list("maptools","rgdal","shiny","leaflet","maptools","rgdal","raster","ncdf4","fields","maps",
            "ggplot2","zoo","XML","RColorBrewer","chron","gridExtra","plotrix","jpeg"))

Watermark <- readJPEG("C:\\GoogleDrive\\Interactive\\Rcode\\Shiny\\ClimateApp\\ClimateShiny\\www\\LogoFade.jpg")
runApp("ClimateShiny")

