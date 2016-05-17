load("C:\\Users\\mtalbert\\Desktop\\Climate\\ParkOutput\\UnitedStates\\USProjectionMaps")

#FormalBackgrounds branch of git
 setwd(file.path("C:\\GoogleDrive\\Climate","Rcode"))
    sourceList<-list("ChkLibs.r","InputConstants.r","ClipToPolygon.r",
    "MyPlotSetup.r","GenerateColors.r","Convert.R","GenerateLabel.r",
    "GetParkBoundary.r","GetParkBox.r",
    "LocalTSClassAndMethods.r","ClassesAndMethods.R",
    "YearlyLinePlot.r","AnomalyPlot.r","ImagePlot.r",
    "PlotMappedDataClass.R","BatchClimateTs.r",
    "EmissionsQuant.r","EmissionsLinegg.r","MonthlyLine.r","CurvesColorForTime.r",
    "MyBoxPlotByRCP.r","LMTable.r","ChangeTable.r",
    "dataClassesAndMethods.r","combineBCSD.r","GetIndicies.r","getTime.r","BioclimTs.r","SubsetTs.r",
    "my.filled.contour.r","my.image.plot.r","scatterMargins.r","scatterplotProgression.R","plotProgression.r","ElevationMap.r")
    unlist(lapply(sourceList,source))

setwd("C:\\GoogleDrive\\Interactive\\Rcode\\Shiny\\ClimateApp")
sourceList<-list("ClimateShiny/external/ChkLibs.r","ClimateShiny/external/GetParkBoundary.r")
unlist(lapply(sourceList,source))

ChkLibs(list("maptools","rgdal","shiny","leaflet","maptools","rgdal","raster","ncdf4","fields","maps",
            "ggplot2","zoo","XML","RColorBrewer","chron","gridExtra","plotrix"))


runApp("ClimateShiny")
