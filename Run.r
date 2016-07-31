setwd("C:\\GoogleDrive\\Interactive\\Rcode\\Shiny\\ClimateApp")


ChkLibs(list("rgdal","shiny","leaflet","maptools","rgdal","raster","ncdf4","fields","maps",
            "ggplot2","zoo","XML","RColorBrewer","chron","gridExtra","plotrix","jpeg","Cairo"))

library(shiny)
library(maptools)
library(leaflet)
library(RColorBrewer)
library(ggplot2)
library(raster)
library(Cairo)
library(gridExtra)
setwd("C:\\GoogleDrive\\Interactive\\Rcode\\Shiny\\ClimateApp")
runApp("ClimateShiny")

