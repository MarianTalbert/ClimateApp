#Fort Globals
ShapeList<-list(PleaseSelect="",
                NpsShapes= "C:\\Users\\mtalbert\\Desktop\\Climate\\InputLayers\\NPS_SmallParksBuffered\\SmallParksBuffered.shp",
                StateBounds = "C:\\Users\\mtalbert\\Desktop\\Climate\\InputLayers\\StateBounds\\statep010.shp")
TempLoc<-"C:\\Users\\mtalbert\\Desktop\\Climate\\InputLayers\\Temp"

WorkspacePath<-"C:\\Users\\mtalbert\\Desktop\\Climate\\ParkOutput\\NPSSummary"
 NpsShapes =file.path("C:\\Users\\mtalbert\\Desktop\\Climate\\InputLayers\\nps_boundary\\nps_boundary.shp")

 NP<-readShapePoly(NpsShapes)
 
 #get rid of the noncontiguous crap and any parks that they couldn't be bothered to name
 rmIslands<-c("AK","HI",as.character(unique(NP$STATE)[!unique(NP$STATE)%in%state.abb]))
 
 NpsCodes<-NP$UNIT_CODE[!NP$STATE %in% rmIslands & !is.na(NP$UNIT_NAME)]
 NpsLst<-NP$UNIT_NAME[!NP$STATE %in% rmIslands &  !is.na(NP$UNIT_NAME)]
 o<-order(as.character(NpsLst))
 NpsLst<-as.character(NpsLst[o])
 NpsCodes<-as.character(NpsCodes[o])

# NpsShp$PARKNAME
# NP$PARKNAME