load("data/USProjectionMaps.Rdat")

sourceList<-list.files("external",full.name=TRUE)
unlist(lapply(sourceList,source))

WorkspacePath<-"data/RegionSummaries"
NpsShapes ="data/nps_boundary/nps_boundary.shp"

 NP<-readShapePoly(NpsShapes)
 
 #get rid of the noncontiguous states and any unnamed parks
 rmIslands<-c("AK","HI",as.character(unique(NP$STATE)[!unique(NP$STATE)%in%state.abb]))
 
 NpsCodes<-NP$UNIT_CODE[!NP$STATE %in% rmIslands & !is.na(NP$UNIT_NAME)]
 NpsLst<-NP$UNIT_NAME[!NP$STATE %in% rmIslands &  !is.na(NP$UNIT_NAME)]
 
 o<-order(as.character(NpsLst))
 NpsLst<-as.character(NpsLst[o])
 NpsCodes<-as.character(NpsCodes[o])
 #for git just use a subset of all parks
 l<-list.files("data/RegionSummaries")
 ind<-which(NpsCodes%in%l,arr.ind=TRUE)
 NpsLst<-NpsLst[ind]
 NpsCodes<-NpsCodes[ind]

