ShapePath<-ShapeList[[3]]
Shape <- readShapePoly(ShapePath)

Bounds<-GetParkBoundary(Shape,ShapePath,ParkCode="California",UNIT_CODE="STATE",Buffer=NA)

for(i in 1:length(Bounds@polygons[[1]]@Polygons)){
  coords<-Bounds@polygons[[1]]@Polygons[[i]]@coords
  MyMap<-MyMap %>% addPolygons(coords[,2],coords[,1])}

pal = colorNumeric("BuGn", values(ShinyMapLst[[1]][[1]][[1]]),
                   na.color = "transparent")
#I think I just have a conversion issue here
MyMap<-leaflet() %>% addTiles() %>%  addRasterImage(ShinyMapLst[[1]][[1]][[1]], colors = pal, opacity = 0.8) %>%
  addLegend(pal = pal, values = values(ShinyMapLst[[1]][[1]][[1]]),
            title = "Map")
for(i in 1:length(Bounds@polygons[[1]]@Polygons)){
  coords<-Bounds@polygons[[1]]@Polygons[[i]]@coords
  MyMap<- addPolygons(MyMap,
    lat=coords[,2],
    lng=coords[,1],
    fill=FALSE,
    layerId=as.character(i))
}


coords<-Bounds@polygons[[1]]@Polygons[[i]]@coords
MyMap %>% addPolygons(coords[,2],coords[,1])

pal = colorNumeric("BuGn", c(92,10),ordered=TRUE,
                   na.color = "transparent")
leaflet() %>% addTiles() %>%  addRasterImage(ShinyMapLst[[1]][[1]][[1]], colors = pal, opacity = 0.8) %>%
  addLegend(pal = pal, values = c(92,10),
            title = "Map")
v<-values(ShinyMapLst[[1]][[1]])
hist(v)
v1<-values(ShinyMapLst[[1]][[2]])
v2<-values(ShinyMapLst[[1]][[3]])
str(ShinyMapLst[[1]][[2]])
