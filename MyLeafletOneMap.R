library(shiny)
library(leaflet)
library(RColorBrewer)
load("C:\\Users\\mtalbert\\Desktop\\Climate\\ParkOutput\\UnitedStates\\studyWorkspace")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("Map", width = "100%", height = "50%"),
   absolutePanel(top = "40%", left = 10,
                       selectInput("mapVar", "Variable",
                            choices=c("Precipitation","Temperature","Elevation"),
                            selected="Temperature")
  )
)

server <- function(input, output, session) {
  
  Mapi=reactive({   
     switch(input$mapVar,
                  Temperature = 1,
                  Precipitation = 2,
                  Elevation =3)})
  MapCols=reactive({
      switch(input$mapVar,
                         Temperature = "OrRd",
                         Precipitation = "BuGn",
                         Elevation ="Spectral")})
  MapLab=reactive({
    switch(input$mapVar,
           Temperature = "Average Annual Temperature",
           Precipitation = "Total Annual Precipitation",
           Elevation ="Elevation")})
  Mi<-isolate(Mapi())
  MapLst<-reactive({
    if(input$mapVar=="Temperature") return(ShinyMapLst[[1]])
    if(input$mapVar=="Precipitation") return(ShinyMapLst[[2]])
    if(input$mapVar=="Elevation") return(as.list(ShinyMapLst[[3]]))
        })
  
    output$Map <- renderLeaflet({
      dataset <- MapLst()
      pal = colorNumeric(MapCols(), values(dataset[[1]]),
                         na.color = "transparent")
      MyMap<-leaflet() %>% addTiles() %>%  addRasterImage(dataset[[1]], colors = MapCols(), opacity = 0.8) %>%
        addLegend(pal =pal, values = values(dataset[[1]]),title="map")
      for(i in 1:length(Bounds@polygons[[1]]@Polygons)){
        coords<-Bounds@polygons[[1]]@Polygons[[i]]@coords
        MyMap<- addPolygons(MyMap,
                            lat=coords[,2],
                            lng=coords[,1],
                            fill=FALSE,
                            layerId=as.character(i))
      
      }
      return(MyMap)
    })
  

}  

shinyApp(ui, server)
