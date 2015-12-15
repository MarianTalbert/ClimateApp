library(shiny)
library(leaflet)
library(RColorBrewer)
load("C:\\Users\\mtalbert\\Desktop\\Climate\\ParkOutput\\UnitedStates\\USMaps")
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map1", width = "100%", height = "50%"),
  leafletOutput("Maps2",width="50%",height="25%"),
  leafletOutput("Maps3",width="50%",height="25%"),
  leafletOutput("Maps4",width="50%",height="25%"),
  leafletOutput("Maps5",width="50%",height="25%"),
  absolutePanel(top = 30, left = 10,
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
  MapLst<-
 
  lapply(1:length(ShinyMapLst[[Mi]]),function(i){
    print(Mi)
    output[[paste("Maps",i,sep="")]] <- renderLeaflet({        
      pal = colorNumeric(MapCols(), values(ShinyMapLst[[Mi]][[i]]),
                         na.color = "transparent")
      leaflet() %>% addTiles() %>%  addRasterImage(ShinyMapLst[[Mi]][[i]], colors = pal, opacity = 0.8) %>%
        addLegend(pal = pal, values = values(ShinyMapLst[[Mi]][[i]]),
                  title = input$mapVar)
    })
  })
  
  output$map1 <- renderLeaflet({
   
    
    pal = colorNumeric(MapCols(), values(ShinyMapLst[[Mi]][[1]]),
              na.color = "transparent")
   
    leaflet() %>% addTiles() %>%  addRasterImage(ShinyMapLst[[Mi]][[1]], colors = pal, opacity = 0.8) %>%
      addLegend(pal = pal, values = values(ShinyMapLst[[Mi]][[1]]),
                title = MapLab())
  })
  
}  

shinyApp(ui, server)
