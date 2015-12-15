library(shiny)
library(leaflet)
library(RColorBrewer)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map1", width = "100%", height = "50%"),
  leafletOutput("map2",width="50%",height="25%"),
  leafletOutput("map3",width="50%",height="25%"),
  absolutePanel(top = 30, left = 10,
                       selectInput("mapVar", "Variable",
                            choices=c("Precipitation","Temperature","Elevation"),
                            selected="Temperature")
  )
)

server <- function(input, output, session) {
  MapSpecs <- reactiveValues(
    i = 2,
    MapCols = "OrRd",
    Label = "Average Annual Temperature"
  )
  Mapi=reactive({   
     switch(input$mapVar,
                  Temperature = 1,
                  Precipitation = 2,
                  Elevation =3)})
  MapCols=reactive({
      switch(input$mapVar,
                         Temperature = "OrRd",
                         Precipitation = "BuGn",
                         Elevation ="Spectral")
               
  
   })
  MapLab=reactive({
    switch(input$mapVar,
           Temperature = "Average Annual Temperature",
           Precipitation = "Total Annual Precipitation",
           Elevation ="Elevation")
    
    
  })
  
  output$map1 <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
   
    MCols <- MapCols()
    MLab <-MapLab()
    Mi<-Mapi()
    
    pal = colorNumeric(MCols, values(r),
              na.color = "transparent")
   
    leaflet() %>% addTiles() %>%  addRasterImage(r, colors = pal, opacity = 0.8) %>%
      addLegend(pal = pal, values = values(r),
                title = MLab)
  })
}  

 
 

shinyApp(ui, server)
