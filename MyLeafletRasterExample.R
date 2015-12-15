library(shiny)
library(leaflet)
library(RColorBrewer)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map1", width = "100%", height = "50%"),
  leafletOutput("map2",width="50%",height="25%"),
  leafletOutput("map3",width="50%",height="25%"),
  absolutePanel(top = 10, left = 10,
                       selectInput("mapVar", "Variable",
                            choices=c("Precipitation","Temperature","Elevation"),
                            selected="Temperature")
  )
)

server <- function(input, output, session) {
 
  output$map1 <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
   
    if(input$mapVar == "Temperature") pal = colorNumeric("OrRd", values(r),
              na.color = "transparent")
   
    if(input$mapVar == "Precipitation") pal =  colorNumeric("BuGn", values(r),
                                               na.color = "transparent")
    
      
    if(input$mapVar == "Elevation")    pal = colorNumeric("Spectral", values(r),
                                                na.color = "transparent")
   
    leaflet() %>% addTiles() %>%  addRasterImage(r, colors = pal, opacity = 0.8) %>%
      addLegend(pal = pal, values = values(r),
                title = "Surface temp")
  })
}  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
 
 

shinyApp(ui, server)
