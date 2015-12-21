library(shiny)
library(leaflet)
library(RColorBrewer)
load("C:\\Users\\mtalbert\\Desktop\\Climate\\ParkOutput\\UnitedStates\\USMaps")
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("Maps1", width = "100%", height = "50%"),
  fluidRow(
    column(6,leafletOutput("Maps2",width="100%")),
    column(6,leafletOutput("Maps3",width="100%"))),
  fluidRow(
    column(6,leafletOutput("Maps4",width="100%")),
    column(6,leafletOutput("Maps5",width="100%"))),
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
     })
 
  lapply(1:length(ShinyMapLst[[Mi]]),function(i){
    output[[paste("Maps",i,sep="")]] <- renderLeaflet({
      dataset <- MapLst()
     # browser()
      pal = colorNumeric(MapCols(), values(dataset[[i]]),
                         na.color = "transparent")
      leaflet() %>% addTiles() %>%  addRasterImage(dataset[[i]], colors = pal, opacity = 0.8) %>%
        addLegend(pal = pal, values = values(dataset[[i]]),
                  title = input$mapVar)
    })
  })

}  

shinyApp(ui, server)
