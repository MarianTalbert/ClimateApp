 
LatLst<-as.list(seq(from=29,to=50))
LonLst<-as.list(seq(from=-125,to=-67))
names(LatLst)<-seq(from=29,to=50) 
names(LonLst)<-seq(from=-125,to=-67) 
# Define UI for application that draws a histogram
shinyUI(navbarPage("Climate Primer",
#===============================================
# ==========  Study Area Specification Tab ==========#
 tabPanel("Specify Study Area",

        #========= Main Panel================#
    mainPanel(
       wellPanel(              
           h2("Specify a bounding box for the study"),       
         fluidRow(
            column(2,h3("Latitude:   From")),
           column(1,
               selectInput("LatStart",choices=LatLst, 
                   selected = 29,label="")),
            column(2,
              selectInput("LatSDec",choices=list(".0625" = .0625, ".1875" = .1875, ".3125" = .3125), 
                 selected = 1,label="")),
            column(1,h3("To")),
             column(1,
               selectInput("LatEnd",choices=LatLst, 
              selected = 50,label="")),                       
             column(2,
              selectInput("LatEtDec",choices=list(".0625" = .0625, ".1875" = .1875, ".3125" = .3125), 
                 selected = 1,label=""))
          ),
         fluidRow(
           column(2,h3("Longitude:   From")),
           column(1,
               selectInput("LonStart",choices=LonLst, 
                  selected =-125,label="")),
            column(2,
               selectInput("LonSDec",choices=list(".0625" = .0625, ".1875" = .1875, ".3125" = .3125), 
                  selected = 1,label="")),
             column(1,h3("To")),
             column(1,
               selectInput("LonEnd",choices=LonLst, 
              selected = -67,label="")),                       
             column(2,
               selectInput("LonEtDec",choices=list(".0625" = .0625, ".1875" = .1875, ".3125" = .3125), 
                  selected = 1,label=""))     
           ),
          
        
                h2("or upload a shapefile"),
                helpText("Please either select from the available", 
                      "shapefiles, upload the desired file or specify a bounding box."),
                 fluidRow(
                 column(2,
                    selectInput("Dataset", choices=names(ShapeList),label=h4("Available Shapefiles"))),
                 column(5,
                    fileInput("InputFile", label = h4("Please point to the .zip containing the shapefile"))),
                 column(2,
                   selectInput("Attribute", label=h4("Select Attribute"),"Loading...")), 
                  column(3,  
                   selectInput("AttributeValue", label = h4("Select the Attribute Value"), 
                  "Loading..."))),
          actionButton("DisplayShape", label = "Display study area on map"),
          style="padding: 5px;"),       
       fluidRow(
         column(2,
        selectInput("mapVar", "Variable",
                   choices=c("Precipitation","Temperature","Elevation"),
                   selected="Temperature")),
       column(2,
        selectInput("varTime", "Time Period",
                   choices=c("1990s","2040s","2080s"),
                   selected="1990s")),
       column(2,
       checkboxInput("diffFromHist", label = "Show difference from historic period", value = FALSE))
       ),
         
       leafletOutput("Map"),
         img(src="NCCSClogo.jpg",height=170,width=220)      
              
    )
   
        
    ),
    
   #========================================
   #  Projected Trends  
 tabPanel("Projected Trends",
     sidebarPanel(
                                                
         h2("Settings that Apply for all Graphics"),
            textInput("ParkName", label = h4("Study area name for graphics"), 
              value = "Enter text ..."),
			
            radioButtons("PlotUnits", label = h4("Plot Units"),
              choices = list("US units (F/ inches per month)" = "c(\"F\",\"In\")",
                              "Metric (C/mm per month)" = "c(\"C\",\"mm\")")
                        ),
              radioButtons("Var", 
                    label = h3("Variable"), 
                    choices = list("Max Temp" = 1, 
                 "Min Temp" = 2,
                 "Avg Temp" = 3,
                 "Precip" = 4
                  ),
              selected = 1),
              
              checkboxGroupInput("RibbonRCP", 
                  label = h4("RCPs for Plotting"), 
                  choices = list("RCP 2.6" = "RCP 2.6", 
                     "RCP 4.5" = "RCP 4.5",
                     "RCP 6.0" = "RCP 6.0",
                     "RCP 8.5" = "RCP 8.5"
                     ),
                  selected = c("RCP 2.6","RCP 4.5","RCP 6.0","RCP 8.5")),
                   
                 radioButtons("ObsRibbon", 
                        label = h4("Add Observational Data"), 
                        choices = list("Maurer" = "Maurer", 
                           "PRISM" = "Prism",
                           "TopoWx"="TopoWx"
                           ),
                        selected = "Maurer"),
         radioButtons("RibbonOrLine", 
                      label = h4(""), 
                      choices = list("Ribbon" = "Ribbon", 
                                     "Line" = "Line"
                      ),
                      selected = "Ribbon"), 
         width=2
              
        
        ),
      mainPanel(
        column(7,
        wellPanel(              
              h2("Projected Trends"),
                          
                plotOutput("Emissions"),style="padding: 5px;", height="350px",
                    helpText("Projections of mean annual temperature and total annual precipitation",
                    "are produced using 1/8th degree BCSD data that was downloaded from the Green Data Oasis",
                    "heavy lines indicate mean by RCP. A 5-year rolling average smooth",
                    "is applied to obserational data before plotting.",
                    "The limits of the Ribbon represent the 5% and 95% quantiles for all models within a given RCP")
                    
         ),
        style="padding: 5px;"),
        column(9, 
        wellPanel(           
                h2("Projected Trends by Season"),
                plotOutput("ProjBoxplot"),style="padding: 5px;", height="350px",
                   helpText("Seasonal Box plots are produced using the",
                   "1/8th degree BCSD data from the GDO.",
                   " Each box is created by calculating ",
                   "the seasonal mean (or total for precipitation) for each run and available model",
                   "within a 20 year period for the given RCP. The boxes represent the 25% to",
                   "75% quantiles and the whiskers extend out to 1.5*IQR (Inter-quantile range).")
                
        ),
        style="padding: 5px;")       
                 
        )
      ),
   
#====================================================
#======= Historic Trends    
tabPanel("Historic Trends",
    
       sidebarPanel(
              radioButtons("ObsHist", 
                        label = h4("Observational Data"), 
                        choices = list("Maurer" = "Maurer", 
                           "PRISM" = "Prism",
                           "TopoWx"="TopoWx",
                           "Compare"="CompareHist"
                           ),
                        selected = "Maurer"),
               radioButtons("HistVar", 
                    label = h3("Variable"), 
                    choices = list("Max Temp" = 1, 
                 "Min Temp" = 2,
                 "Avg Temp" = 3,
                 "Precip" = 4
                  ),
              selected = 1),width=2),
       mainPanel(
         column(6, 
            wellPanel(
              h2("Historic trend plots"),
               checkboxInput("Trend", label = h6("Add Linear Trend"),
                                   value = TRUE),
                  checkboxInput("MovAvg", label = h6("Add Moving Average"),
                                   value = TRUE),                                         
                plotOutput("HistoricTrends"),
                helpText("Annual historic line plot for average annual Temperature",
                   "and total precipitation. Options are available to add a 10 year",
                   "rolling average and a linear models fit with 95% confdence interval")
             ),
            style="padding: 5px;"),
           column(6,
             wellPanel(
                sliderInput("MonthBase", label = h2("Years to use for calculating monthly normals"),
                            min = 1895, max = 2010, value =c(1951,1980),format="#",width="100%"),    
                plotOutput("MonthlyLine"),
                helpText("Monthly normals are the monthly mean (or total for precipitatoin)",
                        "of the time series calculated over the specified set of years")
             ),
             style="padding: 5px;")
        )
 ),
tabPanel("Anomaly Plots",
       sidebarPanel(
              radioButtons("AnomalyHist", 
                        label = h4("Observational Data"), 
                        choices = list("Maurer" = "Maurer", 
                           "PRISM" = "Prism",
                           "TopoWx"="TopoWx"
                           ),
                        selected = "Maurer"),
               radioButtons("AnomalyVar", 
                    label = h3("Variable"), 
                    choices = list("Max Temp" = 1, 
                 "Min Temp" = 2,
                 "Avg Temp" = 3,
                 "Precip" = 4
                  ),
              selected = 1),width=2),
        mainPanel( 
             
             wellPanel(
                   h2("Anomaly Plots"),
                   div(class="row",    
                    div(class="span5",       
                        div(class="span8",plotOutput("AnomalyPlot")), 
                        div(class="span8",plotOutput("ImagePlot")), 
                    sliderInput("Baseline", label = h4("Baseline Years"),
                            min = 1895, max = 2010, value =c(1895,1980),format="#",width="100%"))
                      
               ))
        )
),    

#===============================================
# ==========  Model Scatterplot Tab ==========#    
tabPanel("Projection Scatterplot",
         sidebarPanel(
           checkboxGroupInput("ScatterRCP", 
           label = h4("RCPs for Plotting"), 
           choices = list("RCP 2.6" = "RCP 2.6", 
                          "RCP 4.5" = "RCP 4.5",
                          "RCP 6.0" = "RCP 6.0",
                          "RCP 8.5" = "RCP 8.5"),
           selected = c("RCP 2.6","RCP 4.5","RCP 6.0","RCP 8.5")),
           checkboxInput("ScatterMars", label = h6("Add model densities"),
                         value = TRUE),
           checkboxInput("ScatterText", label = h6("Use model names rather than numbers"),
                         value = TRUE),
           
           width=2
         ),
         
         mainPanel(
                 h2("Scatterplots showing how models compare for the region of interest"),
                  plotOutput("ScatterPlot",width="750px",height="650px"),
                fluidRow( column(6,sliderInput("ScatterBase", label = h4("Baseline Years"),
                  min = 1895, max = 2005, value =c(1951,1980),format="#",width="50%")),         
                        column(6,
                  sliderInput("ScatterProj", label = h4("Future Period"),
                  min = 2015, max = 2100, value =c(2050,2070),format="#",width="50%"))
              )
              
              
     )),
#===============================================
# ==========  About Tab ==========#           
  tabPanel("About",
    	mainPanel(
                #img(src="NCCSClogo.jpg",height=250,width=250),
                
                
                includeHTML("include.html"),
                 img(src="CCSClogo.jpg",height=170,width=220)  
            )
         ),   
tabPanel("Other",    
     sidebarPanel(position="right",
            h1("Map Specification"),
            #helpText("Please either select from the available", 
             #     "shapefiles or upload the desired file.")
            selectInput("MappedData", label = h3("Dataset to use"), 
                choices = list("PRISM" = 1, "Maurer" = 2,
                       "1/8th degree BSCD projections" = 3,"NEX"=4), selected = 1),
             selectInput("MappedVar", label = h4("Variable"), 
                choices = list("Avg. Annual Temperature" = "Tmp", "Precipitation"="Pr","Available Color Scales"="Co"), selected = 1),
              selectInput("MappedSubset", label = h4("Seasonal Subset"), 
                choices = list("FullYear"=1,"Jan" = 2, "Feb" = 3,
                       "March" = 4,"April"=5), selected = 1),
              checkboxGroupInput("MapRCP", 
                  label = h4("RCPs for Plotting"), 
                  choices = list("RCP 2.6" = 1, 
                     "RCP 4.5" = 2,
                     "RCP 6.0" = 3,
                     "RCP 8.5" = 4
                     ),
                  selected = 1),
              sliderInput("BaselineTime", label = h4("Baseline Years"),
              min = 1895, max = as.numeric(as.character(format(Sys.time(),"%Y"))), value =c(1895,2010),format="#",width="100%"),         
              
              sliderInput("FutureTime1", label = h4("Future Period 1"),
              min = 2015, max = 2100, value =c(2030,2060),format="#",width="50%"),
              
              sliderInput("FutureTime2", label = h4("Future Period 2"),
              min = 2015, max = 2100, value =c(2070,2100),format="#",width="50%"),
              
              selectInput("ColorScale", label = h4("Color Scale"), 
                choices = list("Default"=0,"yellow to red" = 1, "teal to blue" = 2,
                       "red to blue" = 3,"brown to blue"=4,"green to red"=4,"brown to purple"=4), selected = 0),         
                       
              checkboxGroupInput("AddBoundaries", 
                  label = h4("Add boundaries"), 
                  choices = list("Shape Boundary" = 1, 
                     "State Boundary" = 2
                     ),
                  selected = 1), 
            width=2
                                                      
             )     
                        
  )
) )