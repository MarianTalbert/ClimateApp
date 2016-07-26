 
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
                 fluidRow(
                 column(5,
                   selectInput("NationalPark", choices=as.character(NpsLst),
                               label=h4("National Park"),selected="Adams")),
                 column(2,radioButtons("MapUnits", label = h4("Plot Units"),
                              choices = list("Metric (C/mm per month)" = 2,
                                             "US units (F/ inches per month)" = 1), 
                              selected=1
                 )),
                 column(4),
                 column(1,
                        img(src="NCCSClogo.jpg",height=68,width=100),
                        helpText(" "),
                        img(src="USGSLogo.png",height=40,width=100)
                 ),
          style="padding: 5px;"),
       fluidRow(
         column(2,
        selectInput("mapVar", "Variable",
                   choices=c("Precipitation","Temperature","Elevation"),
                   selected="Temperature")),
       column(2,
        selectInput("mapRCP", "Emissions Path",
                   choices=c("High (RCP 8.5)","Mid Low (RCP 4.5)"),
                   selected="Mid Low (RCP 4.5)")),
       column(2,
              selectInput("mapTime", "Time Period",
                          choices=c("1990s","2040s","2080s"),
                          selected="1990s")),
       column(2,sliderInput("mapTrans","Transparency", 0, 1,.8)),    
       column(2,
       checkboxInput("diffFromHist", label = "Show difference from historic period", 
                     value = FALSE))
       ),
         
       leafletOutput("Map",width="100%",height="600px")
              
    ),width=12 
   
        
    )
 ),
   #========================================
   #  Projected Trends  
 tabPanel("Projected Trends",
     sidebarPanel(
            radioButtons("ProjUnits", label = h4("Plot Units"),
              choices = list("Metric (C/mm per month)" = 2,
              "US units (F/ inches per month)" = 1), selected=1
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
           img(src="NCCSClogo.jpg",height=68,width=100),
           helpText(" "),
           img(src="USGSLogo.png",height=40,width=100),
         
         width=2
              
        
        ),
      mainPanel(
        h2(textOutput("projLab")), 
        
          column(10,
              wellPanel(   
                plotOutput("Emissions",width="100%"),
              style="padding: 5px;")), 
          column(2,
                 wellPanel(
                    helpText("Projections of mean annual temperature and total annual precipitation",
                    "are produced using 1/8th degree BCSD data that was downloaded from the Green Data Oasis",
                    "heavy lines indicate mean by RCP. A 5-year rolling average smooth",
                    "is applied to obserational data before plotting.",
                    "The limits of the Ribbon represent the 5%",
                    "and 95% quantiles for all models within a given RCP")
                    ),
                    style="padding: 5px;"), 
         #),
        #style="padding: 5px;"),
        #column(9, 
        
          column(10,
                 wellPanel(
                   plotOutput("ProjBoxplot",width="100%")
                   ),
                   style="padding: 5px;"),
          column(2,
                 wellPanel(
                   helpText("Seasonal Box plots are produced using the",
                   "1/8th degree BCSD data from the GDO.",
                   " Each box is created by calculating ",
                   "the seasonal mean (or total for precipitation) for each run and available model",
                   "within a 20 year period for the given RCP. The boxes represent the 25% to",
                   "75% quantiles and the whiskers extend out to 1.5*IQR (Inter-quantile range).")
                 ),
                 style="padding: 5px;"       
          ),width=10
                 
      )),
   
#====================================================
#======= Historic Trends    
tabPanel("Historic Trends",
    
       sidebarPanel(
         radioButtons("HistUnits", label = h4("Plot Units"),
                      choices = list("Metric (C/mm per month)" = 2,
                                     "US units (F/ inches per month)" = 1), selected=1
         ),
              radioButtons("ObsHist", 
                        label = h4("Observational Data"), 
                        choices = list("Maurer" = "Maurer", 
                           "PRISM" = "Prism",
                           "TopoWx"="TopoWx",
                           "Compare"="CompareHist"
                           ),
                        selected = "CompareHist"),
               radioButtons("HistVar", 
                    label = h3("Variable"), 
                    choices = list("Max Temp" = 1, 
                 "Min Temp" = 2,
                 "Avg Temp" = 3,
                 "Precip" = 4
                  ),
              selected = 1),
             img(src="NCCSClogo.jpg",height=68,width=100),
             helpText(" "),
             img(src="USGSLogo.png",height=40,width=100),
         width=2),
       mainPanel(
         h2(textOutput("histLab")),
         column(10,
                wellPanel(
                   plotOutput("HistoricTrends")
                )
           ),
         column(2,
                wellPanel(
                 helpText("Annual historic line plot for average annual Temperature",
                   "and total precipitation. Options are available to add a 10 year",
                   "rolling average and a linear models fit with 95% confdence interval"),
                 checkboxInput("Trend", label = h6("Add Linear Trend"),
                               value =FALSE),
                 checkboxInput("MovAvg", label = h6("Add Moving Average"),
                               value = TRUE)
                )
         ),
          
           
            
               column(10,
                 wellPanel(      
                  plotOutput("MonthlyLine"),
                  sliderInput("MonthBase", label = h3("Years to use for calculating monthly normals"),
                              min = 1895, max = 2010, value =c(1951,1980),sep="",width="100%")
                )),
               column(2,
                wellPanel(
                helpText("Monthly normals are the monthly mean (or total for precipitatoin)",
                        "of the time series calculated over the specified set of years")
               )
             ),
             
         width=10
        )
 ),
tabPanel("Anomaly Plots",
       sidebarPanel(
         radioButtons("AnomUnits", label = h4("Plot Units"),
                      choices = list("Metric (C/mm per month)" = 2,
                                     "US units (F/ inches per month)" = 1), selected=1
           ),
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
              selected = 1),
             img(src="NCCSClogo.jpg",height=68,width=100),
             helpText(" "),
             img(src="USGSLogo.png",height=40,width=100),
         width=2),
        mainPanel( 
                 h2(textOutput("anomalyLab")),
                 column(10,
                   wellPanel( 
                        plotOutput("AnomalyPlot"))),
                 column(2,
                    wellPanel( 
                        helpText("The yearly anomaly plot shows",
                         "depicts the quantity of interest relative to the mean",
                         "over the selected baseline period."))),
                 column(10,
                        wellPanel( 
                        plotOutput("ImagePlot"),
                        sliderInput("Baseline", label = h4("Baseline Years"),
                                    min = 1895, max = 2010, value =c(1895,1980),
                                    sep="",width="100%"))),
                 column(2,
                        wellPanel( 
                        helpText("The image plot shows the normalized",
                         "difference from a baseline period for each month",
                         "and year.  The baseline is calculated monthly within",
                         "the specified year range.  They pixels are normalized ",
                         "by month and colors range from + or - 2.5 Standard ",
                         "deviations from the mean of the baseline period."))),
                
             width=10
        )
),    

#===============================================
# ==========  Model Scatterplot Tab ==========#    
tabPanel("Projection Scatterplot",
         sidebarPanel(
           radioButtons("ScatterUnits", label = h4("Plot Units"),
                        choices = list("Metric (C/mm per month)" = 2,
                                       "US units (F/ inches per month)" = 1), selected=1
           ),
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
           img(src="NCCSClogo.jpg",height=68,width=100),
           helpText(" "),
           img(src="USGSLogo.png",height=40,width=100),
           
           width=2
         ),
         
         mainPanel(
               h2(textOutput("scatterLab")),
                  plotOutput("ScatterPlot",width="950px",height="850px"),
                fluidRow( column(6,sliderInput("ScatterBase", label = h4("Baseline Years"),
                  min = 1895, max = 2005, value =c(1951,1980),sep="",width="50%")),         
                        column(6,
                  sliderInput("ScatterProj", label = h4("Future Period"),
                  min = 2015, max = 2100, value =c(2050,2070),sep="",width="50%"))
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
         )
) )