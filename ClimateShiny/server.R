
# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  dat<-NA
  #===== Available Shapefiles Update =======#
  observe({
		if(is.null(input$InputFile))
			return()                                                             
     #add any new shapefiles that have been uploaded to the list
     unzip(input$InputFile$datapath,exdir=TempLoc)
     TempShape<-file.path(TempLoc,gsub(".zip","",input$InputFile$name))
     FileList<-list.files(TempShape,full.names=TRUE) 
     ShapeFile<-FileList[match(".shp",substr(FileList,nchar(FileList)-3,nchar(FileList)))]
     ShapeList[[length(ShapeList)+1]]<-ShapeFile
     names(ShapeList)[length(ShapeList)]<-gsub(".zip","",input$InputFile$name)
   
     ShapeList<<-ShapeList #assign this to global
      updateSelectInput(session, "Dataset", choices = names(ShapeList))
		})
	#======================================	
	 # create the map
  values <- reactiveValues(
      rangeIndx=1,
      Shape=NA
  )
  Mapi=reactive({   
    switch(input$mapVar,
           Temperature = 1,
           Precipitation = 2,
           Elevation =3)})
 
  MapLab=reactive({
    switch(input$mapVar,
           Temperature = "Average Annual Temperature (F)",
           Precipitation = "Total Annual Precipitation (In/Yr)",
           Elevation ="Elevation")})
  
  MapLst<-reactive({
    if(input$mapVar=="Temperature") return(ShinyMapLst[[1]])
    if(input$mapVar=="Precipitation") return(ShinyMapLst[[2]])
    if(input$mapVar=="Elevation") return(ShinyMapLst[[3]])
  })
  MapPal=reactive({
    
    values$rangeIndx<-switch(input$mapVar,
                             Temperature = 1,
                             Precipitation = 2,
                             Elevation =3)
  
    if(input$mapVar=="Temperature"& !input$diffFromHist)
      pal = colorNumeric("OrRd", VarRng[[values$rangeIndx]],
                         na.color = "transparent")
    if(input$mapVar=="Temperature"& input$diffFromHist){
      values$rangeIndx<-5

      pal = colorNumeric("YlOrRd", VarRng[[values$rangeIndx]],
                         na.color = "transparent")
    }
    if(input$mapVar=="Precipitation" & !input$diffFromHist)
      pal = colorNumeric("BuGn", VarRng[[values$rangeIndx]],
                         na.color = "transparent")
    if(input$mapVar=="Precipitation" & input$diffFromHist){
      values$rangeIndx<-4
      pal = colorNumeric("BrBG", VarRng[[values$rangeIndx]],
                         na.color = "transparent")
  }
    if(input$mapVar=="Elevation") 
  pal = colorNumeric(terrain.colors(10), VarRng[[values$rangeIndx]],
                     na.color = "transparent")
    return(pal)
  })
  output$Map <- renderLeaflet({
    #This is getting to be a mess I should probably write out the rasters
    #to a the file system and construct their names from the input choices
    TimePeriod<-switch(input$mapTime,
                       "1990s"=1,
                       "2040s"=2,
                       "2080s"=3)
    if(input$mapVar=="Elevation") TimePeriod<-1
    RcpChoice<-1
    if(TimePeriod!=1){
    RcpChoice<-switch(input$mapRCP,
           "High (RCP 8.5)"=2,
           "Mid Low (RCP 4.5)"=1)}
    diffMap<-input$diffFromHist
    dataset <- MapLst()
    Title<-MapLab()
    #colRange<-VarRng[[Mapi]]
    
    if(!inherits(dataset,"list")){ 
      #for elevation the options are a bit more limted
      dataset<-list(list(dataset))
      TimePeriod=1
      diffMap=FALSE
    }
    
    if(TimePeriod!=1 & diffMap){
      if(Title=="Total Annual Precipitation"){
      dataset[[TimePeriod]][[RcpChoice]]<-
         100*(dataset[[TimePeriod]][[RcpChoice]]-dataset[[1]][[1]])/dataset[[1]][[1]]
      Title<-"Percent Change in Precipitation"
      
      }
      else{ 
        
        dataset[[TimePeriod]][[RcpChoice]]<-
          (dataset[[TimePeriod]][[RcpChoice]]-dataset[[1]][[1]])
        Title<-"Change in Temperature"
        
      }
    }
    
   
    MyMap<-leaflet() %>% addTiles() %>%  addRasterImage(dataset[[TimePeriod]][[RcpChoice]], colors = MapPal(), 
                    opacity = input$mapTrans) %>%
      addLegend(pal = MapPal(), values = VarRng[[values$rangeIndx]],title=Title)

         Code<-NpsLst[which(input$NationalPark== NpsLst,arr.ind=TRUE)]
        Bounds<-NP[which(Code==as.character(NP$PARKNAME),arr.ind=TRUE),]
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
  
  #===== Select Attribute Update =======#		
  ds <- reactiveValues(
    MaurerLst=NA,
    PrismLst=NA,
    TopoWxLst=NA,
    GDOLst=NA,
    ParkName=NA
  )
  observe({
     ind<-which(input$NationalPark==NpsLst,arr.ind=TRUE)
     path<-file.path(WorkspacePath,NpsCodes[ind],"StudyWorkspace")
     load(path)
     ds$MaurerLst<-MaurerLst
     ds$PrismLst<-PrismLst
     ds$GDOLst<-GDOLst
     ds$TopoWxLst<-TopoWxLst
     ds$ParkName<-NpsLst[ind]
    
		})

 
#====== Once an Attribute value is selected  add it to the map   

         output$Emissions<-renderPlot({
        if(input$ObsRibbon=="Prism") PastLst<-ds$PrismLst 
        if(input$ObsRibbon=="Maurer") PastLst<-ds$MaurerLst
        if(input$ObsRibbon=="TopoWx") PastLst<-ds$TopoWxLst
        
         if(input$RibbonOrLine=="Ribbon") 
                EmissionSDPlot(ds$GDOLst[[as.numeric(input$Var)]],PastClim=PastLst[[as.numeric(input$Var)]],
                ParkName=ds$ParkName,DisplayOutput=TRUE,OutputGraphics=OutputGraphics,rcp=input$RibbonRCP,
                cexMult=1.2,writeMain=writeMain,Period=5)
         if(input$RibbonOrLine=="Line")
                EmissionLinePlot(ds$GDOLst[[as.numeric(input$Var)]],PastClim=PastLst[[as.numeric(input$Var)]],
                ds$ParkName,DisplayOutput=TRUE,OutputGraphics=OutputGraphics,rcp=input$RibbonRCP,cexMult=1.2,
                writeMain=writeMain,Period=5)
           })
        output$ProjBoxplot<-renderPlot({
        if(input$ObsRibbon=="Prism") PastLst<-ds$PrismLst
        if(input$ObsRibbon=="Maurer") PastLst<-ds$MaurerLst
        if(input$ObsRibbon=="TopoWx") PastLst<-ds$TopoWxLst
       
             BoxplotRCP(InputDat=ds$GDOLst[[as.numeric(input$Var)]],BaseDat=PastLst[[as.numeric(input$Var)]],Baseline=c(1950,1980),
                BarAvg=20,AllOnePlot=TRUE,Col=NA,DisplayOutput=TRUE,
                OutputGraphics=OutputGraphics,cexMult=1.5,writeMain=TRUE,PlotBase=FALSE,RCP=input$RibbonRCP)
  })
 
#==================================
#==== Historic Trends plots  
  output$HistoricTrends<-renderPlot({ 
       if(input$ObsHist=="Prism") PastLst<-list(PRISM=ds$PrismLst[[as.numeric(input$HistVar)]])
       if(input$ObsHist=="Maurer") PastLst<-list(Maurer=ds$MaurerLst[[as.numeric(input$HistVar)]])
       if(input$ObsHist=="TopoWx") PastLst<-list(TopoWx=ds$TopoWxLst[[as.numeric(input$HistVar)]])
       if(input$ObsHist=="CompareHist"){
             PastLst=list(PRISM=ds$PrismLst[[as.numeric(input$HistVar)]],
                    Maurer=ds$MaurerLst[[as.numeric(input$HistVar)]],
                    TopoWx=ds$TopoWxLst[[as.numeric(input$HistVar)]])
                        
                    if(as.numeric(input$HistVar)==4) PastLst<-PastLst[1:2] #no TopoWx for Preci 
                                    }
       TminPlot<-YearlyLinePlot(PastLst,MovAvgPeriod=10,
                   Xlab=(""),
                   MovAvg=input$MovAvg,LM=input$Trend,maCol="blue",
                   DisplayOutput=TRUE,OutputGraphics=OutputGraphics,cexMult=1.6,writeMain=writeMain)
               
  })
 output$MonthlyLine<-renderPlot({ 
       if(input$ObsHist=="Prism") PastLst<-list(PRISM=ds$PrismLst[[as.numeric(input$HistVar)]])
       if(input$ObsHist=="Maurer") PastLst<-list(Maurer=ds$MaurerLst[[as.numeric(input$HistVar)]])
       if(input$ObsHist=="TopoWx") PastLst<-list(TopoWx=ds$TopoWxLst[[as.numeric(input$HistVar)]])
       if(input$ObsHist=="CompareHist"){
             PastLst=list(PRISM=ds$PrismLst[[as.numeric(input$HistVar)]],
                    Maurer=ds$MaurerLst[[as.numeric(input$HistVar)]],
                    TopoWx=ds$TopoWxLst[[as.numeric(input$HistVar)]])
                     if(as.numeric(input$HistVar)==4) PastLst<-PastLst[1:2] #no TopoWx for Precip     
                                    }

      MonthlyLine(Observational=PastLst,Baseline=input$MonthBase,cexMult=1.6,plotLegend=TRUE,
      PlotUnits=PlotUnits[ifelse(input$HistVar==4,2,1)])
               
  })
   
  output$AnomalyPlot<-renderPlot({
     if(input$AnomalyHist=="Prism") PastLst<-ds$PrismLst
       if(input$AnomalyHist=="Maurer") PastLst<-ds$MaurerLst
       if(input$AnomalyHist=="TopoWx") PastLst<-ds$TopoWxLst
    
  
  AnomalyPlot(PastLst[[as.numeric(input$AnomalyVar)]],Baseline=input$Baseline,ParkName=ds$ParkName,
   DisplayOutput=TRUE,OutputGraphics=OutputGraphics,cexMult=2,writeMain=writeMain)
   })
   
   output$ScatterPlot<-renderPlot({ 
     
     scatterMargins(ds$GDOLst$Tavg,ds$GDOLst$ppt,Baseline=input$ScatterBase,rcp=input$ScatterRCP,
                    PlotTime=input$ScatterProj,
                    DisplayOutput=TRUE,OutputGraphics=OutputGraphics,
                    cexMult=1.3,writeMain=TRUE,addLegend=TRUE,Text=input$ScatterText,
                    PlotMargins=input$ScatterMars)
          })
    
    
      output$ImagePlot<-renderPlot({ 
       if(input$AnomalyHist=="Prism") PastLst<-ds$PrismLst
       if(input$AnomalyHist=="Maurer") PastLst<-ds$MaurerLst
       if(input$AnomalyHist=="TopoWx") PastLst<-ds$TopoWxLst
       
        ImagePlot(PastLst[[as.numeric(input$AnomalyVar)]],Baseline=input$Baseline,DisplayOutput=TRUE,OutputGraphics=OutputGraphics,cexMult=2.1,writeMain=writeMain)
       })
      
      output$Maps<-renderPlot({
        plotProgression(BaseLst,Xlen,ylen,variable=as.numeric(input$mapVar),DisplayOutput=TRUE,
                        OutputGraphics,Boundary,PlotBound,PlotState,PlotUnits)
        
      })
   
})   
