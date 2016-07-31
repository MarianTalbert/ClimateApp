GetIndicies<-function(Clim,ClimateObj,Boundary,Clip){

   ex<-extent(Boundary)

      Lat<-ncvar_get(Clim,ClimateObj@latName)
      Lon<-ncvar_get(Clim,ClimateObj@lonName)

        if(any(Lon>0)) Lon<-Lon-360

     LatInBox<-which(Lat>=ex@ymin & Lat<=ex@ymax,arr.ind=TRUE)
     LonInBox<-which(Lon>=ex@xmin & Lon<=ex@xmax,arr.ind=TRUE)

if(Clip){
#we have to subset the boundaries because even 12 time steps at a time
    #some ncdfs are too large

  RastArray<- ncvar_get(Clim,start=c(min(LonInBox),min(LatInBox),1),
                        count=c((length(LonInBox)),length(LatInBox),12)) 
                     Lon<-Lon[LonInBox]
                     Lat<-Lat[LatInBox]   
                     dimnames(RastArray)[[1]]<-Lon
                     dimnames(RastArray)[[2]]<-Lat
                    #I can't always clip because not always are there points inside the boundary
                    #figure out the clipping just once because it's quite time consuming
                    ind<-ClipToPolygon(Lon,Lat,(RastArray[,,1]),Boundary,Indicies=TRUE)
                    
                    image(Lon[order(Lon)],Lat[order(Lat)],RastArray[order(Lon),order(Lat),1])
                    RastMat<-RastArray[,,1]
                    RastMat[ind]<-0
                    image.plot(Lon[order(Lon)],Lat[order(Lat)],RastMat[order(Lon),order(Lat)])
                    plot(Boundary,add=TRUE)
                    ind[,1]<-ind[,1]+min(LonInBox)-1
                    ind[,2]<-ind[,2]+min(LatInBox)-1   
                    #because we're reading in a subset of the original raster we have to account for this in our indicies 
                 
                  } 
    else {
                  #this will no longer work because Lat and Lon from the netCDF no longer corresponds to the bounding box
                  ind<-expand.grid(seq(from=min(LonInBox),to=max(LonInBox)),seq(from=min(LatInBox),to=max(LatInBox)))
    }
    return(ind)
    }