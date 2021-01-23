################## installo 
if(!require(BiocManager)){
  install.packages("BiocManager")
  library(BiocManager)
}
if(!require(rhdf5)){
  BiocManager::install(c("rhdf5"))
  library(rhdf5)
}
if(!require(httr)){
  install.packages("httr")
  library(httr)
}
if(!require(raster)){
  install.packages("raster")
  library(raster)
} 

if(!require(jsonlite)){
  install.packages("jsonlite")
  library(jsonlite)
} 

if(!require(XML)){
  install.packages("XML")
  library(XML)
} 

out.crs<-CRS("+init=epsg:6875")
 
## lista di nomi dei file GEDI 
## (estensione h5 - vedi documentazione GDAL https://gdal.org/drivers/raster/hdf5.html) 
granule.names<- list.files(path = "dl_data/", pattern = "GEDI.*.h5", full.names = T, recursive = T)

## per ogni file applicare questa funzione
# granule<-granule.names[[1]]
whereRthePhotons<-function(granule){
  
  cc<-h5ls(granule)
  ph_c<-grep("height*", cc$name) 
  
  if(length(ph_c)>0 && !hit){
    
    for(i in cc[ph_c , "group"] ){
      #i<-cc[ph_c , "group"][[5]]
      print(i)
      nn<-basename(granule)
      extension(nn)<-""
      mydata <- h5read(granule, i) 
      
      ## elimino rumore
      lands<-which( mydata$elevation_bin0>0  )
      
      df.t<- data.frame( X=mydata$longitude_bin0[lands], 
                         Y=mydata$latitude_bin0[lands] )
      
      df.t.df<- data.frame( mydata[ c("elevation_bin0", "elevation_bin0_error",  "height_bin0", "height_lastbin")] )
      if(nrow(df.t)>0) {
        
        mp<-SpatialPointsDataFrame ( df.t, data = df.t.df[lands,], proj4string=CRS("+init=epsg:4326"))
        
        raster::shapefile(mp, "out.tmp/punti", overwrite=T)
        mp32 <- spTransform(mp, out.crs)
        
        df.t<- data.frame(mp32@coords)
        names(df.t)<- toupper(names(df.t))
        df.t$gpstime<- as.double(mydata$heights$delta_time[lands] )
        df.t$Classification<- as.integer( mydata$heights$signal_conf_ph[1,lands] )
        df.t.head <- header_create( df.t )
        
        dd<-sprintf("%s_%s", nn,  substring(i, 2, 10000)  )
        ddd<-sprintf("%s_%s.laz", nn,  substring(i, 2, 10000)  )
        print(sprintf("writing %s", ddd))
        lines[[dd]] <<- Lines(Line(df.t[ c(1,nrow(df.t)), 1:2]),ID=dd)
        
        rlas::write.las( ddd, df.t.head, df.t )
        
      } else {
        print("NOT")
        print(i)
      }
    } 
  } else {
    print("NOTNon contiene punti")
    print(granule)
  }
  
} 

##loop sulla lista dei file ed applica la funzione sopra
whereRthePH<-lapply(granule.names, whereRthePhotons  ) 

fff<-strsplit( names(lines), "_")

aaa <- do.call(rbind, fff)

dff<-data.frame(id=names(lines),aaa)

rownames(dff)<-names(lines)

flights_lines <- SpatialLinesDataFrame(SpatialLines(lines), dff )
crs(flights_lines)<-out.crs

raster::shapefile(flights_lines, "flighLines", overwrite=T)




 




