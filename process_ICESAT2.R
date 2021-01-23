library("BiocManager")
BiocManager::install(c("rhdf5"))
library(httr)
library(rhdf5)
library(raster)
library(jsonlite)
library(XML)
library(rlas)

token.RetSTring<-NULL
getToken<-function(){
  token<-"curl --silent -X POST --header \"Content-Type: application/xml\" -d \"<token><username>tesaf</username><password>Libero17</password><client_id>NSIDC_client_id</client_id><user_ip_address>95.140.134.234</user_ip_address> </token>\" https://cmr.earthdata.nasa.gov/legacy-services/rest/tokens"
  res<-system(token,  intern = T)
  resj<- xmlToList(res)
  token.RetSTring<<-resj 
  resj$id
  }

# res<-system("curl --tlsv1.2 --silent https://www.howsmyssl.com/a/check",  intern = T  )
# resj<- jsonlite::parse_json(res)
# granule.link<-"https://n5eil01u.ecs.nsidc.org/DP7/ATLAS/ATL03.001/2018.11.10/ATL03_20181110013714_06480106_001_01.h5?_ga=2.253602688.842378166.1561128208-1381519981.1542137752"
# granule.link.parsed<-parse_url(granule.link)
# granule.name<-basename(granule.link.parsed$path)
# bb<-"-71.524100,-38.672400,-71.494414,-38.664972"

 
mydata.curl<-function(bb, sname="ATL03"){
parameters<-list(
  #email="no",
     #format="NetCDF4-CF",
     short_name=sname,
     version="001",
     time="2015-09-03T00:00:00,2019-11-28T23:59:59",
     bounding_box=bb,
     bbox=bb,
   #  email="no",
     token=getToken()
     )
 sprintf("curl -O -J --dump-header response-header.txt  \"https://n5eil02u.ecs.nsidc.org/egi/request?%s\"",
                paste0( sprintf("%s=%s", names(parameters), parameters), collapse = "&")) 
}

oo<-tryCatch({
   # system(mydata.curl("-71.5338,-38.6884,-71.4024,-38.6178"),      intern = T)
   # system(mydata.curl("-71.5338,-38.6884,-71.4024,-38.6178", "ATL08"),      intern = T)
        },
         error=function(err){
           print(err) 
         }, 
         warning=function(warn){
           print(warn)
 })

#zip.names<- list.files(pattern = ".zip")
#sapply( zip.names, unzip  )
granule.names<- list.files(pattern = "processed_ATL08[A-Za-z0-9_-]*.h5", recursive = T)

earth <- c("land", "ocean", "sea ice", "land ice", "inland water")
out.crs<-crs("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
hit<-F
lines<-list()
whereRthePH<-lapply(granule.names, function(granule){
  cc<-h5ls(granule)
  ph_c<-grep("heights", cc$name) 
  # ph_c<-grep("segment_ph_cnt", cc$name) 
  if(length(ph_c)>0 && !hit){
    
    for(i in cc[ph_c , "group"] ){
      print(i)
      nn<-basename(granule)
      extension(nn)<-""
      mydata <- h5read(granule, i) 
 
      ## elimino rumore
      lands<-which( t(mydata$heights$signal_conf_ph[1,])>1)
 
      df.t<- data.frame( X=mydata$heights$lon_ph[lands], 
                         Y=mydata$heights$lat_ph[lands], 
                         Z= mydata$heights$h_ph[lands] )
  
      if(nrow(df.t)>0) {
        
        mp<-SpatialPoints( df.t, proj4string=CRS("+init=epsg:4326"))
        
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
}) 

fff<-strsplit( names(lines), "_")

aaa <- do.call(rbind, fff)

dff<-data.frame(id=names(lines),aaa)

rownames(dff)<-names(lines)

flights_lines <- SpatialLinesDataFrame(SpatialLines(lines), dff )
crs(flights_lines)<-out.crs

raster::shapefile(flights_lines, "flighLines", overwrite=T)




 




