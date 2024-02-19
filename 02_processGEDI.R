library("sf") 
source("rGEDI.functions.R")
library(doParallel)
myCluster <- makeCluster(8) # type of cluster
registerDoParallel(myCluster)

# load(file="blocchi.poly.rda")

# areas <- do.call(rbind, lapply(blocchi.poly, st_sf))
areas <- read_sf("/archivio/shared/geodati/LAS/LASAVEPA/ortofoto.shp")
un <- sf::st_union(areas$geometry, by_feature = FALSE)  
areas<-un
# load(file="blocchi.poly.rda")
granule.names.h5<- list.files(path = "dl_data/GEDIv8", pattern = "GEDI.*.h5", full.names = T, recursive = T)
granule.names.gpkg <- list.files(path = "", pattern = "GEDI.*.gpkg", full.names = T, recursive = T)
left <-
  which( !(tools::file_path_sans_ext(granule.names.h5) %in% 
         tools::file_path_sans_ext(granule.names.gpkg)) )
## Here we read and clip GEDI data to polygons

res <- foreach(gn = granule.names.h5[-left])  %dopar% {
  rGEDI.toGeom(gn, clipPoly=areas, overwrite=FALSE, outdir="/archivio/shared/R/GEDI_LIDAR/dl_data/GEDIv8/gpkg/") 
}





granule.names<- list.files(path = "dl_data/GEDI/gpkg",  full.names = T, recursive = T)


lapply(granule.names, function(rr){ 
  sf_use_s2(FALSE)
  message("Clipping to polygon(s)... might take a while")
  gedi.geom <- sf::read_sf(rr)
  gedi.geom.c <- sf::st_intersection(gedi.geom, areas)
  if(! (nrow(gedi.geom.c)>1)){
    message("No points in area")
    return(NA)
  }
   sf::write_sf(gedi.geom.c, 
                            sprintf("dl_data/GEDI/%s", 
                                    basename(rr)) )
  
  }) 

# rGEDI.dateFromFileName(granule.names)
