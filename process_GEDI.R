
for(i in dl.files){
  if( dir.exists(i) ) {
    warning("Skipping directory")
    next
  }
  if( raster::extension(i)!=".h5" ) {
    warning("Skipping as it is not HDF5 file")
    next
  }
  
  clipped<-rGEDI.clip(i, F, italy.bounds)
  mdata<-rGEDI.read(clipped)
 
  # Converting shot_number as "integer64" to "character"
  level1bGeo$shot_number<-paste0(level1bGeo$shot_number)
  head(level2BVPM
  # Converting level1bGeo as data.table to SpatialPointsDataFrame
  library(sp)
  level1bGeo_spdf<-SpatialPointsDataFrame(cbind(level1bGeo$longitude_bin0, level1bGeo$latitude_bin0),
                                          data=level1bGeo)
  
  
}