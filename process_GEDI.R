
for(i in dl.files){
  if( dir.exists(i) ) {
    warning("Skipping directory")
    next
  }
  if( raster::extension(i)!=".h5" ) {
    warning("Skipping as it is not HDF5 file")
    next
  }
  
  clipped<-rGEDI.clip(i, italy.bounds)
  mf<-rGEDI.read(i)
  
  bn<-basename(i)
  
  raster::extension(bn)<-""
  
  ss<-strsplit(bn,"_")[[1]]
  product.type<- paste0("L", as.integer( sub("GEDI","", ss[1]) ), ss[2]  )
  timestamp<- strptime( substr(ss[3], 1, 7), "%Y%j", tz="UTC")
  
  if( is.null(dl.dictionary[[bn]]) ) next
  
}