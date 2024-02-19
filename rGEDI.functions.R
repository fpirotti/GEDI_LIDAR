 
# library(terra)
library(sf)

if(!require(rGEDI)){
  devtools::install_git("https://github.com/carlos-alberto-silva/rGEDI", dependencies = TRUE)
  
  library(rGEDI)
}

isTruthy<-function (x) 
{
  if (inherits(x, "try-error")) 
    return(FALSE)
  if (!is.atomic(x)) 
    return(TRUE)
  if (is.null(x)) 
    return(FALSE)
  if (length(x) == 0) 
    return(FALSE)
  if (all(is.na(x))) 
    return(FALSE)
  if (is.character(x) && !any(nzchar(stats::na.omit(x)))) 
    return(FALSE)
  if (inherits(x, "shinyActionButtonValue") && x == 0) 
    return(FALSE)
  if (is.logical(x) && !any(stats::na.omit(x))) 
    return(FALSE)
  return(TRUE)
}
 
rGEDI.read<-function( path ){
  
  if(!file.exists(path)){
    warning("File does not exist")
    return(NULL)
  }
  
  bn<-basename(path)
  raster::extension(bn)<-""
  
  ss<-strsplit(bn,"_")[[1]]
  product.type<- paste0("L", as.integer( sub("GEDI","", ss[1]) ), ss[2]  )
  
  if(product.type=="L1B"){
    gedi<-readLevel1B(level1Bpath = path)
  }
  if(product.type=="L2A"){
    gedi<-readLevel2A(level2Apath = path)
  }
  if(product.type=="L2B"){
    gedi<-readLevel2B(level2Bpath = path)  
  }
  
  gedi
  
}


#' Clip GEDI
#'
#' @param what2clip either opened h5 file or path to file
#' @param overwrite if file exists, should overwrite?
#' @param ... either should provide xmin, xmax, ymin, ymax in this order or spatial bounding box in SF geometry
#'
#' @return a path to the clipped object, which will have the same name of the original with appended to file name xmin_xmax_ymin_ymax 
#' @export
#'
#' @examples #none
rGEDI.clip<-function( what2clip, overwrite=F, ... ){
  clipper<-list(...)
  
  if(is.null(clipper)) {
    warning("No clipper polygon or other data")
    return(NULL)
  }
  
  
  if( is.element("sfc", class( clipper[[1]] )) ){
    clipper<-sf::st_bbox(sf::st_boundary( clipper[[1]]  ))
  }
  
  if( is.element("bbox", class( clipper[[1]] )) || length(clipper[[1]])==4 ){
    clipper<-clipper[[1]]
  }  else {
    warning("Problem with bound box, should be an SFC element or xmin, xmax, ymin, ymax")
    return(NULL)
  }
  
  
  if(length(clipper)==4 ){
    
    xmin<-clipper[["xmin"]]
    xmax<-clipper[["xmax"]]
    ymin<-clipper[["ymin"]]
    ymax<-clipper[["ymax"]]
    
  }
  
  if( !(isTruthy(xmin) & 
        isTruthy(xmax) &
        isTruthy(ymin) &
        isTruthy(ymax) ) ){
    warning("Missing one corner, should provide xmin, xmax, ymin, ymax in this order or spatial bounding box in SF geometry")
    return(NULL)
    
  }
  if( !(xmin<xmax) &
      !(ymin<ymax)){
    warning("xmin > xmax or ymin > ymax check your input")
    return(NULL)
  }
  
  if(is.character(what2clip)){
    what2clip<-rGEDI.read(what2clip)
  }
  
  if(!grepl("gedi\\.level", class(what2clip)) ){
    warning("File not found or not an HDF class!")
    return(NULL)
  }
  
  bn<-basename(what2clip@h5$filename)
  outdir<-dirname(what2clip@h5$filename)

  raster::extension(bn)<-""
  ss<-strsplit(bn,"_")[[1]]
  product.type<- paste0("L", as.integer( sub("GEDI","", ss[1]) ), ss[2]  )

  bn.out<-sprintf("%s/%s_clip_%s_%s_%s_%s.h5", outdir, bn,
                  gsub("\\.","_", sprintf("%.4f",xmin)), 
                  gsub("\\.","_", sprintf("%.4f",xmax)), 
                  gsub("\\.","_", sprintf("%.4f",ymin)), 
                  gsub("\\.","_", sprintf("%.4f",ymax)) )
      
  if(file.exists(bn.out)&& !overwrite){
    
    tmp <- tryCatch({ 
        rGEDI.read("bn.out") 
        }, 
        error=function(x){ x },
        warning=function(x){ x }
      )
    
    if( sum(c("error", "warning") %in% class(tmp))==0 ){
      rGEDI::close(tmp)
      warning("File exists, returning, if you want to overwrite add \"overwrite=TRUE\" ")
      return(bn.out)
    }
    
    warning("File exists but is not HDF5 so I am overwriting")
    
  }
  
  message("Clipping ", product.type, " with ", "xmin:", xmin, " xmax:", xmax, 
          " ymin:", ymin, " ymax:",  ymax)
  
  if(product.type=="L1B"){
    gedi.clipped <- clipLevel1B(what2clip, xmin, xmax, ymin, ymax,
                                output=bn.out)
  }
  if(product.type=="L2A"){
    gedi.clipped <- clipLevel2A(what2clip, xmin, xmax, ymin, ymax, 
                                output=bn.out)
  }
  if(product.type=="L2B"){
    gedi.clipped <- clipLevel2B(what2clip, xmin, xmax, ymin, ymax,
                                output=bn.out)
  }
  
  rGEDI::close(what2clip)
  
  return(bn.out)
   
}
 

#' Title
#'
#' @param infile either rGEDI hd5 opened file or path to h5 file 
#'
#' @return returns date object
#' @export
#'
#' @examples #none
rGEDI.dateFromFileName<-function(infile){  
  strptime(substr(basename(infile), 10,22), "%Y%j%H%M%S")
}

#' Title
#'
#' @param infile either rGEDI hd5 opened file or path to h5 file
#' @param saveFormat either "shapefile"/"SHP" or "geopackage"/"GPKG" 
#' @param overwrite TRUE or FALSE overwrite if file exists?
#' @param clipPoly simple feature from sf library that must be a polygon.
#' @param ...  if a polygon is provided, it will clip to that polygon
#'
#' @return returns SF object with geometries, if "saveformat" is not null, 
#' output file is created in the same folder and returns path to written file
#' @export
#'
#' @examples #none
rGEDI.toGeom<-function(infile, saveFormat="GPKG", 
                       overwrite=FALSE, outdir=NA, 
                       clipPoly=NULL, ...){
  
  if(file.exists(infile)){
    infile<-rGEDI.read(infile)
  }
  
  if(!grepl("gedi\\.level", class(infile)) ){
    warning("File not found and not an HDF class!")
    return(NULL)
  }
  
  bn<-basename(infile@h5$filename)
  dir<-dirname(infile@h5$filename) 
  bn<-tools::file_path_sans_ext(bn)
  # raster::extension(bn)<-""
  ss<-strsplit(bn,"_")[[1]]
  product.type<- paste0("L", as.integer( sub("GEDI","", ss[1]) ), ss[2]  )
  
  if(is.na(outdir)) { 
    bn.out<- file.path( dir, bn )
  } else {
    
    bn.out<- file.path( outdir, bn )
    if( !dir.exists(dirname(bn.out)) ){
      message("Creating directory ", dirname(bn.out) )
      dir.create(dirname(bn.out), showWarnings = F, recursive = T, mode = "0777")
      Sys.chmod(dirname(bn.out), mode = "0777", use_umask = F)
    }
  }
  
  if( tolower(saveFormat)=="shp" || tolower(saveFormat)=="shapefile"){
    bn.out<-paste0(bn.out, ".shp")
  }
  else if( tolower(saveFormat)=="gpkg" || tolower(saveFormat)=="geopackage"){
    bn.out<-paste0(bn.out, ".gpkg")
  }
  else {
    # warning("You must put a save format either gpkg or shape")
    if(grepl("gedi\\.level", class(infile))) rGEDI::close(infile)
    stop("You must put an output format: either gpkg or shape")
  }  
  
  if(file.exists(bn.out)&& overwrite==F){
    message("File ", bn.out , " exists, returning")
    if(grepl("gedi\\.level", class(infile))) rGEDI::close(infile)
    return(sprintf("%s EXISTS", bn.out))
  }
  
  if(!file.exists(bn.out)&& overwrite==F){
    overwrite=TRUE
  }
  
  if(product.type=="L1B"){
    dd <- getLevel1BGeo(level1b=infile)
    # dd$shot_number<-paste0(dd$shot_number)
    gedi.geom<-sf::st_as_sf( na.omit(dd, cols=c("longitude_bin0", "latitude_bin0")),coords=c("longitude_bin0", "latitude_bin0"), crs=4326)
  }
  if(product.type=="L2A"){ 
    dd <- getLevel2AM(level2a =infile)
    # dd$shot_number<-paste0(dd$shot_number)
    
    gedi.geom<-sf::st_as_sf(na.omit(dd, cols=c("lon_lowestmode", "lat_lowestmode")),coords=c("lon_lowestmode", "lat_lowestmode"), crs=4326)
  }
  if(product.type=="L2B"){
     dd<- rGEDI::getLevel2BVPM(level2b = infile)
     gedi.geom <-  dd[ , c("longitude_bin0", "latitude_bin0", "elev_lowestmode", "elev_highestreturn")]
     gedi.geom<-sf::st_as_sf(gedi.geom,coords=c("longitude_bin0", "latitude_bin0"), crs=4326)
  } 
  

  
  if(!is.null(clipPoly) ){ 
    if( !is.element("sfc", class(clipPoly )) && !is.element("sf", class(clipPoly )) ){ 
      # warning("Clip element must be an sf or sfc  element.") 
      stop("Clip element must be an sf or sfc  element.")
    } 
    if( !is.element("sfc_POLYGON", class(clipPoly )) && 
        !is.element("sfc_POLYGON", class(clipPoly[[1]] )) ){
      warning("Clip element must be a POLYGON, something wrong, will try to clip anyway but check results")  
 
    } 
    sf::sf_use_s2(FALSE)
    message("Clipping to polygon(s)... might take a while")
    gedi.geom <- sf::st_intersection(gedi.geom, 
                                     sf::st_transform(clipPoly, sf::st_crs(gedi.geom) ) )
 
    
  }
  sf::write_sf(gedi.geom, bn.out, delete_layer = T )    
  return(sprintf("DONE %s", bn.out))
}


