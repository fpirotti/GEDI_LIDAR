library(rGEDI)
library(raster)

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


#' Title
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
    warning()
    return(NULL)
  }
  
  
  if( is.element("sfc", class( clipper[[1]] )) ){
    clipper<-sf::st_bbox(sf::st_boundary( clipper[[1]]  ))
  }
  if( is.element("bbox", class( clipper[[1]] )) ){
    clipper<-clipper[[1]]
  }

  if(length(clipper)==4 ){
    
    xmin<-clipper[[1]]
    xmax<-clipper[[2]]
    ymin<-clipper[[3]]
    ymax<-clipper[[4]]
    
  } else {
    warning("Problem with bound box, should be an SFC element or xmin, xmax, ymin, ymax")
    return(NULL)
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
    warning("File exists, returning, if you want to overwrite add \"overwrite=TRUE\" ")
    return(bn.out)
  }
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
#' @param saveFormat either "shapefile"/"SHP" or "geopackage"/"GPKG" 
#' @param overwrite TRUE or FALSE overwrite if file exists?
#'
#' @return returns SF object with geometries, if "saveformat" is not null, 
#' output file is created in the same folder and returns path to written file
#' @export
#'
#' @examples #none
rGEDI.toGeom<-function(infile, saveFormat=NULL, overwrite=F){
  
  if(file.exists(infile)){
    infile<-rGEDI.read(infile)
  }
  
  bn<-basename(infile@h5$filename)
  outdir<-dirname(infile@h5$filename) 
  raster::extension(bn)<-""
  ss<-strsplit(bn,"_")[[1]]
  product.type<- paste0("L", as.integer( sub("GEDI","", ss[1]) ), ss[2]  )
  
  bn.out<-sprintf("%s/%s", outdir, bn )
  
  if(product.type=="L1B"){
    dd <- getLevel1BGeo(level1b=infile,select=c("elevation_bin0", "elevation_lastbin"))
    dd$shot_number<-paste0(dd$shot_number)
    gedi.geom<-sf::st_as_sf(dd,coords=c("lon_lowestmode", "lat_lowestmode"), crs=4326)
  }
  if(product.type=="L2A"){ 
    dd <- getLevel2AM(level2a =infile)
    dd$shot_number<-paste0(dd$shot_number)
    gedi.geom<-sf::st_as_sf(dd,coords=c("longitude_bin0", "latitude_bin0"), crs=4326)
  }
  if(product.type=="L2B"){
     dd<- rGEDI::getLevel2BVPM(level2b = infile)
     gedi.geom <-  dd[ , c("longitude_bin0", "latitude_bin0", "elev_lowestmode", "elev_highestreturn")]
     gedi.geom<-sf::st_as_sf(gedi.geom,coords=c("longitude_bin0", "latitude_bin0"), crs=4326)
  } 
  
  if(is.null(saveFormat)) return(NULL)
  
  if( tolower(saveFormat)=="shp" || tolower(saveFormat)=="shapefile"){
    bn.out<-paste0(bn.out, ".shp")
  }
  if( tolower(saveFormat)=="gpkg" || tolower(saveFormat)=="geopackage"){
    bn.out<-paste0(bn.out, ".gpkg")
  }
  sf::write_sf(gedi.geom, bn.out, delete_layer=overwrite )    
  return(bn.out)
}