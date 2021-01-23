library(rGEDI)
library(raster)
library(functools)
 

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


rGEDI.clip<-function( what2clip, ... ){
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
  
  if( !(functools::Truthy(xmin) & 
        functools::Truthy(xmax) &
        functools::Truthy(ymin) &
        functools::Truthy(ymax) ) ){
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

  bn.out<-sprintf("%s//%s_clip_%s_%s_%s_%s.h5", outdir, bn,
                  gsub(".","_", as.character(xmin)), 
                  gsub(".","_", as.character(xmax)), 
                  gsub(".","_", as.character(ymin)), 
                  gsub(".","_", as.character(ymax)) )
      
  if(product.type=="L1B"){
    gedi.clipped <- clipLevel1B(what2clip, xmin, xmax, ymin, ymax,
                                output=)
  }
  if(product.type=="L2A"){
    gedi.clipped <- clipLevel2A(what2clip, xmin, xmax, ymin, ymax, 
                                output=paste0(outdir,"//level2a_clip_bb.h5"))
  }
  if(product.type=="L2B"){
    gedi.clipped <- clipLevel2B(what2clip, xmin, xmax, ymin, ymax,
                                output=paste0(outdir,"//level2b_clip_bb.h5"))
  }
  
   
}