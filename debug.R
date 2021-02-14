library(shiny)
library(plotly)
library(leaflet)
library(raster)
library(leaflet.extras)
library(shinyalert)
library(hdf5r)
library(sf)
library(doParallel)  
library(foreach)
library(tools)
library(tmap)
library("rnaturalearth")
library("rnaturalearthdata")
library(gdalUtils)
library(KernSmooth)
source("rGEDI.functions.R")



tmpwd<-getwd()
setwd("/archivio/shared/R/GEDI_LIDAR")

world <- ne_countries(scale = "medium", returnclass = "sf")
italy <- world[world$name_sort=="Italy",]$geometry
italy.bounds <- sf::st_bbox(sf::st_boundary( italy  ))
italy.nord.bounds<-  c(xmin=6.6278, xmax=15.1011, ymin=43.7549, ymax=47.0821)
save(italy, italy.bounds, italy.nord.bounds, file="italy.nord.bounds.rda")

load("italy.nord.bounds.rda")
ul_lat<- italy.nord.bounds[["ymax"]]
ul_lon<- italy.nord.bounds[['xmin']]
lr_lat<- italy.nord.bounds[['ymin']]
lr_lon<- italy.nord.bounds[['xmax']]



maxlatlon<-0.05

daterange<-c("2018-01-01", format(Sys.Date(), "%Y-%m-%g") )
# gLevel1B<-gedifinder(product="GEDI01_B",ul_lat, ul_lon, lr_lat, lr_lon,version="001",daterange=daterange)
# gLevel2A<-gedifinder(product="GEDI02_A",ul_lat, ul_lon, lr_lat, lr_lon,version="001", daterange=daterange)
# gLevel2B<-gedifinder(product="GEDI02_B",ul_lat, ul_lon, lr_lat, lr_lon,version="001", daterange=daterange)
# save(gLevel1B, gLevel2A, gLevel2B, file="gLevels.rda")

load("gLevels.rda")

if(file.exists("dl.dictionary.rds")) dl.dictionary<-readRDS("dl.dictionary.rds") else dl.dictionary<-list()

outdir<-"dl_data/GEDI"
# notClippedFiles<- list.files(path=outdir, pattern = ".*_01\\.h5$", include.dirs = F )
# ## clipping the unclipped
# for(i in notClippedFiles){
#   fp<- file.path(outdir, basename(i)) 
#   cl<-rGEDI.clip( fp,   overwrite = T, italy.nord.bounds )
#   file.remove(fp)
# }
# 
# dl.files<- list.files(path=outdir, pattern = ".*\\.h5$", include.dirs = F, full.names = F)
# dl.files<-substr(dl.files, 1, 46) 
# 
# 
# tmp.files<- list.files(path=outdir, pattern = ".*\\.curltmp$", include.dirs = F, full.names = T )
# file.remove(tmp.files)




dl.files<- list.files(path=outdir, pattern = ".*\\.h5$", include.dirs = F, full.names = F)
dl.files<-substr(dl.files, 1, 46) 
todownload<-which( !(tools::file_path_sans_ext(basename(gLevel1B))%in%dl.files)) 
length(unique(gLevel2A)) 
length(unique(dl.files))
length(todownload)


#  id<-mcparallel({ gediDownload(filepath=gLevel1B[[1]], outdir=outdir) })
# mccollect(id, wait = F)
# tools::pskill(id$pid)


basenames.files.to.download<-substr(basename(gLevel1B), 1, 46) 

geo.files<- list.files(path=file.path(outdir,"gpkg"), pattern = "\\.gpkg$",  full.names = T)
clipped.files<- list.files(path=file.path(outdir), 
                           pattern = ".*_clip_.*\\.h5$", include.dirs = F, full.names = T)

downloaded.full.files<- list.files(path=outdir, pattern = "_01\\.h5$", include.dirs = F, full.names = T)
downloaded.full.files.to.remove<-  downloaded.full.files[ which(  (substr(basename(downloaded.full.files), 1, 46)%in% substr(basename(clipped.files), 1, 46) ) )] 
downloaded.full.files.to.remove<-  downloaded.full.files[ which(  (substr(basename(downloaded.full.files), 1, 46)%in% substr(basename(geo.files), 1, 46) ) )] 
downloaded.full.files.NOT.to.remove<-  downloaded.full.files[ which(  !(substr(basename(downloaded.full.files), 1, 46)%in% substr(basename(geo.files), 1, 46) ) )] 
file.remove(downloaded.full.files.to.remove)
(list.files(outdir, pattern = 
              tools::file_path_sans_ext(basename(downloaded.full.files.NOT.to.remove[[1]])) ,
           include.dirs = F, 
           recursive = T, full.names = F))

file.remove(downloaded.full.files.to.remove)

toclip<- downloaded.full.files[ which(  !(substr(basename(downloaded.full.files), 1, 46)%in% substr(basename(clipped.files), 1, 46) ) )] 


todownload<- which(   !(basenames.files.to.download %in% substr(basename(clipped.files),  1, 46)) &
                      !(basenames.files.to.download %in% substr(basename(downloaded.full.files.NOT.to.remove),  1, 46)) )

clipped.files.yesGeo.notRemovedFullDownload<- setdiff( tools::file_path_sans_ext(basename(geo.files)), 
                             tools::file_path_sans_ext(basename(clipped.files)) )


missing.geo.files<- setdiff( tools::file_path_sans_ext(basename(clipped.files)), 
                             tools::file_path_sans_ext(basename(geo.files)) )

missing.clipped.files.yesgeo<- setdiff( tools::file_path_sans_ext(basename(geo.files)), 
                             tools::file_path_sans_ext(basename(clipped.files)) )
 

registerDoParallel(cores=10) 
cl <- makeCluster(10, type="FORK")

output <- foreach(i = downloaded.full.files.NOT.to.remove,
                  .packages = c("sf", "rGEDI", "tools"))  %dopar%  {
                    
                    fp<- file.path(outdir, basename(i))
                    #gediDownload(filepath=i,outdir=outdir)
                    cl<-rGEDI.clip( fp, overwrite = T, italy.nord.bounds )
                    if(is.character(cl)) file.remove(fp) 
                    #rGEDI.toGeom(cl, saveFormat = "gpkg", overwrite = F,  outdir = "gpkg", italy.nord.bounds)
                    cl
                  } 
stopCluster(cl)



  

#clipped.files<- list.files(path=outdir, pattern = ".*_clip_.*\\.h5$", include.dirs = F, full.names = T )
gpkg.files<- list.files(path=outdir, pattern = "\\.gpkg$", full.names = T, recursive = T )


bounds<- st_buffer( st_transform(st_as_sfc(st_bbox(italy.nord.bounds)), 32632), 400000  )

tt<-tm_basemap() +
  
  tm_shape(world,is.master = T, bbox = bounds) + 
  tm_polygons( interactive=F, alpha = 0.21)  +
  tm_shape( sf::st_as_sfc( italy.nord.bounds ) ) + 
  tm_polygons( interactive=F, col = "red", alpha = 0.2, border.col = "red" )  +
  
  tm_graticules(n.x=2, n.y=3, labels.rot = c(0, 90),labels.size =1.2  )  
tt



out.tif<-file.path(outdir, "grids", "northItalyCollection.tif")
file.remove(out.tif)
a<-0
outtable<-data.frame(totn_lowqual=rep(NA, length(gpkg.files)), 
                     avgquality_lowqual=rep(NA, length(gpkg.files)),
                     totn_highqual=rep(NA, length(gpkg.files)), 
                     avgquality_higqual=rep(NA, length(gpkg.files)) )

for(i in gpkg.files) {
  print(i)
  a<-a+1
  ptm <- proc.time()
  of<-gdalUtils::ogrinfo(  gpkg.files[[a]],  ro = T,
                      sql = sprintf("SELECT count(*) as cc, avg(quality_flag) as avg from \"%s\"  group by  quality_flag > 0.5", 
                                    tools::file_path_sans_ext( basename(gpkg.files[[a]]) ) ) )
  
  inf<-stringr::str_extract_all( of[c(12:13, 16,17)], pattern = "\\d+")
  if(is.na(inf[[1]]) || inf[[1]]==0 ){
    inf[[2]]<-NA
  }
  
  if(is.na(inf[[3]]) || inf[[3]]==0){
    inf[[4]]<-NA
  }
  outtable[a, ]<-inf
  
  if(a==1){
    gdalUtils::ogr2ogr( src_datasource_name = i, 
                        sql = sprintf("SELECT * from \"%s\"  WHERE quality_flag > 0.5", 
                                      tools::file_path_sans_ext( basename(i) ) ), 
                        overwrite = T, 
                        dst_datasource_name = "GEDI000_clip_6_6278_15_1011_43_7549_47_0821_COLLECTION2.gpkg" )
    next
    
  }
  
  gdalUtils::ogr2ogr( src_datasource_name = i, 
                      sql = sprintf("SELECT * from \"%s\"  WHERE quality_flag > 0.5", 
                                    tools::file_path_sans_ext( basename(i) ) ), gt =  65536,
                      dst_datasource_name = "GEDI000_clip_6_6278_15_1011_43_7549_47_0821_COLLECTION2.gpkg", append = T )
  
  message(sum( as.numeric( unlist(inf)), na.rm = T ) , " punti in ",  round((proc.time() - ptm)[[3]], 1), " sec" ) 
  
}

outtable$perlowquality<- as.integer(outtable$totn_lowqual)/(as.integer(outtable$totn_lowqual)+as.integer(outtable$totn_highqual))*100
hist(outtable$perlowquality, breaks=34, xlab="qualità < 0.5 (%)", ylab="Frequenza", freq=F, main="" )
alln<-sum(as.numeric(outtable$totn_highqual), as.numeric(outtable$totn_lowqual), na.rm = T)
plot(as.numeric(outtable$totn_highqual), as.numeric(outtable$totn_lowqual), 
     xlab="qualità < 0.5 (%)", ylab="Frequenza", main="" )

pts.single<-sf::read_sf( "GEDI000_clip_6_6278_15_1011_43_7549_47_0821_COLLECTION2.gpkg",
               query = "SELECt * from \"SELECT\" limit 2",
             layer="SELECT"  )  

dts<-sf::read_sf( "GEDI000_clip_6_6278_15_1011_43_7549_47_0821_COLLECTION2.gpkg",
                  query = "SELECt count(1), min() from \"SELECT\" group by ",
                  layer="SELECT"  ) 
## Create kernel density output
kde <- bkde2D( st_coordinates(pts),
              bandwidth=c(.0023, .0034), gridsize = c(5000,2500))
# Create Raster from Kernel Density output
KernelDensityRaster <- raster(list(x=kde$x1 ,y=kde$x2 ,z = kde$fhat))

#create pal function for coloring the raster
KernelDensityRaster[ KernelDensityRaster[]==0 ]<-NA
palRaster <- leaflet::colorNumeric("Spectral", domain = KernelDensityRaster@data@values)
raster::writeRaster(KernelDensityRaster, "GEDI000_clip_6_6278_15_1011_43_7549_47_0821_COLLECTION2.tif",  overwrite=T )

# gdalUtils::gdal_rasterize(src_datasource = "GEDI000_clip_6_6278_15_1011_43_7549_47_0821_COLLECTION.gpkg", 
#                           dst_filename = "GEDI000_clip_6_6278_15_1011_43_7549_47_0821_COLLECTION.tif",  l = "SELECT",
#                             burn = 1, add = T, a_nodata = 0, tr = c(0.01,0.01))

gdalUtils::gdaladdo("GEDI000_clip_6_6278_15_1011_43_7549_47_0821_COLLECTION3.tif", levels=c(2,4,8,16,32), ro=T, r="cubic")
## rasterize
rr<-raster("GEDI000_clip_6_6278_15_1011_43_7549_47_0821_COLLECTION.tif")
vv<-hist(rr,  maxpixels=1000000, plot=F )
length(vv$density)
(vv$counts)
saveRDS(dl.dictionary, "dl.dictionary.rds")


levels.zoom<-c( 360, 180, 90, 45, 22.5, 11.25, 5.625, 2.813, 1.406, 0.703, 0.352, 0.176, 
                0.088, 0.044, 0.022, 0.011, 0.005, 0.003, 0.001, 0.0005, 0.00025)

levels.zoom.meters<-c( 156412, 78206, 39103, 19551, 9776, 4888, 2444, 1222, 610.984,
                       305.492, 152.746, 76.373, 38.187, 19.093, 9.547, 4.773, 2.387, 
                       1.193, 0.596, 0.298, 0.149) 

for(i in 10:(length(levels.zoom)-3) ){
  print(i)
  of <- sf::read_sf( "GEDI000_clip_6_6278_15_1011_43_7549_47_0821_COLLECTION.gpkg", 
                    query= sprintf("SELECT CastToInteger(ST_x(geom)/%f)*%f as x, 
                  CastToInteger(ST_y(geom)/%f )*%f as y, count(1) as number  from \"SELECT\" 
                  group by CastToInteger(ST_x(geom)/%f), CastToInteger(ST_y(geom)/%f )  ", 
                                   levels.zoom[[i]], levels.zoom[[i]], 
                                   levels.zoom[[i]], levels.zoom[[i]], 
                                   levels.zoom[[i]], levels.zoom[[i]]
                    )  )
  
of.sf<- st_as_sf(of, coords = c("x", "y"), crs=4326)
sf::st_write(of.sf, "GEDI000_clip_6_6278_15_1011_43_7549_47_0821_COLLECTION.gpkg", 
             layer=sprintf("level%s", i), overwrite=F, append=F  )
   

}




unique( trunc(of2[,1]/0.176)*0.176 ) 
