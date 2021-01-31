# library("BiocManager")
# BiocManager::install(c("rhdf5"))
library(httr)
library(hdf5r)
library(raster)
library(jsonlite)
library(XML)
library(rlas)
library(httr)
library(doParallel)  
library(foreach)
library(bigstatsr)
library(bench)
library(lwgeom)
library(mapview)
library(bigmemory) 
library(stars)




if(!exists("italy.nord.bounds")) load("italy.nord.bounds.rda")
data.to.write<-c(
  "signal_photons/delta_time",
  "land_segments/longitude",
  "land_segments/latitude",
  "land_segments/canopy/h_canopy",
  "land_segments/canopy/canopy_rh_conf",
  "land_segments/canopy/h_median_canopy_abs",
  "land_segments/canopy/h_min_canopy",
  "land_segments/canopy/h_mean_canopy_abs",
  "land_segments/canopy/h_median_canopy",
  "land_segments/canopy/h_canopy_abs",
  "land_segments/canopy/toc_roughness",
  "land_segments/canopy/h_min_canopy_abs",
  "land_segments/canopy/h_dif_canopy",
  "land_segments/canopy/h_canopy_quad",
  "land_segments/canopy/n_ca_photons",
  "land_segments/canopy/centroid_height",
 # "land_segments/canopy/canopy_h_metrics_abs",
  "land_segments/canopy/landsat_perc",
  "land_segments/canopy/h_mean_canopy",
 # "land_segments/canopy/subset_can_flag",
  "land_segments/canopy/canopy_h_metrics",
  "land_segments/canopy/n_toc_photons",
  "land_segments/canopy/canopy_flag",
  "land_segments/canopy/landsat_flag",
  "land_segments/canopy/h_max_canopy_abs",
  "land_segments/canopy/h_canopy_uncertainty",
  "land_segments/canopy/canopy_openness",
  "land_segments/canopy/h_max_canopy",
  "land_segments/terrain/h_te_uncertainty",
  "land_segments/terrain/h_te_mean",
  "land_segments/terrain/h_te_min",
  "land_segments/terrain/h_te_interp",
  "land_segments/terrain/h_te_max",
  "land_segments/terrain/h_te_skew",
  "land_segments/terrain/h_te_median",
  "land_segments/terrain/h_te_best_fit",
  "land_segments/terrain/h_te_std",
  "land_segments/terrain/terrain_slope",
  "land_segments/terrain/n_te_photons",
  "land_segments/terrain/h_te_mode" 
)

download.data.path = "dl_data/ICESAT"
dirs <- list(
  download.data.path = download.data.path,
  download.data.path.proc = file.path(download.data.path, "processed"),
  download.data.path.geodata = file.path("/archivio/shared/geodati/vettoriali/ICESAT/ATL08")
)


token.RetSTring <- NULL
getToken <- function() {
  token <-
    "curl --silent -X POST --header \"Content-Type: application/xml\" -d \"<token><username>tesaf</username><password>Libero17</password><client_id>NSIDC_client_id</client_id><user_ip_address>95.140.134.234</user_ip_address> </token>\" https://cmr.earthdata.nasa.gov/legacy-services/rest/tokens"
  res <- system(token,  intern = T)
  resj <- xmlToList(res)
  token.RetSTring <<- resj
  resj$id
}

create.url <- function(bb, sname = "ATL08", outfile) {
  parameters <- list(
    short_name = sname,
    version = "001",
    "temporal[]" = "2015-09-03T00:00:00,2029-11-28T23:59:59",
    bounding_box = bb, 
    version = "3",
    version = "03",
    version = "003",
    provider = "NSIDC_ECS",
    page_size = "2000",
    #  email="no",
    token = getToken()
  )
  
  sprintf(
    "curl -o \"%s\" -O -J --dump-header response-header.txt  \"https://cmr.earthdata.nasa.gov/search/granules.json?%s\"",
    outfile,
    paste0(sprintf("%s=%s", names(parameters), parameters), collapse = "&")
  )
}

find.granules<-function(type = "ATL08", bounds){
  oo <- tryCatch({ 
    outfile <- tempfile()
    cmd <-
      create.url(paste0(collapse = ",", bounds), type, outfile)
    system(cmd, intern = T)
    read_json(outfile)
  },
  error = function(err) {
    err
  },
  warning = function(warn) {
    warn
  })
}  


for (i in dirs) {
  if (!dir.exists(i)) {
    dir.create(i, recursive = T, mode = "0777")
    Sys.chmod(i, mode = "0777", use_umask = TRUE)
  }
}


#granule.paths <- list.files(download.data.path, pattern = "*.h5", full.names = T)

 
oo<-find.granules(type = "ATL08", italy.nord.bounds)
granule.paths <- c()
existing.paths<-c()
for (i in oo$feed$entry) {
  url.address <- i$links[[1]]$href
  outfile<-file.path(download.data.path, basename(url.address))
  if (is.element(basename(url.address), granule.names)) {
    existing.paths<-c(existing.paths, outfile)
    next
  }
  print(basename(url.address))
  r <- GET(url.address, authenticate("tesaf", "Libero17"))
  bin <- content(r, "raw")
  writeBin(bin, outfile)
  granule.paths<-c(granule.paths, outfile)
}

message( length(existing.paths), " files already processed...")
message( length(granule.paths), " files to be processed...")
#granule.paths<-existing.paths

 #list.files(download.data.path, pattern = "*.h5", full.names = T)
granule.names <-basename(granule.paths)


earth <- c("land", "ocean", "sea ice", "land ice", "inland water")
out.crs <- sf::st_crs(4326)
beams<-c("gt1r","gt2r","gt1l","gt3l","gt2l","gt3r")
hit <- F
lines <- list()
tables<-list()


#granule<-"dl_data/ICESAT/ATL08_20181014141925_02440102_003_01.h5"
getGranule<-function(granule) {
  
  op<-hdf5r::H5File$new(granule) 
  pp <- op$ls(recursive=TRUE)
  if(nrow(pp)==0){
    warning("HDF file seems empty... is the file corrupt?")
    return(NULL)
  }
  beams.data<-list()
  for(i in beams){
    beams.data[[i]]<-list()
    for(i1 in data.to.write){ 
      beams.data[[i]][[basename(i1)]] <- op$open(sprintf("%s/%s", i, i1) )$read() 
      beams.data[[i]][[basename(i1)]][ beams.data[[i]][[basename(i1)]] > 3.3e+38 ] <-NA
    }
    timetag<-as.POSIXct(beams.data[[i]]$delta_time, origin = "2018-01-01")
    beams.data[[i]]$delta_time<-NULL
    
    every <-   ( length(timetag) / length(beams.data[[i]]$longitude) )
    ww<-seq(1, length(timetag), every)
    beams.data[[i]]$time<-timetag[ww]
    nn<-beams.data[[i]][["canopy_h_metrics"]]
    nn<-as.list(as.data.frame(t(nn)))
    names(nn)<- sprintf("canopyH_%sp", c(25,50,60,70,75,80,85,90,95 ))
    beams.data[[i]][["canopy_h_metrics"]]<-NULL 
    beams.data[[i]] <- c(beams.data[[i]], nn)
  }
  op$close()
  f.table<-data.table::rbindlist(beams.data, idcol = "beam")
  fname.gr<-basename(granule)
  raster::extension(fname.gr)<-""
  
  geo1<- st_as_sf(f.table,  coords=c("longitude", "latitude"), crs=out.crs )
 

  geo<- st_intersection(geo1,  st_as_sfc(italy.nord.bounds))
  if(nrow(geo) > 0) st_write(geo, file.path(dirs$download.data.path.geodata, paste0(fname.gr,".gpkg")), append = F)
  geo 
}

no_cores <- detectCores() - 2
registerDoParallel(cores=no_cores)

cl <- makeCluster(no_cores, type="FORK")


  tmp <- foreach(i = granule.paths,
                 .packages = c("sf", "data.table", "hdf5r"))  %dopar%  {
                    getGranule(i)
                 }

  stopCluster(cl)

save(tmp, file="totalTable.rda")

tmp2<-tmp[!sapply(tmp,is.null)]
# whereRthePH <- lapply(granule.paths[1:5], getGranule)
geo <- do.call(rbind, tmp2)

rr<-raster::raster( raster::extent(st_bbox(geo)), resolution=0.001 )
mm<-st_as_stars(rr, values = NA_integer_)
r2 = st_rasterize(geo, mm, options = c("MERGE_ALG=ADD", "ALL_TOUCHED=TRUE") )


st_write(geo, file.path(dirs$download.data.path.geodata, "ATL08_merged.gpkg"), append = F)
