library(shiny)
library(plotly)
library(leaflet)
library(raster)
library(leaflet.extras)
library(shinyalert)
library(hdf5r)
library(sf)
library("rnaturalearth")
library("rnaturalearthdata")


world <- ne_countries(scale = "medium", returnclass = "sf")
italy <- world[world$name_sort=="Italy",]$geometry
italy.bounds <- sf::st_bbox(sf::st_boundary( italy  ))

load("italy.nord.bounds.rda")
ul_lat<- italy.nord.bounds[["ymax"]]
ul_lon<- italy.nord.bounds[['xmin']]
lr_lat<- italy.nord.bounds[['ymin']]
lr_lon<- italy.nord.bounds[['xmax']]

source("rGEDI.functions.R")


GEDI.products<-list("1B"="1B - Geolocated Waveforms", "2A"="2A - RH metrics", "2B"="2B - Canopy Cover Franction")
GEDI.products.rbuttons<-names(GEDI.products)
names(GEDI.products.rbuttons)<-GEDI.products
maxlatlon<-0.05

daterange<-c("2018-01-01", Sys.Date())
gLevel1B<-gedifinder(product="GEDI01_B",version="001",daterange=daterange)
gLevel2A<-gedifinder(product="GEDI02_A",ul_lat, ul_lon, lr_lat, lr_lon,version="001",daterange=daterange)
gLevel2B<-gedifinder(product="GEDI02_B",ul_lat, ul_lon, lr_lat, lr_lon,version="001",daterange=daterange)

if(file.exists("dl.dictionary.rds")) dl.dictionary<-readRDS("dl.dictionary.rds") else dl.dictionary<-list()

dl.files<- list.files(path="dl_data", pattern = "*", full.names = T, include.dirs = F )
 
 

 for(i in dl.files){
   if( dir.exists(i) ) {
      warning("Skipping directory")
      next
   }
   if( raster::extension(i)!=".h5" ) {
    warning("Skipping as it is not HDF5 file")
    next
   }
   mf<-rGEDI.read(i)
   rrGEDI.clip(i)

   if( is.null(dl.dictionary[[bn]]) ) next
   
 }

saveRDS(dl.dictionary, "dl.dictionary.rds")


