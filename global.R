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
#library(devtools)
#devtools::install_git("https://github.com/carlos-alberto-silva/rGEDI", dependencies = TRUE)
source("rGEDI.functions.R")
#source("DAACDataDownload.R")

GEDI.products<-list("1B"="1B - Geolocated Waveforms", "2A"="2A - RH metrics", "2B"="2B - Canopy Cover Franction")
GEDI.products.rbuttons<-names(GEDI.products)
names(GEDI.products.rbuttons)<-GEDI.products
maxlatlon<-0.05

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


