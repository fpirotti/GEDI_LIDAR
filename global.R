library(shiny)
library(shinydashboard)
library(sf)
library(jsonify)
library(leaflet)
library(leaflet.opacity)
library(leaflet.extras)
library("rnaturalearth")
library("rnaturalearthdata")
library(ggplot2)
library(shinyalert)
library(tmap)
 
 
GEDI.products<-list("1B"="1B - Geolocated Waveforms", "2A"="2A - RH metrics", "2B"="2B - Canopy Cover Franction")
GEDI.products.rbuttons<-names(GEDI.products)
names(GEDI.products.rbuttons)<-GEDI.products


load("italy.nord.bounds.rda")  
tmap_mode("view")
#tm_view(leaflet.options = leafletOptions(preferCanvas = T) )
hm<-raster("GEDI000_clip_6_6278_15_1011_43_7549_47_0821_COLLECTION.tif")
#hm[hm[]==0]<-NA

ttt<-  tm_shape(hm) +
  tm_raster( title="GEDI Point Density", interpolate=T,  n=50, palette = "-RdYlGn", legend.hist=T, legend.show = F, alpha = 0.6) +
  tm_minimap()

#of2<- st_coordinates(of)
#names(of2)<-c("X","y")
#of2<-as.data.frame(of2)
#save(of2, file="points.rda")
#load("points.rda")
