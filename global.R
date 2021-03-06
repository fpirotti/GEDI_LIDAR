library(shiny)
library(shinyWidgets)
library(sf)
library(jsonify)
library(leaflet)
library(leaflet.opacity)
library(leaflet.extras)
library("rnaturalearth")
library(raster)
library(ggplot2)
library(shinyjs)
library(shinyalert)
library(leaflet)
library(leaflet.extras)
library(mapview)
library(plotly)
 

GEDI.products<-list("1B"="1B - Geolocated Waveforms", "2A"="2A - RH metrics", "2B"="2B - Canopy Cover Franction")
GEDI.products.rbuttons<-names(GEDI.products)
names(GEDI.products.rbuttons)<-GEDI.products

pointSource <-"/archivio/shared/geodati/vettoriali/laser/GEDI/GEDI01_B_clip_6_6278_15_1011_43_7549_47_0821_COLLECTION.gpkg"

query <- sprintf("select count(*) from \"SELECT\";" )

df<- sf::read_sf( pointSource,
                  query = query ,
                  fid_column_name ="fid",  # as_tibble = FALSE,
                  layer="SELECT" ) 
nPoints <- df$`count(*)`
load("italy.nord.bounds.rda")  
#tmap_mode("view")
#tm_view(leaflet.options = leafletOptions(preferCanvas = T) )
#hm<-raster("output/GEDI01_B_clip_6_6278_15_1011_43_7549_47_0821_COLLECTION.tif")
url = "https://www.cirgeo.unipd.it/cgi-bin/qgis_mapserv.fcgi?map=/archivio/shared/R/GEDI_LIDAR/output/GEDIqgis.qgz"


#hm[hm[]==0]<-NA

ttt<-  leaflet( options=leafletOptions(preferCanvas=T ) ) %>% addTiles(group = "OSM") %>% 
  addProviderTiles(providers$Esri.NatGeoWorldMap, group = "ESRI") %>% 
  leaflet.extras::addBingTiles(apikey = "AjvjPYuoA4IgNeooKvodDLcxbVL1F8RdIxXUeYsb6PgiVapURz_PbbWvOxVKmNps",
               imagerySet = c("Aerial"), layerId = NULL, group =  "BING Satellite") %>% 
  
  addWMSTiles( url, layers = "GEDI01_B_clip_6_6278_15_1011_43_7549_47_0821_COLLECTION",
               options = WMSTileOptions(format = "image/png", transparent = T), group = "GEDI Point Density") %>% 
  setView(lng = 11, lat = 45.0, zoom = 6) %>%
  addMeasure() %>%
  addScaleBar() %>%
  
  # Layers control
  addLayersControl(
    baseGroups = c("OSM","ESRI","BING Satellite" ),
    overlayGroups = c("GEDI Point Density", "GEDI Footprints"),
    options = layersControlOptions(collapsed = FALSE)
  )
 

# 
# xv<-seq(0,4,0.01)
# yv<-dnorm(xv,2,0.5) 
# yv <- aaaaa
# plot(yv, ylim=c(-4,24))
# dY <- 1/diff(yv)   # the derivative of your function
# dX <- rowMeans(embed(1:length(yv),2)) # centers the X values for plotting
# points(dX,dY,type="l",col="red") #check
# 


#of2<- st_coordinates(of)
#names(of2)<-c("X","y")
#of2<-as.data.frame(of2)
#save(of2, file="points.rda")
#load("points.rda")
