server <- function(input, output, session) {
  mydata<-NULL
  myBounds<-NULL 
  limits <- 1000
 
  
  output$myMap <- renderLeaflet({ ttt  })
  
  
  
  ###### CHANGED BOUNDS--------
  observeEvent(input$myMap_shape_click, {
    
    req(input$myMap_shape_click)
    print(input$myMap_shape_click)
    
    df<-sf::read_sf( "output/GEDI01_B_clip_6_6278_15_1011_43_7549_47_0821_COLLECTION.gpkg",
                 query = sprintf("select   * from \"SELECT\" WHERE fid=%d;", 
                                 as.integer(substr(input$myMap_shape_click$id, 2, 9999999) ) ), 
                 layer="SELECT", as_tibble = F ) 
    df$geom<-NULL
    cnames<-grep(names(df),pattern = "rh\\d", value=F)
    cnames
    dfd<-data.frame(val=unlist(df[cnames]) )
    dfd.val<-  diff(dfd$val)  
    sss<-ggplot() + geom_point( aes(y= dfd$val , x= round( (1:(nrow(dfd) ))/100, 2 ) ) ) +
      xlab("Normalized Cumulative Return Energy")+
      ylab("Relative Height") +
      geom_vline(xintercept = c(0.25, 0.5, 0.75) ) +
        theme_bw()
    
    shinyalert::shinyalert(         html = TRUE,
                                    text = tagList(
                                      renderPlot ({ sss })
                                    ) )
  })
  
  observeEvent(  input$myMap_zoom, {
    leafletProxy("myMap", session) %>%
      clearShapes()  
  }, priority = 100)
  
  ###### CHANGED BOUNDS--------
  observeEvent({ input$myMap_zoom
    input$myMap_bounds } , { 
 
    lev <-   input$myMap_zoom
    bbox<-st_bbox( st_as_sf( as.data.frame( t(matrix(input$myMap_bounds, ncol=2)) ), coords = c("V2", "V1"), crs=4326) )
    wkt = st_as_text(st_as_sfc( bbox ) ) 
    #wkt<-"POLYGON ((10.82349 45.58309, 10.83318 45.58309, 10.83318 45.5861, 10.82349 45.5861, 10.82349 45.58309))"
    if(lev > 13) {  
 
      currlayer2<-tryCatch({
         sf::read_sf( "output/GEDI01_B_clip_6_6278_15_1011_43_7549_47_0821_COLLECTION.gpkg",
                     wkt_filter = wkt, fid_column_name ="fid",  # as_tibble = FALSE,
                     layer="SELECT" ) 
        },
        error=function(X){
          X
        })
      
      currlayer3<<-currlayer2
      if(nrow(currlayer2)==0){
        shinyjs::html("logwindow","Nessun punto")
        return(NULL)
      }
      
      currlayer2$Time <- format( as.POSIXct(currlayer2$delta_time, origin="2018-01-01T00:00:00Z"),"%Y-%m-%d %H:%M:%OS4" )
      currlayer2<-currlayer2[,c("Time", "fid", "geom", "rh90")]
      
      shinyjs::html("logwindow",paste0(nrow(currlayer2), " point visualized."))
      
      leafletProxy("myMap", data = currlayer2) %>%
        clearShapes() %>%
        addCircles(radius = lev, group= "GEDI Footprints", weight = 1, color = "#770000",
                    fillOpacity = 0.3, layerId = currlayer2$fid   )
      
      shinyjs::runjs("$('.leaflet-control-layers-overlays > label:nth-child(2)').css('color', 'black');");
      
    } else {
      
      shinyjs::runjs("$('.leaflet-control-layers-overlays > label:nth-child(2)').attr('title', 'Zoom to level 14 or more the extract GEDI points and see them on the screen');");
      shinyjs::runjs("$('.leaflet-control-layers-overlays > label:nth-child(2)').css('color', 'grey');");
      shinyjs::html("logwindow",paste0("Zoom to level 14 or more to load points: you are now at zoom level ", lev, "."))
    }
    


    
  }, priority = 10)
    
  
  
}
