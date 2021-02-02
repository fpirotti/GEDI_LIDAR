server <- function(input, output, session) {
  mydata<-NULL
  myBounds<-NULL 
  limits <- 1000
 
  
  output$myMap <- renderTmap({ ttt  })
  
  
  
  ###### CHANGED BOUNDS--------
  observeEvent(input$myMap_shape_click, {
    
    req(input$myMap_shape_click)
    print(input$myMap_shape_click)
    
    df<-sf::read_sf( "GEDI000_clip_6_6278_15_1011_43_7549_47_0821_COLLECTION2.gpkg",
                 query = sprintf("select   * from \"SELECT\" WHERE fid=%d;", 
                                 as.integer(substr(input$myMap_shape_click, 2, 9999999) ) ), 
                 layer="SELECT", as_tibble = F ) 
    df$geom<-NULL
    cnames<-grep(names(df),pattern = "rh\\d", value=F)
    cnames
    dfd<-data.frame(val=unlist(df[cnames]) )
    dfd.val<-  diff(dfd$val)  
    sss<-ggplot() + geom_point( aes(y= dfd$val , x=1:(nrow(dfd) ) ) ) +
      coord_flip() + theme_bw()
    shinyalert::shinyalert(         html = TRUE,
                                    text = tagList(
                                      renderPlot ({ sss })
                                    ) )
  })
  
  observeEvent(  input$myMap_zoom, {
    tmapProxy("myMap", session, {  tm_remove_layer(402)   })
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
         sf::read_sf( "GEDI000_clip_6_6278_15_1011_43_7549_47_0821_COLLECTION2.gpkg",
                     wkt_filter = wkt, fid_column_name ="fid",  # as_tibble = FALSE,
                     layer="SELECT" ) 
        },
        error=function(X){
          X
        })
      
      currlayer3<<-currlayer2
      if(nrow(currlayer2)==0){
        tmapProxy("myMap", session, { 
          tm_remove_layer(402) 
          })  
        print("Nessun punto")
        return(NULL)
      }
      
      currlayer2$Time <- format( as.POSIXct(currlayer2$delta_time, origin="2018-01-01T00:00:00Z"),"%Y-%m-%d %H:%M:%OS4" )
      currlayer2<-currlayer2[,c("Time", "fid", "geom")]
      
      print( nrow(currlayer2) )
      tmapProxy("myMap", session, { 
        tm_remove_layer(402) +
          tm_shape(currlayer2,  name="GEDI" ) +
          tm_symbols( col = "Time", alpha = 0.5, id = "fid",  
                      interactive = T, zindex = 402, size= 1,
                      legend.col.show = F ) + 
          tmap_options( max.categories = 6 )
      })
      
    } 
    


    
  }, priority = 10)
    
  
  
}
