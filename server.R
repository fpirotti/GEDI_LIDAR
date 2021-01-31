server <- function(input, output, session) {
  mydata<-NULL
  myBounds<-NULL 
  limits <- 1000
  # output$plot <- renderPlotly({
  #   req(mydata)
  #   dt<-data.frame(  x= 1:(  (input$bar_max[[2]] - input$bar_max[[1]])*100), 
  #              ph=mydata$heights$h_ph[  (1+input$bar_max[[1]]*100) : (input$bar_max[[2]]*100)   ] )
  #   p <- ggplot(data = dt, aes(x = x, y = ph)) +
  #     geom_point(alpha = input$opacity/100)
  #   p <- ggplotly(p) %>% toWebGL()
  # })
  

  
  output$myMap <- renderTmap({ ttt  })
  
  
  
  ###### CHANGED BOUNDS--------
  observeEvent(input$myMap_shape_click, {
    
    req(input$myMap_shape_click)
    print(input$myMap_shape_click)
    
    df<-sf::read_sf( "GEDI000_clip_6_6278_15_1011_43_7549_47_0821_COLLECTION.gpkg",
                 query = sprintf("select   * from \"SELECT\" WHERE fid=%d;", as.integer(substr(input$myMap_shape_click, 2, 9999999) ) ), 
                 layer="SELECT", as_tibble = F ) 
    df$geom<-NULL
    dfd<-data.frame(val=unlist(df[cnames]) )
    sss<-ggplot() + geom_area( aes(y=diff(dfd$val), x=1:(nrow(dfd)-1) ) ) +
      coord_flip() + theme_bw()
    cnames<-grep(names(df),pattern = "rh\\d", value=F)
    shinyalert::shinyalert(sss)
  })
  
  
  ###### CHANGED BOUNDS--------
  observeEvent(input$myMap_zoom, {
    # bb<<-(input$myMap_bounds)
    # zz<<-(input$myMap_zoom)
    lev<-(input$myMap_zoom+5)
    if(lev < 10) { 
      lev<-10 
      print("ret")
      return(NULL)
    }
    
    bbox<-st_bbox( st_as_sf( as.data.frame( t(matrix(input$myMap_bounds, ncol=2)) ), coords = c("V2", "V1"), crs=4326) )
    wkt = st_as_text(st_as_sfc( bbox ) ) 
    #wkt<-"POLYGON ((10.82349 45.58309, 10.83318 45.58309, 10.83318 45.5861, 10.82349 45.5861, 10.82349 45.58309))"
    if(lev > 19) {  
      # qq<-sprintf("select fid as ffid, * from \"SELECT\" WHERE  ST_Within(geom, GeomFromText( \"%s\") ) limit %d;", 
      #             wkt, limits )
      # qq3<<-qq
      currlayer2<-tryCatch({
         sf::read_sf( "GEDI000_clip_6_6278_15_1011_43_7549_47_0821_COLLECTION.gpkg",
                     wkt_filter = wkt, fid_column_name ="fid",  # as_tibble = FALSE,
                     layer="SELECT" ) 
        },
        error=function(X){
          X
        })
      
      currlayer2$delta_time <- format( as.POSIXct(currlayer2$delta_time, origin="2018-01-01T00:00:00Z"),"%Y-%m-%d %H:%M:%OS4" )
      
      tmapProxy("myMap", session, { 
        tm_remove_layer(402) +
          tm_shape(currlayer2,  name="GEDI" ) +
          tm_symbols( col = "red", alpha = 0.5, id = "fid",  interactive = T, zindex = 402, size= (lev-19)/3 )
      })
      
    }
    #else {
       
      # currlayer2<-tryCatch({
      #   sf::read_sf( "GEDI000_clip_6_6278_15_1011_43_7549_47_0821_COLLECTION.gpkg",
      #                wkt_filter = wkt, # query = qq,
      #                layer=sprintf("level%d",lev)  )  },
      #   error=function(X){
      #     X
      #   })
      # 
      # print(nrow(currlayer2))
      # 
      # tmapProxy("myMap", session, { 
      #   tm_remove_layer(402) +
      #     tm_shape(currlayer2,  name="GEDI_aggregati" ) +
      #     tm_symbols( col = "number",alpha = 0.5, interactive = F, zindex = 402, size=0.5 )
      # })
      
   # }
    


    
  })
    
  
  
}
