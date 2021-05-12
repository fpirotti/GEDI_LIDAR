
library(htmlwidgets)



server <- function(input, output, session) {
  mydata<-NULL
  myBounds<-NULL 
  limits <- 1000
 

  
  
  output$myMap <- renderLeaflet({ ttt %>% 
      onRender(
        "function(el,x){
                    this.on('mousemove', function(e) {
                        var lat = e.latlng.lat;
                        var lng = e.latlng.lng;
                        var coord = [lat, lng];
                        
                        $('#logwindow').html('Lat='+  lat.toFixed(5)+
                           ' - Long='+  lng.toFixed(5)  );
                    }); 
                }"
      )
    })
  
  
  ###### DOWNLOAD  --------
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".gpkg", sep="")
    },
    content = function(file) {
      if(input$myMap_zoom < 13 ){
        
        gdalUtils::ogr2ogr(src_datasource_name = pointSource,
                           dst_datasource_name = file, spat = c(0,0,0,0)  )
      } else {
        
        gdalUtils::ogr2ogr(src_datasource_name = pointSource,
                           dst_datasource_name = file, spat = c(input$myMap_bounds$east,
                                                                input$myMap_bounds$south,
                                                                input$myMap_bounds$west,
                                                                input$myMap_bounds$north)  )        
      }
      

       
    }
  )
  
  
  ###### CLICK  --------
  observeEvent(input$myMap_shape_click, {
    
    req(input$myMap_shape_click)
    print(input$myMap_shape_click)
    
    df<-sf::read_sf( pointSource,
                 query = sprintf("select   * from \"SELECT\" WHERE fid=%d;", 
                                 as.integer(input$myMap_shape_click$id) ) , 
                 layer="SELECT", as_tibble = F ) 
    df$geom<-NULL
    cnames<-grep(names(df),pattern = "^rh\\d", value=F)
    
    df$Time <- format( as.POSIXct(df$delta_time, origin="2018-01-01T00:00:00Z"),"%Y-%m-%d %H:%M:%OS4" )
    # cnames
    # zElevation <- df$elev_lowestmode
    # zTop  <- df$elev_highestreturn
    # 
    
    dfd<-data.frame(val=unlist(df[cnames]) )
    #dfd.val<-  diff(dfd$val)  
    
    aaaaa<-dfd$val
    q <- 0:100
    Energy.Percent <- q 
    Height <- aaaaa
    # model <- lm(aaaaa ~ poly(q,5))
    # predicted.intervals <- predict(model,data.frame(x=q),interval='confidence',
    #                                level=0.99)
    diffs <- diff( aaaaa )
    diff2 <- 100 +  c( diffs[[1]], diffs)* -100
    diffs <- -1/c( diffs[[1]], diffs)
    sss<- ggplot() + 
      geom_point( aes(y= Height , x=Energy.Percent), color=colourvalues::color_values(diffs, palette = "spectral")  ) + 
      geom_point( aes(y= Height , x=diff2 )  ) + 
    #  geom_line(colour = "red",  aes(y= predicted.intervals[,'fit'] , x=q ) ) +
      xlab("Percent Energy Returned")+
      ylab("Relative Height (m)") +
      #ggtitle(sprintf("%s - Shot number: %s", df$beam, df$shot_number)) + 
      geom_hline(yintercept = c(aaaaa[[25]], aaaaa[[50]], aaaaa[[75]]) ) +
      geom_vline(xintercept = c( 25 ,   50 ,  75 ), alpha=0.2 ) +
      scale_y_continuous(sec.axis = sec_axis(~ . + df$elev_lowestmode,
                                              name = "Elevation (m a.m.s.l.)")) +
      theme_bw()
    
     
    
    shinyalert::shinyalert(         html = TRUE,
                                    size = "l",
                                    text = tagList( 
                                      fluidRow( 
                                        column(6, style="text-align:left;", HTML(sprintf("Data: %s<br>Beam: %s
                                                               <br>Shot: %s<br><hr>Alt. low/high: %.2f m /%.2f m
                                                               <br>Delta Alt.: %.2f m",
                                                         df$Time, df$beam, df$shot_number, 
                                                         (df$elev_lowestmode),
                                                         (df$elev_highestreturn),
                                                         (df$elev_highestreturn) - (df$elev_lowestmode) ) ) ),
                                        column(6, renderPlotly ({   sss }) )
                                      )
                                    ) )
  })
  
  
  ## SEARCH -------
  observeEvent(  input$search, {
    req(  input$search)
    
    if( is.na(as.numeric(input$search))){
      shinyjs::alert("Shot number not understood!")
      return(NULL)
    }
    
    shotnumber <-  ( trimws(input$search) )
    query <- sprintf("select * from \"SELECT\" WHERE shot_number='%s';", 
                     shotnumber )
    
    df<- sf::read_sf( pointSource,
                      query = query ,
                   fid_column_name ="fid",  # as_tibble = FALSE,
                 layer="SELECT" ) 
    
    if(nrow(df)==0){ 
      shinyjs::alert( sprintf("Shot number '%s' not found!", input$search) )
      return(NULL)
    }
    coord <- (st_coordinates(df))
    
    leafletProxy("myMap", session) %>% 
      addCircles(data=df, group="temporary", color = "yellow", radius=10, 
                 options = pathOptions(interactive = F, clickable = F) ) %>% 
      fitBounds(lng1 =coord[1,1], lat1 = coord[1,2],
                lng2 = coord[1,1],lat2 = coord[1,2] )
    
    
    
  } )
  
  
  observeEvent(  input$myMap_zoom, {
    leafletProxy("myMap", session) %>%
      clearGroup("GEDI Footprints") 
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
         sf::read_sf( pointSource,
                     wkt_filter = wkt, fid_column_name ="fid",  # as_tibble = FALSE,
                     layer="SELECT" ) 
        },
        error=function(X){
          X
        })
      
      if(is.element("error", class(currlayer2) )) {
        shinyjs::html("logwindow", paste0("ERRORE! ", currlayer2) )
        return(NULL)
      }
      
      currlayer3<<-currlayer2
      if(nrow(currlayer2)==0){
        shinyjs::html("logwindow","Nessun punto")
        return(NULL)
      }
      
      currlayer2$Time <- format( as.POSIXct(currlayer2$delta_time, origin="2018-01-01T00:00:00Z"),"%Y-%m-%d %H:%M:%OS4" )
      currlayer2<-currlayer2[,c("Time", "fid", "geom", "rh90")]
      
      shinyjs::html("logwindow",paste0(nrow(currlayer2), " point visualized."))
      
      leafletProxy("myMap", data = currlayer2) %>%
        clearGroup("GEDI Footprints") %>%
        addCircles(radius = lev, group= "GEDI Footprints", weight = 1, color = "#770000",
                    fillOpacity = 0.3, layerId = currlayer2$fid   )
      
      shinyjs::runjs("$('.leaflet-control-layers-overlays > label:nth-child(2)').css('color', 'black');");
      shinyjs::runjs("if($('.leaflet-control-layers-overlays > label:nth-child(2)').children().children().length > 2 ){
         $('.leaflet-control-layers-overlays > label:nth-child(2)').children().children()[2].remove();
       }");
      
      shinyjs::runjs("$('.leaflet-control-layers-overlays > label:nth-child(2)').attr('title', 'Zoom is OK!');");
      
      
      shinyjs::enable("downloadData")
    } else {
      shinyjs::disable("downloadData")
      shinyjs::runjs("$('.leaflet-control-layers-overlays > label:nth-child(2)').attr('title', 'Zoom to level 14 or more the extract GEDI points and see them on the screen');");
      shinyjs::runjs("$('.leaflet-control-layers-overlays > label:nth-child(2)').css('color', 'red');");
      shinyjs::runjs("if($('.leaflet-control-layers-overlays > label:nth-child(2)').children().children().length < 3 ) $('.leaflet-control-layers-overlays > label:nth-child(2)').children().append('<i class=\"fa fa-warning\" role=\"presentation\" aria-label=\"warning icon\"></i>')");
      shinyjs::html("sss", lev)
    }
    


    
  }, priority = 10)
    
  
  
}
