server <- function(input, output, session) {
  mydata<-NULL
  myBounds<-NULL
  output$plot <- renderPlotly({
    req(mydata)
    dt<-data.frame(  x= 1:(  (input$bar_max[[2]] - input$bar_max[[1]])*100), 
               ph=mydata$heights$h_ph[  (1+input$bar_max[[1]]*100) : (input$bar_max[[2]]*100)   ] )
    p <- ggplot(data = dt, aes(x = x, y = ph)) +
      geom_point(alpha = input$opacity/100)
    p <- ggplotly(p) %>% toWebGL()
  })
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  
  output$mymap <- renderLeaflet({
    leaflet()  %>% 
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      addMiniMap()%>%
      # Layers control
      addLayersControl(
        baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
        overlayGroups = c("Outline"),
        options = layersControlOptions(collapsed =T)
      )
  })
  
  
  
  
  
  ###### CHANGED BOUNDS--------
  observeEvent(input$mymap_bounds, {
    print(input$mymap_bounds)

  })
  
  observeEvent(input$process, {
    req(input$mymap_bounds)
    aa<-input$mymap_bounds
    bb<-unlist(input$mymap_bounds)
      
    ext<-raster::extent( bb[ c(4,2,3,1) ] )
    
    
    ul_lat<-ymin(ext)
    ul_lon<-xmin(ext)    
    lr_lat<-ymax(ext)
    lr_lon<-xmax(ext)
    
    ymin<-ul_lat<-46.283
    xmin<-ul_lon<-11.3507   
    ymax<-lr_lat<-46.2375
    xmax<-lr_lon<-11.4141
    
    if( abs(ul_lat-lr_lat)>maxlatlon ||  abs(ul_lon-lr_lon)>maxlatlon ){
      shinyalert(
        html = TRUE,
        text = tagList( 
          sprintf("Size of window (%.3f° %.3f°) degrees is too large, should be %.3f°
                  please zoom closer", abs(ul_lat-lr_lat),
                  abs(ul_lon-lr_lon), maxlatlon )
        )
      )
      return(NULL)
    }
    
    outdir<-"dl_data"
    gedilevel2b<-readLevel2B(level2Bpath = paste0(outdir,"/GEDI02_B_2019149155511_O02606_T01466_02_001_01.h5"))
     
    # level1b_clip_bb <- clipLevel1B(gedilevel1b, xmin, xmax, ymin, ymax,output=paste0(outdir,"//level1b_clip_bb.h5"))
    # level2a_clip_bb <- clipLevel2A(gedilevel2a, xmin, xmax, ymin, ymax, output=paste0(outdir,"//level2a_clip_bb.h5"))
    level2b_clip_bb <- clipLevel2B(gedilevel2b, xmin, xmax, ymin, ymax,output=paste0(outdir,"//level2b_clip_bb.h5"))
    # 
    # gedilevel2b<- rGEDI::readLevel2B(i)
    # level2BVPM<-rGEDI::getLevel2BVPM(gedilevel2b)
    # head(level2BVPM[,c("beam","shot_number","pai","fhd_normal","omega","pgap_theta","cover")])
    
     
  })
  
  
  observeEvent(input$download , {
    req(input$product)
    req(input$mymap_bounds)
    aa<-input$mymap_bounds
    bb<-unlist(input$mymap_bounds)
    
    ext<-raster::extent( bb[ c(4,2,3,1) ] )
    
    
    ul_lat<-ymin(ext)
    ul_lon<-xmin(ext)    
    lr_lat<-ymax(ext)
    lr_lon<-xmax(ext)
    
    if( abs(ul_lat-lr_lat)>maxlatlon ||  abs(ul_lon-lr_lon)>maxlatlon ){
      shinyalert(
        html = TRUE,
        text = tagList( 
          sprintf("Size of window (%.3f° %.3f°) degrees is too large, should be %.3f°
                  please zoom closer", abs(ul_lat-lr_lat),
                  abs(ul_lon-lr_lon), maxlatlon )
        )
      )
      return(NULL)
    }
    
    gLevel<-NULL
    if(input$product=="1B") gLevel<-gedifinder(product="GEDI01_B",ul_lat, ul_lon, lr_lat, lr_lon,version="001",
                                               daterange=input$daterange)
    if(input$product=="2A") gLevel<-gedifinder(product="GEDI02_A",ul_lat, ul_lon, lr_lat, lr_lon,version="001",
                                               daterange=input$daterange)
    if(input$product=="2B") gLevel<-gedifinder(product="GEDI02_B",ul_lat, ul_lon, lr_lat, lr_lon,version="001",
                                               daterange=input$daterange)
 
    
    
  })
  
}
