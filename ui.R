ui <- fluidPage(
  tags$head(
    tags$style(HTML(" 
      .lnlt { 
            font-size: 12px !important;
            font-weight:bold;
            text-align: center;
      } "))
  ),
  useShinyalert(),  # Set up shinyalert
  useShinyjs(),  # Set up shinyalert
  
  div( img(src="cirgeoheader.PNG" ), 

    fluidRow(
      column(3, actionButton("download", "Download Granules") ),
      column(9, h4("Zoom to city level (zoom=14 or more) to see and query the GEDI footprints L2A") )
    )
   # actionButton("process", "Process Granules")
  ), 
  
 # fluidRow(  
   addSpinner( leafletOutput(  'myMap'   ),spin = "cube-grid", color = "#330000"), 
   div(id="logwindow", style="border:1px solid black; height:30px; width:100%;", "LOG")
  #)

)
