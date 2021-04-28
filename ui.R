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
  
  div( div(style="display:none;", icon("warning") ), img(src="cirgeoheader.PNG" , style="height: 80px;" ), 

    fluidRow(
      column(3, downloadButton("downloadData", "Download Granules") ),
      column(3, actionButton("search", "Search shot number") ),
      column(6, textInput("shotNumber", NULL, placeholder = "27741105700219828") ),
    #  column(6, h5("Zoom to city level (zoom=13) or closer to view/download/query GEDI footprints, now you are at zoom=", span( id="sss") ) )
    )
   # actionButton("process", "Process Granules")
  ), 
  
 # fluidRow(  
   addSpinner( leafletOutput(  'myMap'   ),spin = "cube-grid", color = "#330000"), 
   div(id="logwindow", style="border:1px solid black; height:30px; width:100%;", "LOG")
  #)

)
