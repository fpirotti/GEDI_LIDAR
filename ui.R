ui <- fluidPage(
  
  useShinyalert(),  # Set up shinyalert
  
  inputPanel(
    sliderInput("bar_max", label = "Max:",
                min = 0, max = 1000, value = c(1, 10), step = 0.1),
    
    sliderInput("opacity", label = "Contrast",  min = 0, max = 100, value = 1, step = 1),
    radioButtons("product", NULL, GEDI.products.rbuttons),
    dateRangeInput("daterange",NULL, start="2019-07-01", end="2021-05-22" ),
    actionButton("download", "Download Granules"),
    actionButton("process", "Process Granules")
  ), 
  
  fluidRow(
    column(6 ,  leafletOutput("mymap") ),
    column(6 ,   plotlyOutput("plot") )
  )

)
