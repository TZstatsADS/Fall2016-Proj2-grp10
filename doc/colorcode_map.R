library(shiny)
library(leaflet)
library(data.table)
library(dplyr)
blocks = fread('../output/blocks_Manhattan.csv')
source('../lib/get_points_from_segment.R')


ui <- bootstrapPage(
  #tags$head(includeCSS("../doc/styles.css")),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(class = "panel panel-default", top = 10, right = 10,
                h4("Select preference:"),
                sliderInput("tree", "   Trees:", min=1, max=100, value=50),
                sliderInput("slope", label = "   Flatness:", min=1, max=100, value=50)
  )
)

               

map = leaflet() %>%
  addTiles() %>%
  setView(lng = -73.96, lat = 40.81, zoom = 16)


server <- function(input, output, session) {
  
  update_ind = reactive({
    bounds = input$map_bounds
    with(blocks, which( (start_lon<bounds$east | end_lon<bounds$east) &
                                (start_lon>bounds$west | end_lon>bounds$west) &
                                (start_lat>bounds$south | end_lat>bounds$south) &
                                (start_lat<bounds$north | end_lat<bounds$north) ) )
  })
  


  colors = colorRamp(c("red", "black", "green"))
  
  
  observe({

    score = input$tree*(blocks$tree_dens)  +  input$slope*(-blocks$slope)
    score = score[update_ind()]
    score = rank(score)
    
    leafletProxy("map") %>% clearShapes()
    k = 0
    for(i in update_ind()){
      k = k + 1
      seg_points = get_points_from_segment(blocks$the_geom[i])
      leafletProxy("map") %>% addPolylines(lng = seg_points$lon, 
                                           lat = seg_points$lat,   
                                           col = rgb(colors(score[k]/max(score))/255),
                                           weight = 7, 
                                           opacity = 1)
    }
    
  })
  
  
  output$map = renderLeaflet({
    map
  })
  
  
}

shinyApp(ui, server)








