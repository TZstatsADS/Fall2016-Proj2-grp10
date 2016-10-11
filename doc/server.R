library(shiny)
library(leaflet)
library(lattice)
library(dplyr)
library(ggplot2)
library(RCurl)
library(RJSONIO)
library(plyr)
library(ggplot2)
library(plotly)
load("Nodes.RData")
load("Segments.RData")
source("Main Algo Function.R")
source("Main Algo Code.R")


shinyServer(function(input, output) {
  # _____________________________________________API: geocode______________________________________________#
  #https://maps.googleapis.com/maps/api/geocode/json?address=1600+Amphitheatre+Parkway,+Mountain+View,+CA&key=AIzaSyC82ht4goSYy9M7Dp9tXc-vO9qxCoeF0jM'
  # Geocode API
  url <- function(address, return.call = "json", sensor = "false") {
    root <- "https://maps.google.com/maps/api/geocode/"
    u <- paste(root, return.call, "?address=", address,"&key=AIzaSyC82ht4goSYy9M7Dp9tXc-vO9qxCoeF0jM", sep = "")
    return(URLencode(u))
  }
  
  geoCode <- function(address,verbose=FALSE) {
    if(verbose) cat(address,"\n")
    u <- url(address)
    doc <- getURL(u) #download JSON 
    x <- fromJSON(doc,simplify = FALSE) # convert JSON into list
    # check is the status is ok
    if(x$status=="OK") {
      lat <- as.numeric(x$results[[1]]$geometry$location$lat)
      lng <- as.numeric(x$results[[1]]$geometry$location$lng)
      location_type <- x$results[[1]]$geometry$location_type
      formatted_address <- x$results[[1]]$formatted_address
      return(c(lat, lng, location_type, formatted_address))
      Sys.sleep(0.5)
    } else {
      return(c(NA,NA,NA,NA))
    }
  }
  #____________________________________________MAP______________________________________________________#
  # make icon
  start <- makeIcon(
    iconUrl = "https://www.adiumxtras.com/images/pictures/super_mario_3d_icons_1_35820_8077_image_12514.png",
    iconWidth = 25, iconHeight = 40
  )
  
  end = makeIcon(
    iconUrl = "https://s-media-cache-ak0.pinimg.com/originals/f9/0b/32/f90b326586d33de6a5ff78ff2605df9c.jpg",
    iconWidth = 25, iconHeight = 40
  )
  
  # Find point from input
  points_start <- eventReactive(input$start,{
    address <- input$start
    locations <- ldply(address,function(x) geoCode(x))
    names(locations) <- c("lat", "lon", "location_type", "formatted")
    return (cbind(locations$lon,locations$lat))
  })
  
  points_end <- eventReactive(input$stop,{
    address <- input$stop
    locations <- ldply(address,function(x) geoCode(x))
    names(locations) <- c("lat", "lon", "location_type", "formatted")
    return (cbind(locations$lon,locations$lat))
  })
  
  #######Currently working here
  #Update button
  output$action=reactive({
    path <- Find.Path(points_start,input$tree,input$slope,input$foutain,Nodes,Segments,input$distance,points_end)
    
    #put in code as input and output
    #This line needs help
    leafletProxy("map") %>% clearShapes() %>%
      addPolylines(path$Edge$Longtitude1,path$Edge$Latitude1,path$Edge$Longtitude2,path$Edge$Latitude2)
  })
  
  # Create map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>% setView(lng = -73.96411, lat =40.807722, zoom=17 ) %>%
     
      addMarkers(data = points_start(),icon=start) %>%
      addMarkers(data = points_end(),icon=end) 

  })
  
  
  #_____________________________________________Factor Exploration___________________________________________#

  #need help
  datasetInput <- reactive({
    switch(input$dataset,
           "Legnth" = Segments$Length,
           "Slope" = Segments$Slope,
           "Tree" = Segments$Tree,
           "Drink Fountain" = Segments$Fountain,
           "Toliet" = Segments$Restroom)
  })
  
  output$summary <- renderPrint({
    summary(Segments[5:11])
  })
  
  output$distribution <- renderPlotly({
    plot_ly(x = datasetInput(), type = "histogram") 
    #plot_ly(x = Segments$Tree, type = "histogram") 
  })
  
  output$table <- DT::renderDataTable(DT::datatable({
    Segments[5:11]
  }))
  
}
)
