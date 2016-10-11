library(shiny)
library(leaflet)
library(lattice)
library(dplyr)
library(ggplot2)
library(RCurl)
library(RJSONIO)
library(plyr)
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
  
  #Buttom for calculation
  output$action=reactive({
    #TP:Tree SP:Slope FP:Fountain RP:Restrooms 
    #Start.Coord,TP,SP,FP,RP,Nodes,Segments,Distance,End.Coord
    Find.Path(points_start,input$tree,input$slope,input$foutain,Nodes,Segments,input$distance,points_end)
    #Catherine: this is partly done, I need to import the dataset for the 
  })
  
  # When user clicks on segment, information will appear
  observe({
    leafletProxy("map") %>% clearGroup("overlays")
    event <- input$map_marker_click
    if (is.null(event))
      return()
    isolate({
      show()(event$Length, event$Slope, event$Tree,event$Foutain)
    })
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
  
}
)
