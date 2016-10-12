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
library(data.table)
load("../output/Nodes.RData")
load("../output/Segments.RData")
load("../output/Original.Segments.RData")
source("../lib/Main_Algo_Function.R")
source("../lib/Main_Algo_Code2.R")
source("../lib/get_points_from_segment.R")
blocks = fread('../output/blocks_Manhattan.csv')
restrooms = fread('../data/restroom_coordinates.csv')
fountains = fread('../data/drink_location.csv')


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
    } else {
      return(c(NA,NA,NA,NA))
    }
  }
  
  
  #__________________________________ICON______________________________________________________#
  # make icon
  start <- makeIcon(
    iconUrl = "../data/super_mario.png",
    iconWidth = 25, iconHeight = 40
  )
  
  end = makeIcon(
    iconUrl = "../data/princess.jpg",
    iconWidth = 25, iconHeight = 40
  )
  
  toilet_icon=makeIcon(
    iconUrl = "../data/toilet.png",
    iconWidth = 25, iconHeight = 30
  )
  
  fountain_icon=makeIcon(
    iconUrl = "../data/fountain.png",
    iconWidth = 25, iconHeight = 30
  )
  
  
  
  #____________________________________________MAP______________________________________________________#
  
  
  #functions to create markers and polylines
  showRoutine <- function(lng,lat,col) {
    leafletProxy("map") %>% addPolylines(lng, lat, color = col)
  }
  
  showRestroom<-function(lng,lat,restroom){
    leafletProxy("map") %>% addMarkers(lng,lat,icon = toilet_icon)
  }
  
  showDrink<-function(lng,lat,drink){
    leafletProxy("map") %>% addMarkers(lng,lat,icon = fountain_icon)
  } 
  
  #Creating paths for map
  observe({
    if(input$end_dis == 1){
      leafletProxy("map") %>% clearShapes()
      event <- Find.Path(input$start,input$tree,input$slope,
                         input$foutain,input$restroom,input$width,Nodes,Segments,
                         Original.Segments,
                         NA,input$stop,input$return)
    }else{
      leafletProxy("map") %>% clearShapes()
      event <- Find.Path(input$start,input$tree,input$slope,
                         input$foutain,input$restroom,input$width,Nodes,Segments,
                         Original.Segments,
                         input$distance,NA,input$return)
      leafletProxy("map") %>% addMarkers(event$End.Point$Longtitude,event$End.Point$Latitude, icon = end)
    }
    
    #inital run
    isolate({
      showRoutine(lng=as.vector(event$Intersection.Go$Longtitude),
                  lat=as.vector(event$Intersection.Go$Latitude),
                  col = "#FF0088")
    })
    
    #return run
    isolate({
      showRoutine(lng=as.vector(event$Intersection.Back$Longtitude),
                  lat=as.vector(event$Intersection.Back$Latitude),
                  col = "#5500FF")
    })
    
    output$summary_text <- renderPrint(
      paste(round(event$Length),"kilometers")
    )
  
  #Display bathroom and fountains on path only
    if(input$RP_layer || input$FO_layer){

      leafletProxy("map") %>% clearMarkers() %>%
        addMarkers(data = points_start(),icon=start) %>%
        addMarkers(data = points_end(),icon=end)

      rl<-cbind(event$Edge$Longtitude1[which(event$Edge$Restroom>0)],
                event$Edge$Latitude1[which(event$Edge$Restroom>0)],
                event$Edge$Longtitude2[which(event$Edge$Restroom>0)],
                event$Edge$Latitude1[which(event$Edge$Restroom>0)]
      )
      dl<-cbind(event$Edge$Longtitude1[which(event$Edge$Fountain>0)],
                event$Edge$Latitude1[which(event$Edge$Fountain>0)],
                event$Edge$Longtitude2[which(event$Edge$Fountain>0)],
                event$Edge$Latitude1[which(event$Edge$Fountain>0)]
      )

      if(input$RP_layer == 1){
        n<-nrow(rl)
        rest_lat<-vector()
        rest_lng<-vector()


        for (i in 1:n == 1){
          rest_lng[i]<-mean(rl[i,1],mean(c(rl[i,1],rl[i,3])))
          rest_lat[i]<-mean(rl[i,2],mean(c(rl[i,2],rl[i,4])))
        }

        isolate({
          showRestroom(lng = rest_lng, lat = rest_lat, restroom = restroom)
        })

      }


      if(input$FO_layer == 1){
        m<-nrow(dl)
        drink_lat<-vector()
        drink_lng<-vector()


        for(i in 1:m){
          drink_lng[i]<-mean(c(dl[i,1],dl[i,3]))
          drink_lat[i]<-mean(c(dl[i,2],dl[i,4]))
        }

        isolate({
          showDrink(lng=drink_lng,lat=drink_lat,drink =drink)
        })
      }
    }

  })
  
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
  
  # Create map
  output$map <- renderLeaflet({
    if (input$end_dis == 1){
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>% setView(lng = -73.96411, lat =40.807722, zoom=17 ) %>%
     
      addMarkers(data = points_start(),icon=start) %>%
      addMarkers(data = points_end(),icon=end)} else{
          leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>% setView(lng = -73.96411, lat =40.807722, zoom=17 ) %>%
     
      addMarkers(data = points_start(),icon=start) 
      }
  })
  

  #_____________________________________________Factor Exploration___________________________________________#

  # Create map
  output$map2 <- renderLeaflet({
      leaflet() %>%
        addTiles(
          urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
          attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
        ) %>% setView(lng = -73.96411, lat =40.807722, zoom=16 )
  })

  #Display bathroom and fountains
  fountains = fountains %>%
    select(lon, lat) %>%
    filter(!is.na(lon) & !is.na(lat))
  
  observe({
    leafletProxy("map2") %>% clearMarkers()
    if (input$RP_layer_all)
      leafletProxy("map2") %>% addMarkers(lng = restrooms$LNG, lat = restrooms$LAT, icon = toilet_icon)
    if (input$FO_layer_all)
      leafletProxy("map2") %>% addMarkers(lng = fountains$lon, lat = fountains$lat, icon = fountain_icon)
  })
  
  update_ind = reactive({
    bounds = input$map2_bounds
    with(blocks, which( (start_lon<bounds$east | end_lon<bounds$east) &
                          (start_lon>bounds$west | end_lon>bounds$west) &
                          (start_lat>bounds$south | end_lat>bounds$south) &
                          (start_lat<bounds$north | end_lat<bounds$north) ) )
  })
  
  #draw segments:
  colors = colorRamp(c("red", "black", "green"))
  
  observe({
    if(input$SP_TR){
      score = input$tree_pre*(blocks$tree_dens)  +  input$slope_pre*(-blocks$slope)
      score = score[update_ind()]
      score = rank(score)
      
      leafletProxy("map2") %>% clearShapes()
      k = 0
      for(i in update_ind()){
        k = k + 1
        seg_points = get_points_from_segment(blocks$the_geom[i])
        leafletProxy("map2") %>% addPolylines(lng = seg_points$lon, 
                                              lat = seg_points$lat,   
                                              col = rgb(colors(score[k]/max(score))/255),
                                              weight = 3, 
                                              opacity = 1)
      }
    
    }
    if(input$SP_TR == 0){
      leafletProxy("map2") %>% clearShapes()
    }
    
  })

})
