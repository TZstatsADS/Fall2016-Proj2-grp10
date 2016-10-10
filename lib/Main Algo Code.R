#Main Code
library(geosphere)
library(dplyr)
library(igraph)
library(ggmap)
#load("./Nodes.RData")
#load("./Segments.RData")
#source("Main Algo Function.R")

#TP:Tree SP:Slope FP:Fountain RP:Restrooms 
TP = 3
SP = 5
RP = 2
FP = 3
Start.Coord = geocode("Columbia University, New York")
Distance = 5
End.Coord =  NA


Find.Path<-function(Start.Coord,TP,SP,FP,RP,Nodes,Segments,Distance,End.Coord){
  #Set StartID and EndID
  Start.ID = Nearest.ID(Nodes,Start.Coord)
  if(length(End.Coord) == 2){End.ID = Nearest.ID(Nodes,End.Coord)}
  Nodes<-Score.Nodes(Nodes,RP,FP)
  if(!is.na(Distance)){
    End.Coord = GetNodes(Nodes,Start.Coord,Distance)
    End.ID = End.Coord$ID
  }

  #Find Path
  Segments = Segments.Score(Segments,TP,SP,FP,RP)
  
  Path = Shortest(Segments,Nodes,Start.ID,End.ID)
  Route = Path$Nodes #Coordinates in order
  Edge = Path$Path #Segments in order
  Length = sum(Edge$Length)
  Route.Score = round(sum(1/Edge$Distance))/dim(Edge)[1]
  return(list(Intersection = Route, Edge = Edge,Length = Length, Score = Route.Score))
}

Find.Path(Start.Coord,TP,SP,FP,RP,Nodes,Segments,Distance,End.Coord)
