#Main Code
library(geosphere)
library(dplyr)
library(igraph)
library(ggmap)
load("Nodes.RData")
load("Segments.RData")
source("Main_Algo_Function.R")
TP = 10
SP = 5
RP = 10
FP = 5
Start.Coord = geocode("Columbia University, New York")
Distance = 4
End.Coord =  NA
Run.Back = 1 #0 - same way back , 1- different way back
EDGE = Find.Path(Start.Coord,TP,SP,FP,RP,Nodes,Segments,Distance,End.Coord,Run.Back=1)$Edge

Find.Path<-function(Start.Coord,TP,SP,FP,RP,Nodes,Segments,Distance,End.Coord,Run.Back){
  #Set StartID and EndID
  Start.ID = Nearest.ID(Nodes,Start.Coord)
  if(length(End.Coord) == 2){End.ID = Nearest.ID(Nodes,End.Coord)}
  Nodes<-Score.Nodes(Nodes,RP,FP)
  if(!is.na(Distance)){
    End.Coord = GetNodes(Nodes,Start.Coord,Distance/2)
    End.ID = End.Coord$ID
  }
  #Find Path
  Segments = Segments.Score(Segments,TP,SP,FP,RP)
  Path = Shortest(Segments,Nodes,Start.ID,End.ID,Run.Back)
  Edge = Path$Path
  Route = Path$Nodes
  Length = sum(Edge$Length)/1000
  Route.Score = sum(1/Edge$Distance)/Length
  return(list(Intersection = Route, Edge = Edge,Length = Length, Score = Route.Score))
}



