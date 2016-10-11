#Main Code
library(geosphere)
library(dplyr)
library(igraph)
library(ggmap)
load("~/Desktop/Fall2016-Proj2-grp10-master/output/Nodes.RData")
load("~/Desktop/Fall2016-Proj2-grp10-master/output/Segments.RData")
load("~/Desktop/Fall2016-Proj2-grp10-master/output/Original.Segments.RData")
source("~/Desktop/Fall2016-Proj2-grp10-master/lib/Main_Algo_Function.R")


Find.Path<-function(Start.Location,TP,SP,FP,RP,WP,Nodes,Segments,Original.Segments,Distance,End.Location,Run.Back){
  #Set StartID and EndID
  Start.Coord = geocode(Start.Location)
  Start.ID = Nearest.ID(Nodes,Start.Coord)
  if(!is.na(End.Location)){
    End.Coord = geocode(End.Location)
    End.ID = Nearest.ID(Nodes,End.Coord)
    }
  Nodes<-Score.Nodes(Nodes,RP,FP)
  if(!is.na(Distance)){
    End.Coord = GetNodes(Nodes,Start.Coord,Distance)
    End.ID = End.Coord$ID
    End.Coord = End.Coord[,1:2]
  }
  #Find Path
  Segments = Segments.Score(Segments,TP,SP,FP,RP,WP)
  Path = Shortest(Segments,Nodes,Start.ID,End.ID,Run.Back)
  Edge.index = Path$edge.index
  Edge = Path$Path
  colnames(Start.Coord) = c("Longtitude","Latitude")
  colnames(End.Coord) = c("Longtitude","Latitude")
  Route.Go = rbind(Start.Coord,Path$Nodes.Go,End.Coord)
  Route.Back = rbind(End.Coord,Path$Nodes.Back,Start.Coord)
  EDGE = Original.Segments[Edge.index,]
  Length = GetLength(EDGE)
  Route.Score = sum(1/Edge$Distance)/nrow(Edge)
  return(list(Intersection.Go = Route.Go, Intersection.Back = Route.Back,Edge = EDGE ,Length = Length, Score = Route.Score))
}

# Example
# TP = 10
# SP = 5
# RP = 10
# FP = 5
# WP = 10
# Start.Location = "Columbia University, New York"
# Distance = NA
# End.Location = "Times Square,New York"
# Run.Back = 1 #0 - same way back , 1- different way back
# Result = Find.Path(Start.Location,TP,SP,FP,RP,WP,Nodes,Segments,Original.Segments,Distance,End.Location,Run.Back=1)
# EDGE = Result$Edge
# Result$Intersection.Go
# Result$Intersection.Back

