Nearest.ID<-function(Nodes,Coord){
  library(geosphere)
  D = distm(Nodes[,1:2],Coord,fun =distHaversine)[,1]
  return(Nodes[which.min(D),"ID"])
}


Score.Nodes<-function(Nodes,RP,FP){
  SN<-function(r){
    return(sign(r[4])*RP + sign(r[5])*FP)
  }
  Score = apply(Nodes,1,SN)
  Nodes = cbind(Nodes,Score)
  return(Nodes)
}


GetNodes<-function(Node,start,Distance){ #Node with coord and scores
  Nodes.coord = Node[,1:2]
  Distance.Between = distm(Nodes.coord,start,fun = distHaversine)[,1]/1000
  index = which((Distance*0.99)< Distance.Between & Distance.Between < (Distance*1.01))
  t = which.max(Node[index,6])
  return(Node[index[t],])
}



Segments.Score<-function(Segments,TP,SP,FP,RP){ #TP:Tree SP:Slope FP:Fountain RP:Restrooms 
  SS<-function(seg){
    return(TP*5*seg[7] + SP*exp(-seg[6]) + sign(seg[8])*FP+sign(seg[9])*RP) 
  }
  Score = apply(Segments,1,SS)
  Segments = cbind(Segments,1/Score)
  colnames(Segments)[dim(Segments)[2]] = "Distance"
  return(Segments)
}



Shortest<-function(New.Segments,U.Nodes,Start.ID,End.ID){
  colnames(New.Segments)[1:4] = c("Longtitude1","Latitude1","Longtitude2","Latitude2")
  Start = U.Nodes[Start.ID,]
  End = U.Nodes[End.ID,]
  minLong = min(Start[1],End[1])
  maxLong = max(Start[1],End[1])
  minLat = min(Start[2],End[2])
  maxLat = max(Start[2],End[2])
  #Filter Out the region
  Segments = New.Segments%>%filter(Longtitude1<=maxLong,Longtitude2<=maxLong,Longtitude1>=minLong,Longtitude2>=minLong)%>%
    filter(Latitude1<=maxLat,Latitude2<=maxLat,Latitude1>=minLat,Latitude2>=minLat)
  #Function to get segments score and "Distance"
  #
  #
  df = as.data.frame(Segments[c("Start","End","Distance")])
  names(df) = c("start_node","end_node","dist")
  gdf <- graph.data.frame(df, directed=FALSE)
  SHORT = shortest_paths(gdf,as.character(Start.ID),as.character(End.ID))$vpath
  EDGE = as.numeric(shortest_paths(gdf,as.character(Start.ID),as.character(End.ID),output = "epath")$epath[[1]])
  names<-V(gdf)$name  
  Sequence =as.numeric(lapply(SHORT,function(x){names[x]})[[1]])
  return(list(Path = Segments[EDGE,],Nodes = Nodes[Sequence,1:2]))
}

