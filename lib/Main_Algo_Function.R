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



Segments.Score<-function(Segments,TP,SP,FP,RP,WP){ #TP:Tree SP:Slope FP:Fountain RP:Restrooms 
  SS<-function(seg){
    return(WP*seg[6] + TP*5*seg[8] + SP*exp(-seg[7]) + sign(seg[9])*FP+sign(seg[10])*RP) 
  }
  Score = apply(Segments,1,SS)
  Segments = cbind(Segments,1/Score)
  colnames(Segments)[dim(Segments)[2]] = "Distance"
  return(Segments)
}

# New.Segments = Segments
# U.Nodes =Nodes
# library(leaflet)
# # 
# # 
# leaflet()%>%addTiles()%>%addCircleMarkers(lng = Nodes[2057,1],lat = Nodes[2057,2])
#leaflet()%>%addTiles()%>%addCircleMarkers(lng = EDGE[,1],lat = EDGE[,2])
# # A = which(Segments$Start == 2057)
# B = which(Segments$End == 2057)
# Segments1 = Segments[B,]


Shortest<-function(New.Segments,U.Nodes,Start.ID,End.ID,Run.Back){
  float = 0.01
  Start = U.Nodes[Start.ID,]
  End = U.Nodes[End.ID,]
  Segments = New.Segments
  df = as.data.frame(Segments[c("Start","End","Distance")])
  names(df) = c("start_node","end_node","dist")
  gdf <- graph.data.frame(df, directed=FALSE)
  SHORT.Go = shortest_paths(gdf,as.character(Start.ID),as.character(End.ID),weights = E(gdf)$dist)$vpath
  EDGE.Go = as.numeric(shortest_paths(gdf,as.character(Start.ID),as.character(End.ID),output = "epath",weights = E(gdf)$dist)$epath[[1]])
  names<-V(gdf)$name  
  Sequence.Go =as.numeric(lapply(SHORT.Go,function(x){names[x]})[[1]])
  if(Run.Back == 0){
    EDGE.Back = rev(EDGE.Go)
    EDGE.index = c(EDGE.Go,EDGE.Back[-1])
    EDGE = Segments[EDGE.index,]
    Sequence.Back = rev(Sequence.Go)
    Sequence = c(Sequence.Go,Sequence.Back[-1])
  }else{
  New = Segments
  New[EDGE.Go,13] = 10000*New[EDGE.Go,13]
  df.Back = as.data.frame(New[c("Start","End","Distance")])
  names(df.Back) = c("start_node","end_node","dist")
  gdf.Back <- graph.data.frame(df.Back, directed=FALSE)
  SHORT.Back = shortest_paths(gdf.Back,as.character(End.ID),as.character(Start.ID),weights = E(gdf.Back)$dist)$vpath
  EDGE.Back = as.numeric(shortest_paths(gdf,as.character(End.ID),as.character(Start.ID),output = "epath",weights = E(gdf.Back)$dist)$epath[[1]])
  names<-V(gdf)$name  
  EDGE = Segments[c(EDGE.Go,EDGE.Back[-1]),]
  Sequence.Back =as.numeric(lapply(SHORT.Back,function(x){names[x]})[[1]])
  Sequence = c(Sequence.Go,Sequence.Back[-1])
  }
  return(list(Path = EDGE,edge.index =c(EDGE.Go,EDGE.Back[-1]),Nodes.Go = Nodes[Sequence.Go,1:2],Nodes.Back = Nodes[Sequence.Back,1:2]))
}

GetLength<-function(Edge){
  GL<-function(r){
    return(distm(r[1:2],r[3:4],fun = distHaversine)[,1]/1000)
  }
  D = apply(Edge,1,GL)
  return(sum(D))
}


