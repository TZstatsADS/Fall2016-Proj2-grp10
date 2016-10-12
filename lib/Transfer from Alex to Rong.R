library(readr)
library(dplyr)
Data = read.csv("blocks_Manhattan.csv")
Data =as.data.frame(Data)

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

Segments = Data%>%mutate(Longtitude1 = start_lon,
                         Latitude1 = start_lat,
                         Longtitude2 = end_lon,
                         Latitude2 = end_lat,
                         Length = SHAPE_Leng,
                         Width = ST_WIDTH,
                         Slope = slope,
                         Tree = tree_dens,
                         Fountain = n_fountains,
                         Restroom = n_restrooms)%>%select(Longtitude1,Latitude1,Longtitude2,Latitude2,Length,Width,Slope,Tree,Fountain,Restroom)


U.Nodes = split.to.nodes(Segments)
Trim.Nodes = Clean.Nodes(U.Nodes,40)
New.Segments = assign.names.Segments(Trim.Nodes,Segments)
head(New.Segments)
Segments = New.Segments

Segments$Slope = range01(Segments$Slope)
Segments$Tree = 2*Segments$Tree
Segments$Width = range01(Segments$Width)

save(Segments,file = "Segments.RData")

