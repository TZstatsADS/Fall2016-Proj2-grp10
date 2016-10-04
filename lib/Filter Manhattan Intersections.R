library(readr)
library(mgcv)
Intersection = read_csv("node_list.csv")
colnames(Intersection)[3:4] = c("longtitude","latitude")
Boundary = read_csv("Boundary.csv")
Boundary = as.matrix(Boundary)
Intersection_co = as.matrix(Intersection[,3:4])
In.Out = in.out(Boundary,Intersection_co)
Manhattan_Intersection = Intersection[In.Out,]
#write.csv(Manhattan_Intersection,"Manhattan_Intersection.csv",row.names = FALSE)
A = as.data.frame(Manhattan_Intersection[,3:4])
library(leaflet)
leaflet(A)%>%addTiles()%>%addCircles(~longtitude,~latitude)

