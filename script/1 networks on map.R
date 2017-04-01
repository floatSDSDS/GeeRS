# create and visualize user-state networks
library(igraph)
library(dplyr)



names(GeoR)[9]<-"CountB"
GeoR<-group_by(GeoR,state)%>%
  mutate(countRByState=n())


networkR<-select(GeoR,user_id,state)

tmp <- graph.data.frame(GeoT[1:50,5:6])

g<-graph_from_data_frame(networkR[1:500,], directed = T,vertices = NULL)
V(g)$label<-NA
V(g)$size<-1
plot(g,main="UR network")
