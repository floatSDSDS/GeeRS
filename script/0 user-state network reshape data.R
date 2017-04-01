library(maps)
library(geosphere)

stateR<-select(GeoR,user_id,state,latitude,longitude)
stateR<-group_by(stateR,state)%>%
  mutate(meanlat=mean(latitude),meanlon=mean(longitude))
stateR<-select(stateR,-latitude,-longitude)

UserStateR<-group_by(stateR,user_id,state)%>%
  mutate(countUS=n())%>%
  arrange(user_id,state)
UserStateR<-unique(UserStateR)

UserStateR<-group_by(UserStateR,user_id)%>%
  mutate(countS=n())


library(igraph)
g <- graph_from_data_frame(UserStateR[,1:2])
V(g)$type <- ifelse(names(V(g)) %in% UserStateR$user_id, 'user', 'state')
V(g)$label <- ifelse(V(g)$type == 'user', '', names(V(g)))
V(g)$size <- ifelse(V(g)$type == 'user', 1, 5)
V(g)$color <- ifelse(V(g)$type == 'user', 'wheat', 'salmon')
plot(g)



