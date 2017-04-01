library(igraph)
library(dplyr)
# UserStateR<-group_by(UserStateR,user_id)%>%
#  arrange(countS)
# test<-filter(UserStateR,countS>1)
# g <- graph_from_data_frame(test[,1:2])


g <- graph_from_data_frame(UserStateR[,1:2])
V(g)$type <- ifelse(names(V(g)) %in% UserStateR$user_id, 'user', 'state')
V(g)$label <- ifelse(V(g)$type == 'user', '', names(V(g)))
V(g)$size <- ifelse(V(g)$type == 'user', 1, 5)
V(g)$color <- ifelse(V(g)$type == 'user', 'wheat', 'salmon')
plot(g)

test2<-filter(UserStateR,countS>2)
NOuser<-length(unique(test2$user_id))
test2<-ungroup(test2)
test2<-group_by(test2,state)%>%
  mutate(degState=sum(countUS))
degstateList<-unique(select(test2,state,degState))
g2 <- graph_from_data_frame(test2,directed = F)

V(g2)$type <- ifelse(names(V(g2)) %in% UserStateR$user_id, 'user', 'state')
V(g2)$label <- ifelse(V(g2)$type == 'user', " ", paste(names(V(g2)),"\n",
                                                       as.character(degstateList$degState[degstateList$state==names(V(g2))]),
                                                       sep=""))
V(g2)$size <- ifelse(V(g2)$type == 'user', 0, 8)
V(g2)$label.cex<-ifelse(V(g2)$type == 'user',0,0.8)
V(g2)$color <- ifelse(V(g2)$type == 'user', 'wheat', 'salmon')
V(g2)$type <- ifelse(names(V(g2)) %in% UserStateR$user_id, T, F )
E(g2)$color <- heat.colors(8)[test2$countS]

bipartiteLayout<-layout.bipartite(g2, types = names(V(g2)) %in% UserStateR$state)
typeG2<-names(V(g2)) %in% UserStateR$state
bipartiteLayout[typeG2,1]<-seq(1,5*max(bipartiteLayout),length.out=sum(typeG2))

plot(g2,layout=bipartiteLayout,asp=0.1)


UserStateR<-group_by(UserStateR,countS)%>%
  mutate(countLevel=n())%>%
  arrange(countS)

unique(UserStateR$state)


UserStateR<-select(UserStateR,user_id,countS)%>%
  unique()


plottrend<-ungroup(UserStateR)
plottrend<-select(plottrend,user_id,countS)
plottrend<-unique(plottrend)
#plottrend$countS<-as.factor(plottrend$countS)

plottrend<-group_by(plottrend,countS)%>%
  mutate(countLevel=n())%>%
  select(-user_id)%>%
  unique()%>%
  ungroup()

library(ggplot2)
ggplot(plottrend,aes(x=countS,countLevel))+
  geom_line(size=1,color="brown")+
  geom_text(aes(label = countLevel, size=11,vjust = -0.25, hjust = -0.1),color="navy", show.legend = TRUE)+
  scale_x_discrete(limits=paste("",1:8,sep=""),name="user visited x state(x)")+
  scale_y_continuous(name="number of users ")+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        panel.background = element_rect(fill = "lightblue"))


