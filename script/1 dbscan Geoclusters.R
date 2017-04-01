# dbscan and create different levels of Geoclusters
library(igraph)
library(ggplot2)
library(fpc)
library(data.table)
library(dplyr)
library(leaflet)
library(htmltools)

NO<-500
test<-filter(GeoR,state=="SC")
dbscanRT<-dbscan(select(test,latitude,longitude),0.001,3)
NOcluster<-length(unique(dbscanRT$cluster))-1
#temp<-GeoR[1:NO,]
temp<-cbind(test,geolabel=dbscanRT$cluster)
temp<-group_by(temp,geolabel)%>%
  mutate(CountGeoLabel=n())

pal <- colorFactor(heat.colors(NOcluster),domain=1:NOcluster)
temp1<-filter(temp,geolabel!=0)
temp2<-filter(temp,geolabel==0)
leaflet(temp1) %>% 
  addTiles() %>%
  addCircleMarkers(
    ~longitude, ~latitude,
    radius=0.1,
    fillOpacity=0,
    color = ~pal(geolabel),
    popup=~paste("Label:",as.character(geolabel),"number:",as.character(CountGeoLabel))
  )%>%
  addCircleMarkers(
    ~longitude, ~latitude,
    color="red",
    data=temp2,
    popup="isolated"
  )

GeoR<-group_by(GeoR,state)%>%
  mutate(countR=n())
statGeoR<-select(GeoR,state,countR)%>%
  unique()

ggplot(statGeoR, aes(x=state, y=countR,fill=countR))+
  geom_bar(stat="identity",position="dodge",width=0.9) +
  geom_text(aes(label = countR, vjust = -0.8, hjust = 0.5), show.legend = TRUE)



