# install.packages("devtools")
# require(devtools)
# install_github('ramnathv/rCharts@dev')
# install_github('ramnathv/rMaps')

library(rMaps)
pos<-group_by(pos,state)%>%
  mutate(count=n())

pos_new<-select(pos,state,count)
pos_new<-unique(pos_new)
names(pos_new)<-c("state","reviewCount")
library(ggplot2)

ggplot(pos_new, aes(x=state, y=reviewCount,fill=reviewCount))+
  geom_bar(stat="identity",position="dodge",width=0.9) +
  geom_text(aes(label = reviewCount, vjust = -0.8, hjust = 0.5), show.legend = TRUE)

threshhold=100
state_filter<-filter(pos_new,reviewCount<=threshhold)
state_stay<-filter(pos_new,reviewCount>threshhold)

library(maps)       # Provides functions that let us plot the maps
library(mapdata)    # Contains the hi-resolution points that mark out the countries.


library(leaflet)

pal <- colorFactor(c("navy", "red"), domain = c("ship", "pirate"))

leaflet(pos) %>% 
  addTiles() %>%
  addCircleMarkers(
    radius =reviewCount (type == "ship", 6, 10),
    color = ~pal(type),
    stroke = FALSE, fillOpacity = 0.5
  )

plotCluster<-leaflet(pos) %>% 
  addTiles() %>% 
  addMarkers(
    ~longitude, ~latitude,
    clusterOptions = markerClusterOptions()
)

