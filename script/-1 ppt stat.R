install.packages("diagram")#
install.packages("plotrix")#
install.packages("nycflights13")#
install.packages("rworldmap")#
install.packages("WDI")

library(diagram)  
library(plotrix)
library(nycflights13)
library(rworldmap)
library(WDI) # WORLD BANK INDICATORS
library(data.table)
library(maps)
library(geosphere)
library(RgoogleMaps)
library(dplyr)
library(leaflet)
library(ggplot2)

statelistR<-unique(GeoR$state)
statelistT<-unique(GeoT$state)
geoR<-sapply(statelistR,getGeoCode)
geoT<-sapply(statelistT,getGeoCode)
stateboth<-statelistR[statelistR%in%statelistT]
statenot<-statelistR[!statelistR%in%statelistT]
geoboth<-sapply(stateboth,getGeoCode)
geonot<-sapply(statenot,getGeoCode)  

getStateGeo<-group_by(GeoR,state)

# check stat
userlistR<-unique(GeoR$user_id)
userlistT<-unique(GeoT$user_id)
businesslistRT<-unique(GeoRT$business_id)
businesslistR<-unique(GeoR$business_id)
businesslistT<-unique(GeoT$business_id)

rm(businesslistR,businesslistT,businesslistRT)
rm(statelistR,statelistT,userlistR,userlistT)
rm(flag,count,df1)

map("world", fill=T, col="grey66")#,ylim=c(-20,80), xlim=c(-130,100))
points(geoR$lat[geoR$flag],geoR$lon[geoR$flag],pch=19,cex=1, col="red")

#########################################
SBR<-select(GeoR,state,business_id)
SUR<-select(GeoR,state,user_id)
SR<-select(GeoR,state)

SBT<-select(GeoT,state,business_id)
SUT<-select(GeoT,state,user_id)
ST<-select(GeoT,state)

SBRT<-unique(SBRT)%>%
  group_by(state)%>%
  mutate(SBRT=n())%>%
  select(-business_id)%>%
  unique()

SUR<-unique(SUR)%>%
  group_by(state)%>%
  mutate(SUR=n())%>%
  select(-user_id)%>%
  unique()

SRT<-group_by(SRT,state)%>%
  mutate(SRT=n())%>%
  unique()
  

SBRT<-select(GeoRT,state,business_id)
SURT<-select(GeoRT,state,user_id)
SRT<-select(GeoRT,state)

rm(GeoRT)
rm(GeoT)

statesInfo<-left_join(SBR,SUR)
statesInfo<-left_join(statesInfo,SR)%>%
  left_join(SBT)%>%
  left_join(SUT)%>%
  left_join(ST)%>%
  left_join(SBRT)%>%
  left_join(SURT)%>%
  left_join(SRT)
statesInfo$flag<-SR$state%in%ST$state

setwd("L:/7sem/YelpRS thesis/src")
write.csv(statesInfo,"statesInfo.csv",row.names=F)
rm(SR,ST,SRT,SBR,SBT,SBRT,SUR,SUT,SURT)
rm(flag)

statesInfo[!statesInfo$flag,5:7]<-0
statesInfo<-as.data.frame(fread("statesInfo.csv",header=T))

pal <- colorFactor(c("navy","sandybrown"), domain = c(T,F))

geo<-select(GeoR,state,longitude,latitude)
geo<-group_by(geo,state)%>%
  mutate(lon=mean(longitude),lat=mean(latitude))%>%
  select(-longitude,-latitude)
geo<-unique(geo)
statesInfo<-left_join(statesInfo,geo)

leaflet(statesInfo) %>% 
  addTiles() %>%
  addCircleMarkers(
    radius = ~ifelse(flag==T,0.001*SBRT,2*SBRT),
    color = ~pal(flag),
    stroke = T, fillOpacity = 0.5
  )

