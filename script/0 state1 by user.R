### get state==1 by user
library(dplyr)
library(leaflet)
library(data.table)
library(leaflet)

setwd("")

checkAZNV<-filter(UserStateR,countS==1)%>%
  group_by(state)%>%
  mutate(countOneState=n())%>%
  arrange(countOneState,countUS)%>%
  select(-countS)
normtest<-select(checkAZNV,-user_id,-countUS)%>%
  unique()
rm(checkAZNV)


leaflet(normtest) %>% 
  addTiles() %>%
  addCircleMarkers(
    ~meanlon,~meanlat,
    radius = ~log(countOneState+1),
    stroke = T, fillOpacity = 0.5
  )%>%
  addPopups(
    ~meanlon,~meanlat,
    ~paste(state,as.character(countOneState)),
    options = popupOptions(closeButton = FALSE)
  )
