library(data.table)
library(dplyr)
library(Matrix)

setwd("/home/floatsd/8sem/Thesis/YelpRS/src")
GeoR<-as.data.frame(fread("GeoR",header=T))
head(GeoR)
latmin<-min(GeoR$latitude)
latmax<-max(GeoR$latitude)
lonmin<-min(GeoR$longitude)
lonmax<-max(GeoR$longitude)

library(fpc)

setwd("/home/floatsd/8sem/Lab_Yelp/yelpROUND8/rawdata/csv")
business<-as.data.frame(fread("business.csv",header=T))
business<-select(business,business_id,categories,latitude,longitude)

noGrid=10
gridLon<-seq(lonmin-1,lonmax+1,length.out = noGrid)
gridLat<-seq(latmin-1,latmax+1,length.out = noGrid)

business$latLabel<-1
business$lonLabel<-1

for(i in 2:noGrid){
  business$latLabel[which(business$latitude>gridLat[i-1]&business$latitude<gridLat[i])]<-i
  business$lonLabel[which(business$longitude>gridLon[i-1]&business$longitude<gridLon[i])]<-i
}



temp<-group_by(business,latLabel,lonLabel)%>%
  mutate(count=n())%>%
  select(latLabel,lonLabel,count)%>%
  unique()

temp<-arrange(temp,lonLabel,latLabel)
sparse<-sparseMatrix(i = temp$latLabel,j=temp$lonLabel,x = temp$count)
sparse
# 10 x 10 sparse Matrix of class "dgCMatrix"
# 
# [1,] .     .    .    .    . . . . .    .
# [2,] . 36505    .    .    . . . . .    .
# [3,] . 23598    . 7160    . . . . .    .
# [4,] .     .    .  807    . . . . .    .
# [5,] .     .  576 4088    . . . . .    .
# [6,] .     . 2491  530 5592 . . . .    .
# [7,] .     .    .    .    . . . . .    .
# [8,] .     .    .    .    . . . . . 1074
# [9,] .     .    .    .    . . . . .    .
# [10,] .     .    .    .    . . . . . 3480

temp$label<-1
temp<-arrange(temp,desc(count))

temp$label<-ifelse(temp$count==1074|temp$count==3480,3,2)
temp$label[1:2]<-1

business<-left_join(business,temp)
business<-select(business,business_id,latLabel,lonLabel,label)
rm(temp,sparse,latmax,latmin,lonmax,lonmin,sparse,i,gridLat,gridLon,matrix)

head(business)
business<-select(business,business_id,label)
GeoR<-left_join(GeoR,business)

library(leaflet)

business2<-as.data.frame(fread("business.csv",header=T))
business<-select(business,business_id,label)
business2<-left_join(business2,business)
rm(business)

GeoR1<-filter(GeoR,label==1)
userlist<-unique(GeoR1$user_id)
GeoR1<-GeoR[GeoR$user_id %in% userlist,]
rm(GeoR)

summary(GeoR1)
statelist<-unique(GeoR1$state)
business1<-business2[business2$state %in% statelist,]
rm(business2)

business<-business1
business1<-filter(business,label==1)
business2<-filter(business,label==2)
business3<-filter(business,label==3)

rm(business)

statelist1<-unique(business1$state)
statelist2<-unique(business2$state)
statelist3<-unique(business3$state)

business<-rbind(business1,business2,business3)
rm(business1,business2,business3)

names(business)

business$state2<-as.factor(business$state)

library(leaflet)

pal <- colorFactor(rainbow(23), domain = statelist1)
leaflet(filter(business,label==1,state!="AZ",state!="NV")) %>% 
  addTiles() %>%
  addCircleMarkers(
    radius = 10,
    color = ~pal(state),
    stroke = F, fillOpacity = 1,
    popup= ~state
  )


