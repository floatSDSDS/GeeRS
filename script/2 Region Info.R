rm(gridLat,i,gridLon,sparse,sparse2,latmax,latmin,lonmax,lonmin)
rm(noGrid)
rm(temp)

business2_3<-as.data.frame(fread("business2_3.csv",header=T))

names(business1)[99:100]<-c("label1","label2")
names(business2_3)[99:100]<-c("label1","label2")

temp<-select(rbind(business1,business2_3),label1,label2)
business<-rbind(business1,business2_3)
rm(business1,business2_3,temp)

business<-group_by(business,label1,label2)%>%
  mutate(meanLat=mean(latitude),meanLon=mean(longitude),countRegion=n())
head(business[,99:103])

business<-mutate(business,maxLat=max(latitude),minLat=min(latitude),
                 maxLon=max(longitude),minLon=min(longitude))

regionInfo<-select(business,label1,label2,meanLat,meanLon,countRegion,maxLat,minLat,maxLon,minLon)%>%
  unique()
regionInfo

sublat<-abs(regionInfo$maxLat-regionInfo$minLat)
sublon<-abs(regionInfo$maxLon-regionInfo$minLon)
area<-sublat*sublon
sublon

regionInfo$area<-area

write.csv(regionInfo,"regionInfo.csv",row.names=F)

names(business)[101]
business[,101:107]<-NULL

write.csv(business,"businessPool.csv",row.names=F)

rm(area,sublat,sublon)
