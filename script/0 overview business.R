
library(data.table)
library(dplyr)

rm(GeoRT,GeoT)
setwd("L:/7sem/YelpRS thesis/YELPdataset")
business<-as.data.frame(fread("business.csv",header=T))
setwd("L:/7sem/YELP/yelpROUND8/rawdata/csv")
list.files()
user<-as.data.frame(fread("user.csv",header=T))
View(user)
View(business)

#PA NC SC WI IL AZ CA NV FL NM QC ON TX EDH MLN HAM SCB ELN FIF NTH XGL BW RP KHL MN NW TAM AL AK
state<-unique(business$state)
state
temp<-filter(business,state=="")
city<-unique(business$city)
View(as.matrix(sort(city)))

pos<-select(business,business_id,state,city,latitude,longitude)
statelist<-unique(pos$state)


options(geonamesUsername="floatsd")
library(geonames)
GNcountrySubdivision(45.45614,-73.752866)



p=1
pos_sample$Rstate<-mapply(function(lat,lng){
  return(GNcountrySubdivision(lat,lng)$codes[[2]]$code)
},pos_sample$latitude,pos_sample$longitude)


pos_sample[which(pos_sample$state!="EDH")]$CountryCode<-mapply(function(lat,lng){
  return(GNcountrySubdivision(lat,lng)$countryCode)
},pos_sample[which(pos_sample$state!="EDH")]$latitude,pos_sample[which(pos_sample$state!="EDH")]$longitude)


pos_sample$CountryName<-mapply(function(lat,lng){
  return(GNcountrySubdivision(lat,lng)$countryName)
},pos_sample$latitude,pos_sample$longitude)

threshhold=10
pos_few<-filter(pos,count<=threshhold)
pos_more<-filter(pos,count>threshhold)

pos_sample<-pos_more %>% 
  group_by(state) %>%
  do(sample_n(.,5))

pos_sample<-rbind(pos_sample,pos_few)
pos_sample<-rbind(pos_sample,filter(pos,city==""))

rm(pos_few,pos_more)

temp<-filter(pos,state==""|city=="")


pos[which(pos$state==""),]$state<-"QC"
View(pos[which(pos$state=="QC"),])

#http://noc.to/geodecode
pos[which(pos$business_id=="DWUwG--s-OiFyyHGXG2jgg"),]$city<-"Tempe"
pos[which(pos$business_id=="y9bh4lBIELCGPZgatRdDvw"),]$city<-"Edinburgh"

state<-unique(pos$state)

