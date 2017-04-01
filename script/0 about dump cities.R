
library(stringr)
library(stringdist)
library(ggplot2)

# select a business basic GeoInfo table
business_Geo<-select(business,business_id,state,city,longitude,latitude)

# create a vector of cities
unique_city<-sort(unique(business_Geo$city))
change_city<-as.data.frame(unique_city)%>%
  mutate(changed=unique_city)
count<-mapply(function(str){return(dim(filter(business_Geo,city==str))[1])},change_city$changed)
change_city<-cbind(change_city,count)

# normalized the city vector
# replace all the Chinese characters
change_city$changed<-gsub("茅","e",change_city$changed)
change_city$changed<-gsub("猫","e",change_city$changed)
change_city$changed<-gsub("脦","i",change_city$changed)
change_city$changed<-gsub("脢","",change_city$changed)
change_city$changed<-gsub("枚","o",change_city$changed)

# remove all the . , -
change_city$changed<-gsub("-"," ",change_city$changed)
#change_city$changed<-gsub("","",change_city$changed)
change_city$changed<-sub(",.[A-Z][a-zA-Z]$","",change_city$changed)


city_method<-c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")
sim<-list()
img<-list()
melt<-list()
Outputname<-vector()
setwd("L:/7sem/YelpRS thesis/result/0 cities")
for (i in 1:length(city_method)){
  sim[[i]]<-stringdistmatrix(change_city$changed,change_city$changed,method=city_method[i],p=0.1)
  melt[[i]]<-melt(sim[[i]])
  #Outputname<-paste(city_method[i],".csv")
  #write.csv(sim[[i]],Outputname,row.names = F)
}

# output plot
for(i in 1:10){
  img[[i]]<-ggplot(data = melt[[i]]) + geom_tile(aes(x = Var1, y = Var2, fill = value))
  #ggsave(filename=paste(city_method[i],".jpg"), plot=img[[i]])
}
  


