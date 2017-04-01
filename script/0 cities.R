
library(stringr)
library(stringdist)
library(tidyr)
library(ggplot2)
library(reshape)

# select a business basic GeoInfo table
business_Geo<-select(business,business_id,state,city,longitude,latitude)
#business_Geo<-pos


# create a vector of cities
unique_city<-sort(unique(business_Geo$city))
change_city<-as.data.frame(unique_city)%>%
  mutate(changed=unique_city)
count<-mapply(function(str){return(dim(filter(business_Geo,city==str))[1])},change_city$changed)
change_city<-cbind(change_city,count)

ggplot(change_city, aes(x=unique_city, y=count,fill=count))+
  geom_bar(stat="identity",position="dodge",width=0.9) #+
#  geom_text(aes(label = count, vjust = -0.8, hjust = 0.5), show.legend = TRUE)


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
change_city$changed<-tolower(change_city$changed)
change_city$changed<-sub("","",change_city$changed)

new_city<-select(change_city,-unique_city)
new_city<-group_by(new_city,changed)%>%
  mutate(sum=sum(count))
new_city<-select(new_city,-count)
new_city<-unique(new_city)




city_method<-c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")
sim<-list()
img<-list()
melt<-list()
Outputname<-vector()
setwd("L:/7sem/YelpRS thesis/result/0 cities/1.1 after tolower")
for (i in 1:length(city_method)){
  sim[[i]]<-stringdistmatrix(new_city$changed,new_city$changed,method=city_method[i],p=0.1)
  melt[[i]]<-melt(sim[[i]])
  #Outputname<-paste(city_method[i],".csv")
  #write.csv(sim[[i]],Outputname,row.names = F)
}

# output plot
for(i in 1:10){
  img[[i]]<-ggplot(data = melt[[i]]) + geom_tile(aes(x = X1, y = X2, fill = value))
  ggsave(filename=paste(city_method[i],".jpg"), plot=img[[i]])
}
  
View(sim[[10]])

plotindex<-2
View(sim[[plotindex]])
max(sim[[plotindex]])
threshhold=10
plotsim<-matrix(sim[[plotindex]])
plotsim[which(plotsim<threshhold)]=0
plotsim[which(plotsim>=threshhold)]=1
plotmelt<-melt(plotsim)
plotimg<-ggplot(data = plotmelt) + geom_tile(aes(x = X1, y = X2, fill = value))
ggsave(filename=paste("test_",city_method[plotindex],".jpg"), plot=plotimg)



