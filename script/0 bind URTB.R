setwd("L:/7sem/YELP/yelpROUND8/YELPdataset/reviews")
filelist<-list.files()
i=1
reviewtemp<-as.data.frame(fread(filelist[i],header=T))
URtemp<-select(reviewtemp,business_id,user_id,review_id,stars,date)
for (i in 2:length(filelist)-1){
  reviewtemp<-as.data.frame(fread(filelist[i],header=T))
  URtemp<-rbind(URtemp,select(reviewtemp,business_id,user_id,review_id,stars,date))
}
rm(reviewtemp)


setwd("L:/7sem/YELP/yelpROUND8/rawdata/csv")
filelist<-list.files()
tiptemp<-as.data.frame(fread("tip.csv",header=T))
tiptemp<-select(tiptemp,business_id,user_id,date)



setwd("L:/7sem/YelpRS thesis/src")
write.csv(URtemp,"URB-stars&date.csv",row.names = F)
write.csv(tiptemp,"UB-date.csv",row.names = F)

URT<-rbind(select(URtemp,business_id,user_id,date),tiptemp)
write.csv(URT,"URTB-date.csv",row.names=F)

GeoT<-left_join(tiptemp,pos,by="business_id")
rm(tiptemp)
GeoR<-left_join(URtemp,pos,by="business_id")
rm(URtemp)
GeoRT<-left_join(URT,pos,by="business_id")
rm(URT)

GeoR<-select(GeoR,-city)
GeoT<-select(GeoT,-city)
GeoRT<-select(GeoRT,-city)

rm(city_method,count,pos_new,pos_sample,temp,business_Geo,change_city,new_city,plotmelt)
rm(plotsim,i,filelist,img)
rm(melt,Outputname,plotimg,plotindex)
rm(state,threshhold,unique_city)
rm(sim)


