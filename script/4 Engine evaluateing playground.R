# installed required packages(the ones only with Rating Matrix)

# load packages
library(data.table)
library(dplyr)
library(ggplot2)
library(leaflet)
library(recommenderlab)
library(tcltk)

# import data
path=("E:/8sem/thesis/src/2 GeoOnlyAZNV")
setwd(path)
list.files()
Info.region<-as.data.frame(fread("regionInfo.csv",header=T))

pool.business<-as.data.frame(fread("businessPool.csv",header=T))%>%
  left_join(cbind(Info.region[,1:2],label=1:10))%>%
  select(-label1,-label2)

pool.review<-as.data.frame(fread("RPool.csv"))%>%
  left_join(cbind(Info.region[,1:2],label=1:10))%>%
  select(-label1,-label2,-state,-latLabel,-lonLabel)
pool.review$date<-as.Date(pool.review$date)
pool.review$stars<-as.numeric(pool.review$stars)

folderName<-"1 Rating Matrix Only"
if(!dir.exists(folderName))
  dir.create(folderName)
setwd(folderName)

# Divide data by Region Label
## Geographical filter
Regionlist<-1:10


for(region.ith in Regionlist){
  folderName=paste("Region", Regionlist[region.ith] , sep = "")
  if(!dir.exists(folderName))
    dir.create(folderName)
}



# create Rating Matrix
# generate real Rating list
# no good for time aware methods
summary(pool.review)
pool.review<-filter(pool.review,!is.na(label))
pool.review<-group_by(pool.review,user_id,business_id)%>%
  mutate(avgstar=mean(stars))
pool.review<-ungroup(pool.review)
UB.rate<-select(pool.review,user_id,business_id,avgstar,label)%>%
  unique()



UB.set<-list()
pb <- tkProgressBar("progress bar", "",
                    0, 100)

for(region.ith in 1:length(Regionlist)){
  
  UB.set[[region.ith]]<-list()
  
  ## split UB.rate by region label
  UB.set[[region.ith]]$UBrate<-filter(UB.rate,label==Regionlist[region.ith])%>%
    select(-label)
  UB.set[[region.ith]]$regionInfo<-data.frame(Info.region[Regionlist[region.ith],])
  
  ## create Index for users and business
  user_id<-unique(UB.set[[region.ith]]$UBrate$user_id)
  UB.set[[region.ith]]$userIndex<-data.frame(user_id,stringsAsFactors = F)
  UB.set[[region.ith]]$userIndex$userIndex<-1:length(user_id)
  
  business_id<-unique(filter(pool.business,label==Regionlist[region.ith])$business_id)
  UB.set[[region.ith]]$businessIndex<-data.frame(business_id,stringsAsFactors = F)
  UB.set[[region.ith]]$businessIndex$businessIndex<-1:length(business_id)
  
  ## join Index into rating Info
  UB.set[[region.ith]]$UBrate<-left_join(UB.set[[region.ith]]$UBrate,UB.set[[region.ith]]$userIndex)%>%
    left_join(UB.set[[region.ith]]$businessIndex)%>%
    select(userIndex,businessIndex,avgstar)
  
  ## create Sparse Matrix
  UB.set[[region.ith]]$RatingM<-sparseMatrix(i=UB.set[[region.ith]]$UBrate$userIndex,
                                             j=UB.set[[region.ith]]$UBrate$userIndex,
                                             x=UB.set[[region.ith]]$UBrate$avgstar)
  setTkProgressBar(pb, region.ith, label=paste( round(region.ith/(length(Regionlist))*100, 0),"% done"))

}
close(pb)
rm(pb,user_id,business_id,region.ith)


# pass Rating Matrxi into Engine




# test output by users


# evaluate by RMSE



# evaluate by Recall
rm(region.ith,folderName,Regionlist)

