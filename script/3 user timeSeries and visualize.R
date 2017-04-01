
library(data.table)
library(dplyr)
library(ggplot2)
library(leaflet)
library(recommenderlab)
library(tcltk)
library(igraph)

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
pool.review<-pool.review[!is.na(pool.review$label),]
pool.review<-dplyr::group_by(pool.review,user_id)%>%
  dplyr::mutate(countUR=n())
pool.review<-arrange(pool.review,countUR,user_id)

summary(pool.review)

review.userUP55R<-dplyr::filter(pool.review,countUR>55)
# review.userLESS55R<-dplyr::filter(pool.review,countUR<=55)
# rm(review.userLESS55R,review.userUP55R)

userlist<-unique(review.userUP55R$user_id)
review.byU<-mapply(function(user){return(filter(pool.review,user_id==user))},userlist)


# cl <- makeCluster(4)
# results<-parSapply(cl, userlist, function(user,y)return(dplyr::filter(y,user_id==user)),pool.review)
# clusterExport(cl, "y")
# parSapply(cl, userlist, function(user,y)return(filter(y,user_id==user)))
# res.df <- do.call('rbind',results)
# stopCluster(cl)

user.sample<-sample_n(as.data.frame(userlist,stringsAsFactors = F),10)
names(user.sample)<-"user_id"
user.sample<-left_join(user.sample,pool.review)

m <- leaflet() %>% addTiles()
mapshot(m, file = "~/Rplot.png")