
library(shiny)
library(leaflet)
library(RColorBrewer)
library(data.table)
library(dplyr)


sampleR<-as.data.frame(fread("data/sampleR170401.csv",header=T))
user_list<-as.data.frame(unique(sampleR$user_id),stringsAsFactors = F)
names(user_list)<-"user_id"
user_list<-left_join(user_list,unique(sampleR[,1:5]))

name_id<-user_list$user_id
names(name_id)<-user_list$name.x
