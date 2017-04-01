install.packages("dplyr")
install.package("data.table")

library(dplyr)
library(data.table)

path<-"L:/7sem/YELP/MLR/data"
setwd("L:/7sem/YELP/MLR/data")

RwithGender<-as.data.frame(fread("RwithGender40W.csv",header=T))

RwithG<-filter(RwithGender,probability>0.96)%>%
  filter(count>10)
summary(select(RwithG,probability,count))