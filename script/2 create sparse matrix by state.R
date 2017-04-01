# recommendation engiines main


#####
#readin data and create a rating matrix
library(dplyr)
library(data.table)

path<-"L:/7sem/YelpRS thesis/src"
setwd(path)

GeoR<-as.data.frame(fread("GeoR.csv",header=T))
userList<-unique(filter(GeoR,state=="AZ"|state=="NV")$user_id)
prepoolR<-GeoR[GeoR$user_id %in% userList,]
rm(GeoR)
ratepoolR<-select(prepoolR,user_id,business_id,stars)%>%
  group_by(user_id)%>%
  mutate(count_business=n())%>%
  ungroup()%>%
  group_by(user_id,business_id)%>%
  mutate(count_BR=n())%>%
  arrange(desc(count_business),user_id,desc(count_BR))

write.csv(prepoolR,"GeoUserin2State.csv",row.names=F)
ratepoolR$stars<-as.numeric(ratepoolR$stars)
ratepoolR<-mutate(ratepoolR,avgStars=mean(stars))
rm(prepoolR)

state<-as.data.frame(fread("GeoUserin2State.csv",header=T))
state<-select(state,business_id,state)%>%
  unique()
ratepoolR<-left_join(ratepoolR,state)
write.csv(ratepoolR,"UBRate.csv",row.names=F)
statelist<-unique(ratepoolR$state)

userList<-unique(ratepoolR$user_id)
userIndex<-ungroup(ratepoolR)%>%
  select(user_id,countbyB=count_business)%>%
  unique()
write.csv(userIndex,"userIndex.csv")
rm(userList,userIndex)


business<-ungroup(ratepoolR)%>%
  group_by(state)%>%
  mutate(countR=n())%>%
  ungroup()%>%
  group_by(state,business_id)%>%
  mutate(countSB=n())%>%
  arrange(desc(countR),desc(countSB))

names(business)
businessIndex<-select(business,business_id,state,countbyState=countR,countSB)
businessIndex<-unique(businessIndex)
write.csv(businessIndex,"businessIndex.csv")
rm(businessIndex)

#####################
# create a sparse rating Matrix

userIndex<-as.data.frame(fread("userIndex.csv",header = T))
businessIndex<-as.data.frame(fread("businessIndex.csv",header=T))
names(userIndex)[1]<-"userIndex"
names(businessIndex)[1]<-"businessIndex"
write.csv(userIndex,"userIndex.csv",row.names=F)
write.csv(businessIndex,"businessIndex.csv",row.names = F)


PAUBR<-filter(ratepoolR,state=="PA")%>%
  select(user_id,business_id,avgStars)%>%
  left_join(userIndex)%>%
  left_join(businessIndex)
PAUBR<-ungroup(PAUBR)%>%
  select(userIndex,businessIndex,avgStars)
PAUBR$userIndex<-as.numeric(PAUBR$userIndex)
PAUBR$businessIndex<-as.numeric(PAUBR$businessIndex)

#################
# three ways to bulid sparse User-Business Matrix
library(Matrix)
W_PAUBR <- sparseMatrix(i = PAUBR$userIndex,
                         j = PAUBR$businessIndex,
                         x = PAUBR$avgStars)

PAUBR2<-filter(ratepoolR,state=="PA")%>%
  select(user_id,business_id,avgStars)
setDT(PAUBR2)
W_PAUBR2  <- dcast(PAUBR2,user_id~business_id)[-1]

rm(W_PAUBR,W_PAUBR2,PAUBR,PAUBR2)

NVUBR<-filter(ratepoolR,state=="NV")%>%
  select(user_id,business_id,avgStars)
setDT(NVUBR)
W_NVUBR<-dcast(NVUBR,user_id~business_id)
