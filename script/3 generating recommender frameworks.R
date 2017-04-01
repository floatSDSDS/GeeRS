library(data.table)
library(dplyr)


Item<-filter(business,label1==1,label2==1)%>%
  select(business_id,stars)
R1<-GeoR1[GeoR1$label1==1&GeoR1$label2==1,]
R2<-GeoR2[GeoR2$label1==1&GeoR2$label2==1,]
UBR<-rbind(as.data.frame(R1),as.data.frame(R2),stringAsFactors=F)
UBR$stars<-as.numeric(UBR$stars)
rm(R1,R2,GeoR1,GeoR2,business)
UBR<-UBR[-which(is.na(UBR$label1)),]
UBR<-UBR[-which(is.na(UBR$stars)),]
summary(UBR)

# create index for user from R and business from business pool
userIndex<-data.frame(user_id=unique(UBR$user_id),stringsAsFactors=F)
userIndex$userIndex<-1:dim(userIndex)[1]
businessIndex<-data.frame(business_id=unique(Item$business_id),stringAsFactors=F)
businessIndex$businessIndex<-1:dim(businessIndex)[1]


UBR<-select(UBR,user_id,business_id,stars)%>%
  left_join(userIndex)%>%
  left_join(businessIndex)%>%
  ungroup()
UBR$user_id<-NULL
UBR$business_id<-NULL
UBR$stringAsFactors<-NULL

UBR<-select(UBR,userIndex,businessIndex,stars)%>%
  group_by(userIndex,businessIndex)%>%
  mutate(avgStars=mean(stars))%>%
  select(-stars)
setwd("E:/8sem/thesis/src/2 GeoOnlyAZNV")
write.csv(userIndex,"userIndex1.csv",row.names = F)
write.csv(businessIndex,"businessIndex1.csv",row.names=F)
write.csv(UBR,"UBR1.csv",row.names=F)

rm(R1,R2)
summary(UBR)
UBR<-UBR[-which(is.na(UBR$userIndex)),]

W.UBR <- sparseMatrix(i = UBR$userIndex,
                        j = UBR$businessIndex,
                        x = UBR$avgStars)
rm(UI.Matrxi,userlistSample,userSampleArrange,
   IBCF,Item.Item.Corr,LRMF,UBCF,User.Item.Corr,User.Item.Corr.Num.NAs,User.Item.Matrix)

