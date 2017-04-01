# ItemCF Recall
res[[2]][which(res[[1]]!=0)]<-1
length(res[[2]]!=0)

f_Recall<-function(user.ith){
  user_id<-userIndex[user.ith,1]
  
  return(Recall)
}

rm(GeoR1,GeoR2)
View(R1[R1$user_id==user_id,])

f_BIndex<-function(user.ith){
  user.BVector<-res[[1]][user.ith,]
  user.BIndex<-(1:dim(res[[1]])[2])[which(user.BVector!=0)]
  return(user.BIndex)
}

f_RVector<-function(user.ith){
  user.RVector<-user.RVector[user.BIndex]
}

