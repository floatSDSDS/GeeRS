library(dplyr)
library(data.table)
library(fpc)

setwd("src")
GeoR<-as.data.frame(fread("finallyUsedRpool.csv",header=T))
business1<-as.data.frame(fread("business1.csv",header=T))


f_gridCluster<-function(business1,noGrid=10){
  
  latmin<-min(business1$latitude)
  latmax<-max(business1$latitude)
  lonmin<-min(business1$longitude)
  lonmax<-max(business1$longitude)

  gridLon<-seq(lonmin-1,lonmax+1,length.out = noGrid)
  gridLat<-seq(latmin-1,latmax+1,length.out = noGrid)

  
  
  temp<-group_by(business1,latLabel,lonLabel)%>%
    mutate(count=n())%>%
    select(latLabel,lonLabel,count)%>%
    unique()

    business1$latLabel<-1
  business1$lonLabel<-1
  
  for(i in 2:noGrid){
    business1$latLabel[which(business1$latitude>gridLat[i-1]&business1$latitude<gridLat[i])]<-i
    business1$lonLabel[which(business1$longitude>gridLon[i-1]&business1$longitude<gridLon[i])]<-i
  }
  
  temp<-arrange(temp,lonLabel,latLabel)
  sparse<-sparseMatrix(i = temp$latLabel,j=temp$lonLabel,x = temp$count)
  temp$diffLat<-abs(f_diffVect(temp$latLabel))
  temp$diffLon<-abs(f_diffVect(temp$lonLabel))
  
  
}

f_diffVect<-function(vector){
  len<-length(vector)
  vector1<-c(vector,vector[len])
  vector2<-c(vector[1],vector)
  diff<-vector2-vector1
  return(diff[1:len])
}

temp$diffLon<-NULL
temp$diffLat<-NULL

 
# 9 x 9 sparse Matrix of class "dgCMatrix"
# 
# [1,] . .     .   . . .    .     .   .
# [2,] . .     .   . . .    .     .   .
# [3,] . .     .   . . .   22   367  63
# [4,] . .     .   . . . 2472 31684 347
# [5,] . .     .   . . .  212  1329   9
# [6,] . .     .   . . .    .     .   .
# [7,] . .     .   . . .    .     .   .
# [8,] . . 13290 648 . .    .     .   .
# [9,] . .  9650  10 . .    .     .   .

sparse2<-ifelse(sparse==0,0,1)
Matrix(sparse2,9,9)
temp$flag<-c(1,1,1,1,2,2,2,2,2,2,2,2,2)
business1<-left_join(business1,temp)
business1<-select(business1,-latLabel,-lonLabel)
business1<-select(business1,-count)
names(business1)
write.csv(business1,"business1.csv",row.names = F)
