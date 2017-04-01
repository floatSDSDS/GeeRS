install.packages("colorspace",repos = "http://mirrors.ustc.edu.cn/CRAN/")

library(colorspace)
library(dendextendRcpp)
library(dendextend)
library(pvclust)
library(kernlab)
library(dplyr)
library(data.table)
library(leaflet)
library(ggplot2)


#setwd("floatsdsds/YelpRS thesis/src")

setwd("L:/7sem/YelpRS thesis/src")
list.files()
GeoR<-as.data.frame(fread("GeoR.csv",header=T))
statesInfo<-as.data.frame(fread("statesInfo.csv",header=T))



View(GeoR)
BusinessR<-select(GeoR,business_id,latitude,longitude,state)%>%
  filter(state=="PA")%>%
  unique()%>%
  select(-state)


dbscanResult<-dbscan(BusinessR[,2:3],eps=1)

# split
statesInfo<-arrange(statesInfo,desc(SBR))
stateCode<-statesInfo$state
stateList<-list()
for(i in 1:length(stateCode)){
  stateList[[i]]<-list()
  stateList[[i]]$Info<-statesInfo[i,]
  stateList[[i]]$GeoR<-filter(GeoR,state==stateCode[[i]])
  stateList[[i]]$userlist<-unique(stateList[[i]]$GeoR$user_id)
}
stateList[[1]]$Info
dim(stateList[[1]]$GeoR)
stateList$statesInfo<-statesInfo
stateList$statesCode<-stateCode
rm(stateCode,statesInfo)
rm(i)

CN<-matrix(nrow = 29,ncol = 29)
rownames(CN)<-stateCode
colnames(CN)<-stateCode
View(CN)
for(i in 1:29){
  for(j in 1:29){
    CN[i,j]<-ifelse(i>j,sum(stateList[[i]]$userlist %in% stateList[[j]]$userlist),
                    sum(stateList[[j]]$userlist %in% stateList[[i]]$userlist))
  }
}

write.csv()
ggplot(data = melt(CN)) + 
  geom_tile(aes(x = Var1, y = Var2, fill = log(value+1))) +
  theme(legend.title="log(value)")

library(igraph)
meltCN<-as.data.frame(melt(CN))%>%
  filter(value!=0,Var2!=Var1)
selfdeg<-filter(meltCN,Var1==Var2)
selfdeg<-select(selfdeg,stateCode=Var1,value)
g<-graph_from_data_frame(meltCN, directed = F,vertices = stateCode) 

E(g)$weight<-log(meltCN$value+1)
V(g)$name<-paste(V(g)$name,selfdeg$value,
                 sapply(1:29,function(x)sum(CN[x,])),sep="\n")
V(g)$size<-log(selfdeg$value+1)
V(g)$label.cex<-1.8
V(g)$label.degree<-c(seq(0,-pi,length.out=15),seq(pi,0,length.out = 14))
V(g)$label.dist<-0.5

plot(g,vertex.label.color="orangered",
     layout=layout.circle,
     edge.color="black",
     edge.width=E(g)$weight)


######
# igraph on maps
install.packages("Cairo")
library(Cairo)
geoLayout<-layout.circle(g)
geoLayout<-layout.norm(as.matrix(geoState[,2:3]))
V(g)$label.cex<-2
V(g)$label.degree<-0
V(g)$label.dist<-0

plot.igraph(g,vertex.label.color="orangered",
     layout=geoLayout,
     edge.color="black",
     edge.width=E(g)$weight)



######



##############################################################
# place the network on maps
geoSTate<-select(GeoR,state,longitude,latitude)
geoSTate<-group_by(geoSTate,state)%>%
  mutate(meanlon=mean(longitude),meanlat=mean(latitude))%>%
  select(-latitude,longitude)%>%
  unique()


library(raster)
library(igraph)
map <- getData('GADM', country='GRC', level=1)
df<-data.frame("from" = c("Athens", "Iraklio", "Thessaloniki", "Patra"), "to"= c("Thessaloniki", "Thessaloniki", "Athens", "Iraklio"))
meta <- data.frame("name"=c("Athens", "Iraklio", "Thessaloniki", "Patra"), 
                   "lon"=c(23.72800,25.13356,22.94090,21.73507),  
                   "lat"=c(37.98415,35.33349,40.63229,38.24628))
g <- graph.data.frame(df, directed=T, vertices=meta)
lo <- as.matrix(meta[,2:3])
plot(greece)
plot(g, layout=lo, add = TRUE, rescale = FALSE)

# Hierarchical Clustering

geo<-stateList[[1]]$GeoR
geo<-select(geo,business_id,latitude,longitude)%>%
  unique()
business_id<-geo$business_id
geo<-geo[,-1]
dend <- geo %>% dist %>% hclust(method="average") %>% as.dendrogram()
dend %>% plot(main="dend")



dend@segments

