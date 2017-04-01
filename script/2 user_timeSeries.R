
GeoR<-as.data.frame(fread("finallyUsedRpool.csv",header=T))
names(GeoR)

GeoR[,12]<-NULL
temp<-select(business,business_id,label1,label2)
GeoR<-left_join(GeoR,temp)
write.csv(GeoR,"finallyUsedRpool.csv",row.names = F)

names(GeoR)
GeoR$count<-NULL
GeoR$latLabel<-NULL
GeoR$lonLabel<-NULL
GeoR$state<-NULL

rm(temp)

GeoR2<-group_by(GeoR,user_id)%>%
  mutate(count=n())%>%
  arrange(desc(count))

stattemp<-select(GeoR2,user_id,count)%>%
  unique()
stattemp<-ungroup(stattemp)%>%
  group_by(count)%>%
  mutate(count2=n())%>%
  select(-user_id)%>%
  unique()

summary(stattemp)

library(ggplot2)

ggplot(data=stattemp,aes(count,log(count2)))+
  geom_line(size=1)+
  geom_point()+
  labs(title="pearson")+
  theme(legend.position=c(.8,.15),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        panel.background = element_rect(fill = "seashell"))

#170327
GeoR<-as.data.frame(fread("finallyUsedRpool.csv",header=T))
business<-as.data.frame(fread("businessPool.csv",header=T))
regionInfo<-as.data.frame(fread("regionInfo.csv",header=T))

View(regionInfo)

summary(GeoR2)
GeoR1<-filter(GeoR2,count<=10) # divide by median
GeoR2<-filter(GeoR2,count>10)

userlist2<-unique(GeoR2$user_id)
usersample<-sample_n(as.data.frame(userlist2),100)
names(usersample)="user_id"
usersample<-left_join(usersample,GeoR2)
View(usersample)

usersample$date<-as.Date(usersample$date,"%Y-%m-%d")
usersampleArrange<-arrange(usersample,desc(count),user_id,date)%>%
  group_by(user_id)
rm(usersample)
userlistSample<-unique(usersampleArrange$user_id)

switchUserlist<-select(rbind(GeoR1,GeoR2),user_id,label1,label2)%>%
  unique()
switchUserlist<-group_by(switchUserlist,user_id)%>%
  mutate(userswitchCount=n())%>%
  filter(n()>1)
switchUserlist<-arrange(switchUserlist,userswitchCount,user_id)
write.csv(switchUserlist,"userWhoSwitched.csv",row.names = F)

userswitchList<-unique(switchUserlist$user_id)
GeoRSwitch1<-GeoR1[GeoR1$user_id%in%userswitchList,]
GeoRSwitch2<-GeoR2[GeoR2$user_id%in%userswitchList,]

# there are 29958 users has been switched from one region to the other
# in GeoR1(where users only left reviews for not more than 10, there are 93768 reviews remain), with 421996 in GeoR2
# summary(GeoR1)  #length:1184381
# business_id          user_id           review_id            stars               date              latitude       longitude       
# Length:1184381     Length:1184381     Length:1184381     Length:1184381     Length:1184381     Min.   :32.87   Min.   :-115.386  
# Class :character   Class :character   Class :character   Class :character   Class :character   1st Qu.:33.50   1st Qu.:-115.174  
# Mode  :character   Mode  :character   Mode  :character   Mode  :character   Mode  :character   Median :36.04   Median :-115.101  
# Mean   :35.02   Mean   :-113.191  
# 3rd Qu.:36.12   3rd Qu.:-111.977  
# Max.   :55.99   Max.   :   8.495  
# label1          label2          count       
# Min.   :1.000   Min.   :1.000   Min.   : 1.000  
# 1st Qu.:1.000   1st Qu.:1.000   1st Qu.: 2.000  
# Median :1.000   Median :1.000   Median : 3.000  
# Mean   :1.016   Mean   :1.469   Mean   : 3.989  
# 3rd Qu.:1.000   3rd Qu.:2.000   3rd Qu.: 6.000  
# Max.   :3.000   Max.   :6.000   Max.   :10.000  
# summary(GeoRSwitch1)  #length:93768
# business_id          user_id           review_id            stars               date              latitude       longitude       
# Length:93768       Length:93768       Length:93768       Length:93768       Length:93768       Min.   :32.87   Min.   :-115.364  
# Class :character   Class :character   Class :character   Class :character   Class :character   1st Qu.:33.51   1st Qu.:-115.170  
# Mode  :character   Mode  :character   Mode  :character   Mode  :character   Mode  :character   Median :36.08   Median :-112.074  
# Mean   :36.00   Mean   :-106.756  
# 3rd Qu.:36.13   3rd Qu.:-111.877  
# Max.   :55.99   Max.   :   8.495  
# label1          label2          count       
# Min.   :1.000   Min.   :1.000   Min.   : 2.000  
# 1st Qu.:1.000   1st Qu.:1.000   1st Qu.: 4.000  
# Median :1.000   Median :2.000   Median : 6.000  
# Mean   :1.198   Mean   :1.702   Mean   : 5.809  
# 3rd Qu.:1.000   3rd Qu.:2.000   3rd Qu.: 8.000  
# Max.   :3.000   Max.   :6.000   Max.   :10.000  
# summary(GeoR2)  #length:1183646
# business_id          user_id           review_id            stars               date              latitude       longitude       
# Length:1183646     Length:1183646     Length:1183646     Length:1183646     Length:1183646     Min.   :32.87   Min.   :-115.386  
# Class :character   Class :character   Class :character   Class :character   Class :character   1st Qu.:33.48   1st Qu.:-115.171  
# Mode  :character   Mode  :character   Mode  :character   Mode  :character   Mode  :character   Median :33.70   Median :-112.113  
# Mean   :34.98   Mean   :-111.594  
# 3rd Qu.:36.12   3rd Qu.:-111.927  
# Max.   :55.99   Max.   :   8.529  
# 
# label1          label2          count       
# Min.   :1.000   Min.   :1.000   Min.   :  11.0  
# 1st Qu.:1.000   1st Qu.:1.000   1st Qu.:  19.0  
# Median :1.000   Median :2.000   Median :  39.0  
# Mean   :1.054   Mean   :1.582   Mean   : 106.2  
# 3rd Qu.:1.000   3rd Qu.:2.000   3rd Qu.: 108.0  
# Max.   :3.000   Max.   :6.000   Max.   :1564.0  
# NA's   :1       NA's   :1                       
# summary(GeoRSwitch2) #length:421996
# business_id          user_id           review_id            stars               date              latitude       longitude       
# Length:421996      Length:421996      Length:421996      Length:421996      Length:421996      Min.   :32.87   Min.   :-115.370  
# Class :character   Class :character   Class :character   Class :character   Class :character   1st Qu.:33.47   1st Qu.:-115.145  
# Mode  :character   Mode  :character   Mode  :character   Mode  :character   Mode  :character   Median :33.64   Median :-112.029  
# Mean   :35.30   Mean   :-108.006  
# 3rd Qu.:36.12   3rd Qu.:-111.888  
# Max.   :55.99   Max.   :   8.529  
# 
# label1          label2         count       
# Min.   :1.000   Min.   :1.00   Min.   :  11.0  
# 1st Qu.:1.000   1st Qu.:1.00   1st Qu.:  30.0  
# Median :1.000   Median :2.00   Median :  81.0  
# Mean   :1.152   Mean   :1.73   Mean   : 179.1  
# 3rd Qu.:1.000   3rd Qu.:2.00   3rd Qu.: 219.0  
# Max.   :3.000   Max.   :6.00   Max.   :1564.0  
# NA's   :1       NA's   :1           

length(unique(GeoR1$user_id)) #522302
length(unique(GeoRSwitch1$user_id)) #20516
length(unique(GeoR2$user_id)) #38903
length(unique(GeoRSwitch2$user_id)) #9442

GeoRSwitch<-rbind(GeoRSwitch1,GeoRSwitch2)
rm(GeoRSwitch1,GeoRSwitch2)

label<-select(regionInfo,label1,label2)
label$label<-1:10
GeoRSwitch<-left_join(GeoRSwitch,label)

temp<-select(GeoRSwitch,user_id,label)%>%
  unique()%>%
  group_by(user_id)%>%
  mutate(switchtime=n())
GeoRSwitch<-left_join(GeoRSwitch,temp)%>%
  arrange(switchtime,count,user_id,label)

write.csv(GeoRSwitch,"GeoRSwitch.csv",row.names=F)

regionInfo$label<-1:10
rm(label)

switchUserlist<-select(GeoRSwitch,user_id,label1,label2,label,switchtime,count)%>%
  unique()
write.csv(switchUserlist,"userWhoSwitched.csv",row.names=F)
rm(temp)

