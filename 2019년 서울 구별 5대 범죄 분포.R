#필요 라이브러리 설치
library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(plyr)
library(reshape)
library(dplyr)

#데이터 불러오기
crime_situation<-read.csv(file = "C://LJH/Suwon_Security/data/2016~2019_서울시_5대범죄_구별_발생현황.csv",header=T)

#데이터 전처리
crime_2019<-crime_situation[crime_situation$기간==2019,]

#데이터 전처리 ----(1) 지역코드 만들기
id<-c(11110,11140,11170,11200,11215,11230,
      11260,11290,11305,11320,11350,11380,11410,
      11440,11470,11500,11530,11545,11560,11590,
      11620,11650,11680,11710,11740)


#데이터 전처리 ----(2) 살인발생 데이터 만들기
m_df <- subset(crime_2019,
                 select = c("자치구","살인발생"))
m_df <- cbind(m_df,id)
names(m_df)<-c("자치구","murder","id")



m_percent<- m_df$murder/sum(m_df$murder)*100
m_percent<-round(m_percent,2)
m_df <- cbind(m_df,m_percent)

#데이터 전처리 ----(3) 강도발생 데이터 만들기
r_df <- subset(crime_2019,
                  select=c("자치구","강도발생"))
r_df <- cbind(r_df,id)
names(r_df) <- c("자치구","robbery","id")

r_perc <- r_df$robbery/sum(r_df$robbery)*100
r_perc <- round(r_perc,2)
r_df <- cbind(r_df,r_perc)


#데이터 전처리 ----(4) 강간발생 데이터 만들기
rape_df <- subset(crime_2019,
               select=c("자치구","강간강제추행발생"))
rape_df <- cbind(rape_df,id)
names(rape_df)<-c('자치구',"rape","id")

#데이터 전처리 -----(5) 폭력발생 데이터 만들기
t_df <- subset(crime_2019,
                select=c("자치구","절도발생"))
t_df <- cbind(t_df,id)
names(t_df) <- c("자치구","thief","id")

#데이터 전처리 ----(6) 폭력발생 데이터 만들기

v_df <- subset(crime_2019,
                   select=c("자치구","폭력발생"))
v_df<- cbind(v_df,id)
names(v_df)<-c("자치구","violence","id")

#지도 불러내기
map <- shapefile("C://LJH/Suwon_Security/SIG_201703/TL_SCCO_SIG.shp")
map <- spTransform(map, CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
new_map<-fortify(map, region = 'SIG_CD')
new_map$id <-as.numeric(new_map$id)
seoul_map<-new_map[new_map$id<=11740,]

m_merge<-merge(seoul_map,m_df,by='id')

# 구별 코드 만드기
name<-c("종로구","중구","용산구","성동구","광진구",
        "동대문구","중랑구","성북구","강북구","도봉구",
        "노원구","은평구","서대문구","마포구","양천구",
        "강서구","구로구","금천구","영등포구","동작구",
        "관악구","서초구","강남구","송파구","강동구")
long <-c(126.978061,126.995227,126.979489,127.041285,127.085592,
         127.053258,127.093171,127.017674,127.010241,127.033412,
         127.075085,126.925604,126.937978,126.907785,126.854823,
         126.821907,126.856060,126.899865,126.909517,126.951095,
         126.944660,127.030290,127.063453,127.115178,127.147103)
lat  <-c(37.595678,37.559762,37.530907,37.550784,37.545238,
         37.581976,37.597683,37.606227,37.643523,37.668783,
         37.652035,37.619306,37.577736,37.559885,37.524367,
         37.561258,37.494527,37.460746,37.522797,37.499043,
         37.467817,37.473710,37.496490,37.504737,37.550271)

# 살인 발생 지도 만들기
perc <-m_df$m_percent
gu_name<-data.frame(name,perc,long,lat)


m_plot<- ggplot()+
  geom_polygon(data=m_merge,
               aes(x=long,y=lat,group=group,fill=m_percent),color="white")+
  scale_fill_gradient(low="#ECD6AF",high = "#FF6900",space="lab",guide="colourbar")+
  labs(fill="서울시 살인발생 비율 분포")+
  theme_void()+
  theme(legend.position = c(.15, .85))+
  geom_text(data=gu_name,
            aes(x=long, y=lat,label=paste(name,perc,sep="\n")))
  

#강도 발생 서울 지도 만들기
r_merge <- merge(seoul_map,r_df,by="id")
perc2<-r_df$r_perc
gu_name_2<-data.frame(name,perc2,long,lat)

r_plot <- ggplot()+
  geom_polygon(data=r_merge,aes(x=long,y=lat,group=group,fill=r_perc),color='white')+
  scale_fill_gradient(low="#ECD6AF",high = "#FF6900",space="lab",guide="colourbar")+
  labs(fill="서울시 강도발생 비율 분포")+
  theme_void()+
  theme(legend.position = c(.15, .85))+
  geom_text(data=gu_name_2,
            aes(x=long, y= lat, label =paste(name,perc2,sep="\n")))

# 강간 발생 서울 지도 만들기

rape_merge <- merge(rape_df,seoul_map,by='id')
rape_freq <- rape_df$rape
gu_name_3 <- data.frame(name,rape_freq,long,lat)

rape_plot <- ggplot()+
  geom_polygon(data=rape_merge,aes(x=long,y=lat,group=group,fill=rape),color='white')+
  scale_fill_gradient(low="#ECD6AF",high = "#FF6900",space="lab",guide="colourbar")+
  labs(fill = "서울시 강간강제추행 발생 분포")+
  theme_void()+
  theme(legend.position = c(.15, .85))+
  geom_text(data=gu_name_3,
            aes(x=long,y=lat,label=paste(name,rape_freq,sep='\n')))

# 절도 발생 서울 지도 만들기

t_merge <- merge(t_df,seoul_map,by="id")
t_freq <- t_df$theif
gu_name_4 <- data.frame(name,t_freq,long,lat)

t_plot <- ggplot()+
  geom_polygon(data = t_merge,
               aes(x=long,y=lat,group=group,fill=thief),color='white') +
  scale_fill_gradient(low="#ECD6AF",high = "#FF6900",space="lab",guide="colourbar")+
  labs(fill = "서울시 절도 발생 분포")+
  theme_void()+
  theme(legend.position = c(.15, .85))+
  geom_text(data=gu_name_4,
            aes(x=long, y=lat, label = paste(name,t_freq,sep='\n')))




# 폭력 지도 만들기

V_merge <- merge(v_df,seoul_map,by="id")
v_freq <- v_df$violence
gu_name_5 <- data.frame(name,v_freq,long,lat)

v_plot <- ggplot()+
  geom_polygon(data=V_merge,
               aes(x=long, y= lat, group=group, fill=violence),color="white") +
  scale_fill_gradient(low="#ECD6AF",high = "#FF6900",space="lab",guide="colourbar") +
  labs(fill = "서울시 폭력 발생 분포")+
  theme_void()+
  theme(legend.position = c(.15, .85))+
  geom_text(data = gu_name_5,
            aes(x=long, y= lat , label = paste(name,v_freq,sep='\n')))
  

# 도표 합치기
library(gridExtra)

grid.arrange(m_plot,r_plot,rape_plot,t_plot,v_plot,nrow=2,ncol=3)

