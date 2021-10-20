#필요 라이브러리 설치
library(dplyr)
library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(plyr)
library(reshape)
library(gridExtra)

#데이터 불러오기
df <- read.csv("C://LJH/Suwon_Security/data/(완)4개시도_1인가구수.csv",header=T,encoding="ANSI")

#2019년 여성 1인 가구 선택
single2019 <- subset(df,
                     select=c(names(df)),
                     subset =(s0=="여자" & age=="합계" & date==2019))



#법정동 id 만들기

id<-c(11110,11140,11170,11200,
      11230,11260,11290,11305,11320,
      11350,11380,11410,11440,11470,
      11500,11530,11545,11560,11590,
      11620,11650,11680,11710,11740,
      41111,41113,41115,41117,
      48170,
      48121,48123,48125,48127,48129,11215)
single2019 <- cbind(single2019,id)

v_name <- names(single2019)

#서울시 1인 여성가구 선택

single_seoul <- subset(single2019,
                       select=v_name,
                       subset=(sido=="서울특별시"))

#수원시 1인여성 가수 선택

single_suwon <- subset(single2019,
                       select=v_name,
                       subset=(sido=="경기도"))

#진주시 1인 여성가구 선택
single_jinju <-subset(single2019,
                      select= v_name,
                      subset=(sgg_nm=="진주시"))

#창원시 1인 여성가구 선택
single_changwon<- subset(single2019,
                         select=v_name,
                         subset=(sgg_nm=="창원시의창구"|sgg_nm=="창원시성산구"|sgg_nm=="창원시마산합포구"|
                                   sgg_nm=="창원시마산회원구"|sgg_nm=="창원시진해구"|sgg_nm=="창원시진해구"))


#서울시, 수원시, 창원시, 진주시 구이름 만들기

name<-c("종로구","중구","용산구","성동구",
        "동대문구","중랑구","성북구","강북구","도봉구",
        "노원구","은평구","서대문구","마포구","양천구",
        "강서구","구로구","금천구","영등포구","동작구",
        "관악구","서초구","강남구","송파구","강동구",
        "장안구","권선구","팔달구","영통구",
        "진주시",
        "의창구","성산구","마산합포구","마산회원구","마산진해구","광진구")
long <-c(126.978061,126.995227,126.979489,127.041285,
         127.053258,127.093171,127.017674,127.010241,127.033412,
         127.075085,126.925604,126.937978,126.907785,126.854823,
         126.821907,126.856060,126.899865,126.909517,126.951095,
         126.944660,127.030290,127.063453,127.115178,127.147103,
         127.003202,126.979601,127.016678,127.056372,
         128.129327,
         128.650022,128.672589,128.511896,128.536726,128.741783,127.085592)
lat  <-c(37.595678,37.559762,37.530907,37.550784,
         37.581976,37.597683,37.606227,37.643523,37.668783,
         37.652035,37.619306,37.577736,37.559885,37.524367,
         37.561258,37.494527,37.460746,37.522797,37.499043,
         37.467817,37.473710,37.496490,37.504737,37.550271,
         37.313811,37.260261,37.277563,37.274533,
         35.205175,
         35.309506,35.194890,35.116795,35.232756,35.131137,37.545238)


s_all <- single2019$single_person_hh
gu_name <- data.frame(name,long,lat,s_all)

gu_name_seoul <- gu_name[c(1:24),]
gu_name_seoul <- rbind(gu_name_seoul,gu_name[c(35),])

gu_name_suwon <- gu_name[c(25:28),]

gu_name_jinju <- gu_name[c(29),]

gu_name_changwon <- gu_name[c(30:34),]


#기본 지도
map <- shapefile("C://LJH/Suwon_Security/SIG_201703/TL_SCCO_SIG.shp")
map <- spTransform(map, CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
new_map<-fortify(map, region = 'SIG_CD')
new_map$id <-as.numeric(new_map$id)

#서울 지도 만들기
seoul_map<-new_map[new_map$id<=11740,]
seoul_merge<-merge(seoul_map,single_seoul,by='id')



# 서울시 1인 여성 가구 분포 시각화
seoul_plot<-ggplot()+
  geom_polygon(data=seoul_merge,
               aes(x=long,y=lat,group=group,fill=single_person_hh),color="white")+
  scale_fill_gradient(low="#FFDCFF",high = "#FF1493",space="lab",guide="colourbar")+
  labs(fill="서울시 1인 여성 가구 분포(단위:명)")+
  theme_void()+
  theme(legend.position = c(.15, .85))+
  geom_text(data=gu_name_seoul,
            aes(x=long, y=lat,label=paste(name,s_all,sep="\n")))

# 수원시 지도 만들기
suwon_map<-new_map[new_map$id>=41111 & new_map$id<=41117,]
suwon_merge <- merge(suwon_map,single_suwon,by='id')

# 수원시 1인 여성 가구 분포 시각화
suwon_plot<- ggplot()+
  geom_polygon(data=suwon_merge,
               aes(x=long,y=lat,group=group,fill=single_person_hh),color="white")+
  scale_fill_gradient(low="#FFDCFF",high = "#FF1493",space="lab",guide="colourbar")+
  labs(fill="수원시 1인 여성 가구 분포(단위:명)")+
  theme_void()+
  theme(legend.position = c(.15, .85))+
  geom_text(data=gu_name_suwon,
            aes(x=long,y=lat,label=paste(name,s_all,sep="\n")))

# 진주시 지도 만들기
jinju_map <- new_map[new_map$id==48170,]
jinju_merge <- merge(jinju_map,single_jinju,by="id")

# 진주시 1인 여성 가구 분포 시각화

jinju_plot<- ggplot()+
  geom_polygon(data = jinju_merge,
               aes(x=long,y=lat,group=group,fill=single_person_hh),colour="white")+
  scale_fill_gradient(low="#FFDCFF",high = "#FF1493",space="lab",guide="colourbar")+
  labs(fill="진주시 여성1인 가구 분포(단위:명)")+
  theme_void()+
  theme(legend.position = c(.15, .85))+
  geom_text(data=gu_name_jinju,
            aes(x=long,y=lat,label=paste(name,s_all,sep="\n")))

# 창원시 지도 만들기
changwon_map <- new_map[new_map$id>=48121 & new_map$id <= 48129,]
changwon_merge <- merge(changwon_map,single_changwon,by="id")

# 창원시 1인 여성 가구 분포 시각화
changwon_plot <- ggplot()+
  geom_polygon(data=changwon_merge,
               aes(x=long, y=lat,group=group,fill=single_person_hh),color="white")+
  scale_fill_gradient(low="#FFDCFF",high = "#FF1493",space="lab",guide="colourbar")+
  labs(fill="창원시 여성1인 가구 분포(단위:명)")+
  theme_void()+
  theme(legend.position = c(.15, .85))+
  geom_text(data=gu_name_changwon,
            aes(x=long,y=lat,label=paste(name,s_all,sep="\n")))

# 지도 병렬

grid.arrange(seoul_plot,suwon_plot,changwon_plot,jinju_plot,nrow=2,ncol=2)


