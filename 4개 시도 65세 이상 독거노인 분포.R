#필요 설치 라이브러리
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


df <- read.csv("C://LJH/Suwon_Security/data/(완)4개시도_1인가구수.csv",header=T,encoding="ANSI")

#age 범주형 변수 코딩 변경

df$age[df$age == "20~24"] <- "20대"
df$age[df$age == "25~29"] <- "20대"
df$age[df$age == "30~34"] <- "30대"
df$age[df$age == "35~39"] <- "30대"
df$age[df$age == "40~44"] <- "40대"
df$age[df$age == "45~49"] <- "40대"
df$age[df$age == "50~54"] <- "50대"
df$age[df$age == "55~59"] <- "50대"
df$age[df$age == "60~64"] <- "60대"
df$age[df$age == "65~69"] <- "65세 이상"
df$age[df$age == "70~74"] <- "65세 이상"
df$age[df$age == "75~79"] <- "65세 이상"
df$age[df$age == "80~84"] <- "65세 이상"
df$age[df$age == "85세 이상"] <- "65세 이상"

#2019년 65세이상 선별 & 구 단위로 합계 계산
df_over65_2019 <- subset(df,
                     select=c(names(df)),
                     subset =(s0=="계" & age=="65세 이상" & date==2019))

df_over65_2019 <- aggregate(single_person_hh ~ sgg_nm,df_over65_2019,sum)


#ID데이터 프레임 생성

id<-c(11110,11140,11170,11200,
      11230,11260,11290,11305,11320,
      11350,11380,11410,11440,11470,
      11500,11530,11545,11560,11590,
      11620,11650,11680,11710,11740,
      41111,41113,41115,41117,
      48170,
      48121,48123,48125,48127,48129,11215)

sgg_nm<-c("종로구","중구","용산구","성동구",
        "동대문구","중랑구","성북구","강북구","도봉구",
        "노원구","은평구","서대문구","마포구","양천구",
        "강서구","구로구","금천구","영등포구","동작구",
        "관악구","서초구","강남구","송파구","강동구",
        "수원시장안구","수원시권선구","수원시팔달구","수원시영통구",
        "진주시",
        "창원시의창구","창원시성산구","창원시마산합포구","창원시마산회원구","창원시진해구","광진구")

#4개 시도 시각화 레이블 좌표계
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


id <- data.frame(sgg_nm,id,long,lat)

df_over65_2019 <- merge(df_over65_2019,id,by="sgg_nm")

#4개 시도별 데이터프레임 생성
seoul_over_2019 <- df_over65_2019[c(1:18,23:29),c(1,2,3)]
suwon_over_2019 <- df_over65_2019[c(19:22),c(1,2,3)]
jinju_over_2019 <- df_over65_2019[c(30),c(1,2,3)]
changwon_over_2019 <- df_over65_2019[c(31:35),c(1,2,3)]

#지도 시각화 레이블 데이터 셋 만들기
gu_name_seoul<- df_over65_2019[c(1:18,23:29),c(1,2,4,5)]

gu_name_suwon<- df_over65_2019[c(19:22),c(1,2,4,5)]
gu_name_suwon$sgg_nm[gu_name_suwon$sgg_nm=="수원시권선구"] <-"권선구" 
gu_name_suwon$sgg_nm[gu_name_suwon$sgg_nm=="수원시장안구"] <-"권선구" 
gu_name_suwon$sgg_nm[gu_name_suwon$sgg_nm=="수원시팔달구"] <-"권선구" 
gu_name_suwon$sgg_nm[gu_name_suwon$sgg_nm=="수원시영통구"] <-"영통구" 

gu_name_jinju<- df_over65_2019[c(30),c(1,2,4,5)]

gu_name_changwon<- df_over65_2019[c(31:35),c(1,2,4,5)]
gu_name_changwon$sgg_nm[gu_name_changwon$sgg_nm=="창원시의창구구"] <- "의창구"
gu_name_changwon$sgg_nm[gu_name_changwon$sgg_nm=="창원시의성산구"] <- "성산구"
gu_name_changwon$sgg_nm[gu_name_changwon$sgg_nm=="창원시마산합포구"] <- "마산합포구"
gu_name_changwon$sgg_nm[gu_name_changwon$sgg_nm=="창원시마산회원구"] <- "마산회원구"
gu_name_changwon$sgg_nm[gu_name_changwon$sgg_nm=="창원시진해구"] <- "마산진해구"

#기본 지도
map <- shapefile("C://LJH/Suwon_Security/SIG_201703/TL_SCCO_SIG.shp")
map <- spTransform(map, CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
new_map<-fortify(map, region = 'SIG_CD')
new_map$id <-as.numeric(new_map$id)

#서울 지도 만들기
seoul_map<-new_map[new_map$id<=11740,]
seoul_merge<-merge(seoul_map,seoul_over_2019,by='id')



# 서울시 65세이상 독거노인 가구 분포 시각화
seoul_plot<-ggplot()+
  geom_polygon(data=seoul_merge,
               aes(x=long,y=lat,group=group,fill=single_person_hh),color="white")+
  scale_fill_gradient(low="#FAFAA0",high = "#FFA500",space="lab",guide="colourbar")+
  labs(fill="서울시 65세 이상 1인 노인 가구 분포(단위:명)")+
  theme_void()+
  theme(legend.position = c(.15, .85))+
  geom_text(data=gu_name_seoul,
            aes(x=long, y=lat,label=paste(sgg_nm,single_person_hh,sep="\n")))

# 수원시 지도 만들기
suwon_map<-new_map[new_map$id>=41111 & new_map$id<=41117,]
suwon_merge <- merge(suwon_map,suwon_over_2019,by='id')

# 수원시 65세 이상 독거노인 가구 분포 시각화
suwon_plot<- ggplot()+
  geom_polygon(data=suwon_merge,
               aes(x=long,y=lat,group=group,fill=single_person_hh),color="white")+
  scale_fill_gradient(low="#FAFAA0",high = "#FFA500",space="lab",guide="colourbar")+
  labs(fill="수원시 65세이상 1인 가구 분포(단위:명)")+
  theme_void()+
  theme(legend.position = c(.15, .85))+
  geom_text(data=gu_name_suwon,
            aes(x=long,y=lat,label=paste(sgg_nm,single_person_hh,sep="\n")))

# 진주시 지도 만들기
jinju_map <- new_map[new_map$id==48170,]
jinju_merge <- merge(jinju_map,jinju_over_2019,by="id")

# 진주시 65세 이상 독거노인 가구 분포 시각화

jinju_plot<- ggplot()+
  geom_polygon(data = jinju_merge,
               aes(x=long,y=lat,group=group,fill=single_person_hh),colour="white")+
  scale_fill_gradient(low="#FAFAA0",high = "#FFA500",space="lab",guide="colourbar")+
  labs(fill="진주시 65세 이상 1인 노인 가구 분포(단위:명)")+
  theme_void()+
  theme(legend.position = c(.15, .85))+
  geom_text(data=gu_name_jinju,
            aes(x=long,y=lat,label=paste(sgg_nm,single_person_hh,sep="\n")))

# 창원시 지도 만들기
changwon_map <- new_map[new_map$id>=48121 & new_map$id <= 48129,]
changwon_merge <- merge(changwon_map,changwon_over_2019,by="id")

# 창원시 65세 이상 독거노인 가구 분포 시각화
changwon_plot <- ggplot()+
  geom_polygon(data=changwon_merge,
               aes(x=long, y=lat,group=group,fill=single_person_hh),color="white")+
  scale_fill_gradient(low="#FAFAA0",high = "#FFA500",space="lab",guide="colourbar")+
  labs(fill="창원시 65세 이상 1인 가구 분포(단위:명)")+
  theme_void()+
  theme(legend.position = c(.15, .85))+
  geom_text(data=gu_name_changwon,
            aes(x=long,y=lat,label=paste(sgg_nm,single_person_hh,sep="\n")))

# 지도 병렬

grid.arrange(seoul_plot,suwon_plot,changwon_plot,jinju_plot,nrow=2,ncol=2)


















