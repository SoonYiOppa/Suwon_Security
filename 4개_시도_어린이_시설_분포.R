library(dplyr)
library(plyr)
library(ggmap)
library(ggplot2)
library(gridExtra)


# 데이터 불러오기
df_children <- read.csv("C://LJH/Suwon_Security/data/4개시도_어린이놀이시설현황.csv", header=T, encoding="ANSI")

#서울시 자료 추출 & 아동시설 빈도표 작성
seoul_child <- df_children %>%
  filter(grepl("서울",street_address))

s_place <- count(seoul_child,"place")
s_perc <- s_place$freq/sum(s_place$freq)*100
s_perc <- round(s_perc,2)
s_place <- cbind(s_place,s_perc)

#서울시 상위 6개 장소 추출
s_top6 <- s_place %>%
          arrange(desc(freq))%>%
          head(6)

seoul_location <- subset(seoul_child,
                         select = c(place,long,lat),
                         subset=(place=="놀이제공업소"|place=="도시공원"|place=="어린이집"|
                                   place=="유치원"| place=="주택단지"|place=="학교"))

#수원시 자료 추출 & 아동시설 빈도표 작성
suwon_child <- df_children %>%
  filter(grepl("수원시",street_address))

suwon_place <- count(suwon_child,"place")
suwon_perc <- suwon_place$freq/sum(suwon_place$freq)
suwon_perc <- round(suwon_perc,2)
suwon_place <- cbind(suwon_place,suwon_perc)

#수원시 상위 6개 추출
suwon_top6 <- suwon_place %>%
  arrange(desc(freq))%>%
  head(6)

suwon_location <- subset(suwon_child,
                         select = c(place,long,lat),
                         subset=(place=="놀이제공업소"|place=="도시공원"|place=="어린이집"|
                                   place=="유치원"| place=="주택단지"|place=="학교"))

#진주시 자료 추출 & 아동시설 빈도표 작성
jinju_child <- df_children %>%
  filter(grepl("진주시",street_address))

j_place <- count(jinju_child,"place")
j_perc <- j_place$freq/sum(j_place$freq)
j_perc <- round(j_perc,2)
j_place <- cbind(j_place,j_perc)

#진주시 상위 6개 자료 추출
j_top6 <- j_place %>%
  arrange(desc(freq))%>%
  head(6)

jinju_location <- subset(jinju_child,
                         select = c(place,long,lat),
                         subset=(place=="식품접객업소"|place=="도시공원"|place=="어린이집"|
                                   place=="유치원"| place=="주택단지"|place=="학교"))




#창원시 자료 추출 & 아동시설 빈도표 작성
changwon_child <- df_children %>%
  filter(grepl("창원",street_address))

c_place <- count(changwon_child,"place")
c_perc <- c_place$freq/sum(c_place$freq)
c_perc <- round(c_perc,2)
c_place <- cbind(c_place,c_perc)

#창원시 상위 6개 자료 추출
c_top6 <- c_place %>%
  arrange(desc(freq)) %>%
  head(6)

changwon_location <- subset(changwon_child,
                         select = c(place,long,lat),
                         subset=(place=="식품접객업소"|place=="도시공원"|place=="어린이집"|
                                   place=="유치원"| place=="주택단지"|place=="학교"))


# 서울시 top6 장소 막대그래프 그래프 시각화
seoul_bar <- ggplot(data=s_top6,aes(x=place,y=freq))+
  geom_bar(stat = "identity",fill="#F2728C",colour="black")+
  labs(x="설치장소",
       y="빈도",
       title="서울시 상위 6개 어린이 놀이 시설")+
  geom_text(aes(label=freq))

# 수원시 top6 장소 막대 그래프 시각화
suwon_bar <- ggplot(data=suwon_top6,aes(x=place,y=freq))+
  geom_bar(stat = "identity",fill="#9E7EB9",colour="black")+
  labs(x="설치장소",
       y="빈도",
       title="수원시 상위 6개 어린이 놀이 시설")+
  geom_text(aes(label=freq))

# 진주시 top6 장소 막대 그래프 시각화
jinju_bar <- ggplot(data=j_top6,aes(x=place,y=freq))+
  geom_bar(stat = "identity",fill="#F79552",colour="black")+
  labs(x="설치장소",
       y="빈도",
       title="진주시 상위 6개 어린이 놀이 시설")+
  geom_text(aes(label=freq))

# 창원시 top6 장소 막대 그래프 시각화 
changwon_bar <- ggplot(data=c_top6,aes(x=place,y=freq))+
  geom_bar(stat = "identity",fill="#81D3EB",colour="black")+
  labs(x="설치장소",
       y="빈도",
       title="창원시 상위 6개 어린이 놀이 시설")+
  geom_text(aes(label=freq))

# 4개 시도 막대 그그래프 병렬
grid.arrange(seoul_bar,suwon_bar,changwon_bar,jinju_bar,nrow=2,ncol=2)


# 4개시도 to6 장소 막대 그래프 시각화
sido <- rep(c("서울특별시","수원시","진주시","창원시"),each=6)
place<- c(s_top6$place,suwon_top6$place,j_top6$place,c_top6$place)
count <- c(s_top6$freq,suwon_top6$freq,j_top6$freq,c_top6$freq)

all_top6 <- cbind(sido,place)
all_top6 <- cbind(all_top6,count)
all_top6 <- as.data.frame(all_top6)

#그룹 막대 그래프 만들기
ggplot(all_top6,
       aes(x=sido,y=count,group=place))+
  geom_col(aes(fill=place),position="dodge")+
  scale_fill_discrete(name="설치 장소")+
  labs(x="4개 시도",
       y="빈도수",
       title="4개시도 상위 6개 어린이 놀이 시설")+
  geom_text(aes(label=count),position = position_dodge(0.9))


#구글 맵스 API key 입력
register_google(key="구글 맵스 API Key 사용")


#서울시 유아시설 설치장소 시각화


seoul_gc <- geocode(enc2utf8("서울"))
seoul_cen <- as.numeric(seoul_gc)
seoul_map <- get_googlemap(center = seoul_cen,
                     zoom = 11,
                     maptype ="roadmap",
                     maker = gc)

s_map <- ggmap(seoul_map)
s_map+
  geom_point(data=seoul_location,
             aes(x=long,y=lat,color=place))+
  labs(x="경도",
       y="위도",
       title="서울시 상위 6개 어린이 놀이 시설")

#수원시 유아시설 설치장소 시각화
suwon_gc <- geocode(enc2utf8("수원"))
suwon_cen <- as.numeric(suwon_gc)
suwon_map <- get_googlemap(center = suwon_cen,
                           zoom = 12,
                           maptype ="roadmap",
                           maker = gc)

suwon_map <- ggmap(suwon_map)
suwon_map+
  geom_point(data=suwon_location,
             aes(x=long,y=lat,color=place))+
  labs(x="경도",
       y="위도",
       title="수원시 상위 6개 어린이 놀이 시설")



#진주시 유아시설 설치장소 시각화
jinju_gc <- geocode(enc2utf8("진주시"))
jinju_cen <- as.numeric(jinju_gc)
jinju_map <- get_googlemap(center = jinju_cen,
                           zoom = 12,
                           maptype ="roadmap",
                           maker = gc)

jinju_map <- ggmap(jinju_map)
jinju_map+
  geom_point(data=jinju_location,
             aes(x=long,y=lat,color=place))+
  labs(x="경도",
       y="위도",
       title="진주시 상위 6개 어린이 놀이 시설")



#창원시 유아시설 설치 장소 시각화
changwon_gc <- geocode(enc2utf8("창원시"))
changwon_cen <- as.numeric(changwon_gc)
changwon_map <- get_googlemap(center = changwon_cen,
                           zoom = 11,
                           maptype ="roadmap",
                           maker = gc)

changwon_map <- ggmap(changwon_map)
changwon_map+
  geom_point(data=changwon_location,
             aes(x=long,y=lat,color=place))+
  labs(x="경도",
       y="위도",
       title="창원시 상위 6개 어린이 놀이 시설")

#4개시도 지도 병렬


