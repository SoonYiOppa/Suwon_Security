library(dplyr)
library(plyr)
library(reshape)
library(ggmap)
library(ggplot2)

security_light <- read.csv("C://LJH/Suwon_Security/data/전국보안등정보표준데이터.csv", header = T, encoding="ANSI")
security_light <- security_light[,c(2:6)]
security_light <- rename(security_light,
                         c("설치개수" = "freq",
                           "소재지도로명주소"="address1",
                           "소재지지번주소"="address2",
                           "위도"="lat",
                           "경도"="long"))



#서울시 자료 추출 % 위경도 결측치 제거 
seoul_light <- security_light %>%
  filter(grepl("서울",address1,address2))

seoul_light <- seoul_light %>%
  filter(!is.na(long)&!is.na(lat))

#수원시 자료 추출
suwon_light <- security_light %>%
  filter(grepl("수원시",address1,address2))

suwon_light <- suwon_light %>%
  filter(!is.na(long)&!is.na(lat))



#창원시 자료푸풀
changwon_light <- security_light %>%
  filter(grepl("창원시",address2))

changwon_light <- changwon_light %>%
  filter(!is.na(long)&!is.na(lat))

#구글 맵스 Key 적용
register_google(key="구글맵 인증키")


#서울시 시각화
seoul_gc <- geocode(enc2utf8("서울"))
seoul_cen <- as.numeric(seoul_gc)
seoul_map <- get_googlemap(center = seoul_cen,
                           zoom = 11,
                           maptype ="roadmap",
                           maker = gc)

s_map <- ggmap(seoul_map)
s_map+
  geom_point(data=seoul_light,
             aes(x=long,y=lat),color="blue",size=0.1)+
  labs(x="경도",
       y="위도")

# 수원시 시각화
suwon_gc <- geocode(enc2utf8("수원"))
suwon_cen <- as.numeric(suwon_gc)
suwon_map <- get_googlemap(center = suwon_cen,
                           zoom = 12,
                           maptype ="roadmap",                
                           maker = gc)
suwon_map+
  geom_point(data=suwon_light,
               aes(x=long,y=lat))+
  labs(x="경도",
       y="위도",
       title="수원시 상위 6개 어린이 놀이 시설")
             
























