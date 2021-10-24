#필요 라이브러리 설치
library(ggplot2)
library(reshape2)
library(dplyr)
library(plyr)


#데이터 불러오기
df <- read.csv("C://LJH/Suwon_Security/data/(완)4개시도_1인가구수.csv",header=T,encoding="ANSI")

# 범주 통합
df$age[df$age == "20세 미만"] <- "20대 미만"
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

#2019년 관악구 여성 1인가구 추출

v_name <- names(df)

seoul_2019 <- subset(df,
                         select= v_name,
                         subset = (sgg_nm == "관악구" & s0=="여자" & date=="2019"))


seoul_2019 <- seoul_2019[-1,-c(1:7)]

age <-rep(c("20대 미만","20대","20대","30대","30대",
                  "40대","40대","50대","50대","60대",
                  "65세 이상","65세 이상","65세 이상","65세 이상","65세 이상"),6)
shape <- rep(c("단독주택","아파트","연립주택","다세대","상가 주택","주택이외거처"),each=15)
seoul_freq <-c(seoul_2019$detached_house,seoul_2019$apartment,seoul_2019$townhouse,
               seoul_2019$multiple0_house,seoul_2019$house_in_commercial,
               seoul_2019$not_in_house)
seoul_count <- data.frame(age,shape,seoul_freq)
seoul_count <- aggregate(seoul_freq~age+shape,seoul_count,sum)
seoul_count <- arrange(seoul_count,age,shape,seoul_freq)


seoul_perc <-c(round(seoul_count$seoul_freq[1:6]/sum(seoul_count$seoul_freq[1:6])*100,2),
               round(seoul_count$seoul_freq[7:12]/sum(seoul_count$seoul_freq[7:12])*100,2),
               round(seoul_count$seoul_freq[13:18]/sum(seoul_count$seoul_freq[13:18])*100,2),
               round(seoul_count$seoul_freq[19:24]/sum(seoul_count$seoul_freq[19:24])*100,2),
               round(seoul_count$seoul_freq[25:30]/sum(seoul_count$seoul_freq[25:30])*100,2),
               round(seoul_count$seoul_freq[31:36]/sum(seoul_count$seoul_freq[31:36])*100,2),
               round(seoul_count$seoul_freq[37:42]/sum(seoul_count$seoul_freq[37:42])*100,2))

seoul_count <- data.frame(seoul_count,seoul_perc)

# 관악구 연령별 누적 비율 막대 차트 시각화
ggplot(seoul_count,aes(x=age,y=seoul_perc,group=shape))+
  geom_col(aes(fill=shape),position="stack",colour = "black")+
  labs(x="연령",
      y="백분율",
      title="서울 관악구 연령별 1인 여성가구 주거형태")+
  scale_x_discrete(limits=c( "20대 미만","20대","30대","40대","50대","60대","65세 이상"))+
  geom_text(aes(label=paste(seoul_perc,"%")),position=position_stack(vjust=0.5))+
  scale_fill_manual(values = c("#55DDE0","#33658A","#2F4858","#F6AE2D","#F26419","#999999"),
                    name = "주거 형태")

# 수원 권선구 여성 1인 가구 추출
suwon_k_2019 <- subset(df,
                     select= v_name,
                     subset = (sgg_nm == "수원시권선구" & s0=="여자" & date=="2019"))

suwon_k_2019 <- suwon_k_2019[-1,-c(1:7)]


suwon_k_freq <-c(suwon_k_2019$detached_house,suwon_k_2019$apartment,suwon_k_2019$townhouse,
                 suwon_k_2019$multiple0_house,suwon_k_2019$house_in_commercial,
                 suwon_k_2019$not_in_house)
suwon_k_count <- data.frame(age,shape,suwon_k_freq)
suwon_k_count <- aggregate(suwon_k_freq~age+shape,suwon_k_count,sum)
suwon_k_count <- arrange(suwon_k_count,age,shape,suwon_k_freq)


suwon_k_perc <-c(round(suwon_k_count$suwon_k_freq[1:6]/sum(suwon_k_count$suwon_k_freq[1:6])*100,2),
               round(suwon_k_count$suwon_k_freq[7:12]/sum(suwon_k_count$suwon_k_freq[7:12])*100,2),
               round(suwon_k_count$suwon_k_freq[13:18]/sum(suwon_k_count$suwon_k_freq[13:18])*100,2),
               round(suwon_k_count$suwon_k_freq[19:24]/sum(suwon_k_count$suwon_k_freq[19:24])*100,2),
               round(suwon_k_count$suwon_k_freq[25:30]/sum(suwon_k_count$suwon_k_freq[25:30])*100,2),
               round(suwon_k_count$suwon_k_freq[31:36]/sum(suwon_k_count$suwon_k_freq[31:36])*100,2),
               round(suwon_k_count$suwon_k_freq[37:42]/sum(suwon_k_count$suwon_k_freq[37:42])*100,2))

suwon_k_count <- data.frame(suwon_k_count,suwon_k_perc)

#수원시 권선구 누적 막대차트 시각화
ggplot(suwon_k_count,aes(x=age,y=suwon_k_perc,group=shape))+
  geom_col(aes(fill=shape),position="stack",colour = "black")+
  labs(x="연령",
       y="백분율",
       title="수원 권선구 연령별 1인 여성가구 주거형태")+
  scale_x_discrete(limits=c( "20대 미만","20대","30대","40대","50대","60대","65세 이상"))+
  geom_text(aes(label=paste(suwon_k_perc,"%")),position=position_stack(vjust=0.5))+
  scale_fill_manual(values = c("#55DDE0","#33658A","#2F4858","#F6AE2D","#F26419","#999999"),
                    name = "주거 형태")


#수원시 영통구 여성 1인 가구 추출
suwon_y_2019 <- subset(df,
                       select= v_name,
                       subset = (sgg_nm == "수원시영통구" & s0=="여자" & date=="2019"))

suwon_y_2019 <- suwon_y_2019[-1,-c(1:7)]


suwon_y_freq <-c(suwon_y_2019$detached_house,suwon_y_2019$apartment,suwon_y_2019$townhouse,
                 suwon_y_2019$multiple0_house,suwon_y_2019$house_in_commercial,
                 suwon_y_2019$not_in_house)
suwon_y_count <- data.frame(age,shape,suwon_y_freq)
suwon_y_count <- aggregate(suwon_y_freq~age+shape,suwon_y_count,sum)
suwon_y_count <- arrange(suwon_y_count,age,shape,suwon_y_freq)


suwon_y_perc <-c(round(suwon_y_count$suwon_y_freq[1:6]/sum(suwon_y_count$suwon_y_freq[1:6])*100,2),
                 round(suwon_y_count$suwon_y_freq[7:12]/sum(suwon_y_count$suwon_y_freq[7:12])*100,2),
                 round(suwon_y_count$suwon_y_freq[13:18]/sum(suwon_y_count$suwon_y_freq[13:18])*100,2),
                 round(suwon_y_count$suwon_y_freq[19:24]/sum(suwon_y_count$suwon_y_freq[19:24])*100,2),
                 round(suwon_y_count$suwon_y_freq[25:30]/sum(suwon_y_count$suwon_y_freq[25:30])*100,2),
                 round(suwon_y_count$suwon_y_freq[31:36]/sum(suwon_y_count$suwon_y_freq[31:36])*100,2),
                 round(suwon_y_count$suwon_y_freq[37:42]/sum(suwon_y_count$suwon_y_freq[37:42])*100,2))

suwon_y_count <- data.frame(suwon_y_count,suwon_y_perc)

#수원시 영통구 누적 막대차트 시각화
ggplot(suwon_y_count,aes(x=age,y=suwon_y_perc,group=shape))+
  geom_col(aes(fill=shape),position="stack",colour = "black")+
  labs(x="연령",
       y="백분율",
       title="수원 영통구 연령별 1인 여성가구 주거형태")+
  scale_x_discrete(limits=c( "20대 미만","20대","30대","40대","50대","60대","65세 이상"))+
  geom_text(aes(label=paste(suwon_y_perc,"%")),position=position_stack(vjust=0.5))+
  scale_fill_manual(values = c("#55DDE0","#33658A","#2F4858","#F6AE2D","#F26419","#999999"),
                    name = "주거 형태")

#진주시 여성 1인가구 추출

jinju_2019 <- subset(df,
                     select= v_name,
                     subset = (sgg_nm == "진주시" & s0=="여자" & date=="2019"))


jinju_2019 <- jinju_2019[-1,-c(1:7)]


jinju_freq <-c(jinju_2019$detached_house,jinju_2019$apartment,jinju_2019$townhouse,
               jinju_2019$multiple0_house,jinju_2019$house_in_commercial,
               jinju_2019$not_in_house)
jinju_count <- data.frame(age,shape,jinju_freq)
jinju_count <- aggregate(jinju_freq~age+shape,jinju_count,sum)
jinju_count <- arrange(jinju_count,age,shape,jinju_freq)


jinju_perc <-c(round(jinju_count$jinju_freq[1:6]/sum(jinju_count$jinju_freq[1:6])*100,2),
               round(jinju_count$jinju_freq[7:12]/sum(jinju_count$jinju_freq[7:12])*100,2),
               round(jinju_count$jinju_freq[13:18]/sum(jinju_count$jinju_freq[13:18])*100,2),
               round(jinju_count$jinju_freq[19:24]/sum(jinju_count$jinju_freq[19:24])*100,2),
               round(jinju_count$jinju_freq[25:30]/sum(jinju_count$jinju_freq[25:30])*100,2),
               round(jinju_count$jinju_freq[31:36]/sum(jinju_count$jinju_freq[31:36])*100,2),
               round(jinju_count$jinju_freq[37:42]/sum(jinju_count$jinju_freq[37:42])*100,2))

jinju_count <- data.frame(jinju_count,jinju_perc)

# 진주시 연령별 누적 비율 막대 차트 시각화
ggplot(jinju_count,aes(x=age,y=jinju_perc,group=shape))+
  geom_col(aes(fill=shape),position="stack",colour = "black")+
  labs(x="연령",
       y="백분율",
       title="진주시 연령별 1인 여성가구 주거형태")+
  scale_x_discrete(limits=c( "20대 미만","20대","30대","40대","50대","60대","65세 이상"))+
  geom_text(aes(label=paste(jinju_perc,"%")),position=position_stack(vjust=0.5))+
  scale_fill_manual(values = c("#55DDE0","#33658A","#2F4858","#F6AE2D","#F26419","#999999"),
                    name = "주거 형태")

#창원시 마산 합포구 여성 1인가구 추출
changwon_h_2019 <- subset(df,
                       select= v_name,
                       subset = (sgg_nm == "창원시마산합포구" & s0=="여자" & date=="2019"))

changwon_h_2019 <- changwon_h_2019[-1,-c(1:7)]


changwon_h_freq <-c(changwon_h_2019$detached_house,changwon_h_2019$apartment,changwon_h_2019$townhouse,
                    changwon_h_2019$multiple0_house,changwon_h_2019$house_in_commercial,
                    changwon_h_2019$not_in_house)
changwon_h_count <- data.frame(age,shape,changwon_h_freq)
changwon_h_count <- aggregate(changwon_h_freq~age+shape,changwon_h_count,sum)
changwon_h_count <- arrange(changwon_h_count,age,shape,changwon_h_freq)


changwon_h_perc <-c(round(changwon_h_count$changwon_h_freq[1:6]/sum(changwon_h_count$changwon_h_freq[1:6])*100,2),
                 round(changwon_h_count$changwon_h_freq[7:12]/sum(changwon_h_count$changwon_h_freq[7:12])*100,2),
                 round(changwon_h_count$changwon_h_freq[13:18]/sum(changwon_h_count$changwon_h_freq[13:18])*100,2),
                 round(changwon_h_count$changwon_h_freq[19:24]/sum(changwon_h_count$changwon_h_freq[19:24])*100,2),
                 round(changwon_h_count$changwon_h_freq[25:30]/sum(changwon_h_count$changwon_h_freq[25:30])*100,2),
                 round(changwon_h_count$changwon_h_freq[31:36]/sum(changwon_h_count$changwon_h_freq[31:36])*100,2),
                 round(changwon_h_count$changwon_h_freq[37:42]/sum(changwon_h_count$changwon_h_freq[37:42])*100,2))

changwon_h_count <- data.frame(changwon_h_count,changwon_h_perc)

#창원시 마산 합포 누적 막대차트 시각화
ggplot(changwon_h_count,aes(x=age,y=changwon_h_perc,group=shape))+
  geom_col(aes(fill=shape),position="stack",colour = "black")+
  labs(x="연령",
       y="백분율",
       title="창원 마산합포구 연령별 1인 여성가구 주거형태")+
  scale_x_discrete(limits=c( "20대 미만","20대","30대","40대","50대","60대","65세 이상"))+
  geom_text(aes(label=paste(changwon_h_perc,"%")),position=position_stack(vjust=0.5))+
  scale_fill_manual(values = c("#55DDE0","#33658A","#2F4858","#F6AE2D","#F26419","#999999"),
                    name = "주거 형태")

#창원시 의창구 여성 1인가구 추출
changwon_y_2019 <- subset(df,
                          select= v_name,
                          subset = (sgg_nm == "창원시의창구" & s0=="여자" & date=="2019"))

changwon_y_2019 <- changwon_y_2019[-1,-c(1:7)]


changwon_y_freq <-c(changwon_y_2019$detached_house,changwon_y_2019$apartment,changwon_y_2019$townhouse,
                    changwon_y_2019$multiple0_house,changwon_y_2019$house_in_commercial,
                    changwon_y_2019$not_in_house)
changwon_y_count <- data.frame(age,shape,changwon_y_freq)
changwon_y_count <- aggregate(changwon_y_freq~age+shape,changwon_y_count,sum)
changwon_y_count <- arrange(changwon_y_count,age,shape,changwon_y_freq)


changwon_y_perc <-c(round(changwon_y_count$changwon_y_freq[1:6]/sum(changwon_y_count$changwon_y_freq[1:6])*100,2),
                    round(changwon_y_count$changwon_y_freq[7:12]/sum(changwon_y_count$changwon_y_freq[7:12])*100,2),
                    round(changwon_y_count$changwon_y_freq[13:18]/sum(changwon_y_count$changwon_y_freq[13:18])*100,2),
                    round(changwon_y_count$changwon_y_freq[19:24]/sum(changwon_y_count$changwon_y_freq[19:24])*100,2),
                    round(changwon_y_count$changwon_y_freq[25:30]/sum(changwon_y_count$changwon_y_freq[25:30])*100,2),
                    round(changwon_y_count$changwon_y_freq[31:36]/sum(changwon_y_count$changwon_y_freq[31:36])*100,2),
                    round(changwon_y_count$changwon_y_freq[37:42]/sum(changwon_y_count$changwon_y_freq[37:42])*100,2))

changwon_y_count <- data.frame(changwon_y_count,changwon_y_perc)

#창원시 의창구 누적 막대차트 시각화
ggplot(changwon_y_count,aes(x=age,y=changwon_y_perc,group=shape))+
  geom_col(aes(fill=shape),position="stack",colour = "black")+
  labs(x="연령",
       y="백분율",
       title="창원 의창구 연령별 1인 여성가구 주거형태")+
  scale_x_discrete(limits=c( "20대 미만","20대","30대","40대","50대","60대","65세 이상"))+
  geom_text(aes(label=paste(changwon_y_perc,"%")),position=position_stack(vjust=0.5))+
  scale_fill_manual(values = c("#55DDE0","#33658A","#2F4858","#F6AE2D","#F26419","#999999"),
                    name = "주거 형태")
 
                                                  

