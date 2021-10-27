#필요 라이브러리 설치
library(ggplot2)
library(reshape2)


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

gwanank_f_2019 <- subset(df,
                         select= v_name,
                         subset = (sgg_nm == "관악구" & s0=="여자" & date=="2019"))


gwanank_f_2019 <- gwanank_f_2019[-1,]

gwanank_age_2019 <- aggregate(single_person_hh ~ age,gwanank_f_2019,sum)

# 서울시 관악구 1인 여성가구 연령대 막대 그래프
ggplot(data=gwanank_age_2019,aes(x=age,y=single_person_hh))+
  geom_bar(stat = "identity",fill="#F2728C",colour="black")+
  labs(x="연령",
       y="빈도",
       title="서울 관악구 1인 여성가구 연령 분포")+
  scale_x_discrete(limits=c("20대","30대","40대","50대","60대","65세 이상"))+
  geom_text(aes(label=single_person_hh))

#서울시 관악구 1인 여성 가구 20대 거주 유형
gwanank_f_20 <- subset(gwanank_f_2019,
                       select = v_name,
                       subset =(age=="20대"))

shape <- c("단독주택","아파트","연립주택","다세대","비주거용 건물내 주택","주택이외거처")
freq_seoul <- c(sum(gwanank_f_20$detached_house),sum(gwanank_f_20$apartment),
                sum(gwanank_f_20$townhouse),sum(gwanank_f_20$multiple0_house),
                sum(gwanank_f_20$house_in_commercial),sum(gwanank_f_20$not_in_house))
perc_seoul <- freq_seoul/sum(freq_seoul)*100
pie_seoul <- data.frame(shape,freq_seoul,perc_seoul)

ggplot(data=pie_seoul,aes(x="",y=perc_seoul,fill=shape))+
  geom_bar(stat="identity",width=0.5)+
  coord_polar('y',start = 0)+
  labs(title = "서울 관악구 20대 1인 여성가구 주거 형태")+
  geom_text(aes(label=paste0(round(perc_seoul,2),"%")),position = position_stack(vjust=.5), check_overlap = T)+
  scale_fill_manual(values = c("#55DDE0","#33658A","#2F4858","#F6AE2D","#F26419","#999999"),
                    name = c(),
                    labels = c("아파트","단독주택","비주거용 건물내 주택","연립주택","주택이외거처","다세대"))+
  theme_void()+
  theme(axis.line = element_blank(), axis.text = element_blank(),axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5,color="#666666"))

# 수원시 권선구, 영통구



suwon_k<- subset(df,
                 select= v_name,
                 subset = (sgg_nm == "수원시권선구" & s0=="여자" & date=="2019"))
suwon_k <- suwon_k[-1,]

suwon_k_age<- aggregate(single_person_hh ~ age,suwon_k,sum)

suwon_y<- subset(df,
                 select=v_name,
                 subset=(sgg_nm=="수원시영통구"& s0=="여자" & date=="2019"))
suwon_y<-suwon_y[-1,]

suwon_y_age<- aggregate(single_person_hh ~ age,suwon_y,sum)

# 수원시 권선구 1인 여성가구 연령 분포 막대
ggplot(data=suwon_k_age,aes(x=age,y=single_person_hh))+
  geom_bar(stat = "identity",fill="#F2728C",colour="black")+
  labs(x="연령",
       y="빈도",
       title="수원시 권선구 1인 여성가구 연령")+
  scale_x_discrete(limits=c( "20대","30대","40대","50대","60대","65세 이상"))+
  geom_text(aes(label=single_person_hh))

# 수원시 영통구 1인 여성가구 연령 분포 막대 그래프

ggplot(data=suwon_y_age,aes(x=age,y=single_person_hh))+
  geom_bar(stat = "identity",fill="#F2728C",colour="black")+
  labs(x="연령",
       y="빈도",
       title="수원시 영통구 1인 여성가구 연령")+
  scale_x_discrete(limits=c( "20대 미만","20대","30대","40대","50대","60대","65세 이상"))+
  geom_text(aes(label=single_person_hh))

#수원시 권선구 60대 이상 거주형태 파이 그래프
suwon_k_65 <- subset(suwon_k,
                       select = v_name,
                       subset =(age=="65세 이상"))

shape_suwon <- c("단독주택","아파트","연립주택","다세대","비주거용 건물 내 주택","주택이외거처")
freq_suwon_k <- c(sum(suwon_k_65$detached_house),sum(suwon_k_65$apartment),
                  sum(suwon_k_65$townhouse),sum(suwon_k_65$multiple0_house),
                  sum(suwon_k_65$house_in_commercial),sum(suwon_k_65$not_in_house))
perc_suwon_k <- freq_suwon_k/sum(freq_suwon_k)*100
pie_suwon_k <- data.frame(shape_suwon,freq_suwon_k,perc_suwon_k)

ggplot(data=pie_suwon_k,aes(x="",y=perc_suwon_k,fill=shape_suwon))+
  geom_bar(stat="identity",width=0.5)+
  coord_polar('y',start = 0)+
  labs(title = "수원시 권선구 65세 이상 1 여성가구 주거 형태")+
  geom_text(aes(label=paste0(round(perc_suwon_k,2),"%")),position = position_stack(vjust=.5), check_overlap = T)+
  scale_fill_manual(values = c("#55DDE0","#33658A","#2F4858","#F6AE2D","#F26419","#999999"),
                    name = c(),
                    labels = c("아파트","단독주택","비주거용 건물내 주택","다세대","주택이외거처","연립주택"))+
  theme_void()+
  theme(axis.line = element_blank(), axis.text = element_blank(),axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5,color="#666666"))


#수원시 영통구 20대 거주형태 파이 그래프
suwon_y_20 <- subset(suwon_k,
                     select = v_name,
                     subset = (age =="20대"))


freq_suwon_y <- c(sum(suwon_y_20$detached_house),sum(suwon_y_20$apartment),
                  sum(suwon_y_20$townhouse),sum(suwon_y_20$multiple0_house),
                  sum(suwon_y_20$house_in_commercial),sum(suwon_y_20$not_in_house))

perc_suwon_y <- freq_suwon_y/sum(freq_suwon_y)*100
pie_suwon_y <- data.frame(shape,freq_suwon_y,perc_suwon_y)

ggplot(data=pie_suwon_y,aes(x="",y=perc_suwon_y,fill=shape))+
  geom_bar(stat="identity",width=0.5)+
  coord_polar('y',start = 0)+
  labs(title = "수원시 영통구 20대 1인 여성가구 주거 형태")+
  geom_text(aes(label=paste0(round(perc_suwon_y,2),"%")),position = position_stack(vjust=.5), check_overlap = T)+
  scale_fill_manual(values = c("#55DDE0","#33658A","#2F4858","#F6AE2D","#F26419","#999999"),
                    name = c(),
                    labels = c("다세대","단독주택","비주거용 건물내 주택","아파트","연립주택","주택이외 거처"))+
  theme_void()+
  theme(axis.line = element_blank(), axis.text = element_blank(),axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5,color="#666666"))

#진주시 1인 여성가구 연령변수 추출
jinju<- subset(df,
                 select= v_name,
                 subset = (sgg_nm == "진주시" & s0=="여자" & date=="2019"))
jinju <- jinju[-1,]

jinju_age<- aggregate(single_person_hh ~ age,jinju,sum)

#진주시 1인 여성가구 연령 분포 막대그래프 시각화
ggplot(data=jinju_age,aes(x=age,y=single_person_hh))+
  geom_bar(stat = "identity",fill="#F2728C",colour="black")+
  labs(x="연령",
       y="빈도",
       title="진주시 1인 여성가구 연령")+
  scale_x_discrete(limits=c( "20대 미만","20대","30대","40대","50대","60대","65세 이상"))+
  geom_text(aes(label=single_person_hh))

#진주시 20대 여성가구 주거 형태 파이 그래프
jinju_age_20 <- subset(jinju,
                       select = v_name,
                       subset = (age == "20대" ))
freq_jinju_20 <- c(sum(jinju_age_20$detached_house),sum(jinju_age_20$apartment),
                  sum(jinju_age_20$townhouse),sum(jinju_age_20$multiple0_house),
                  sum(jinju_age_20$house_in_commercial),sum(jinju_age_20$not_in_house))

perc_jinju_20 <- freq_jinju_20/sum(freq_jinju_20)*100
pie_jinju_20 <- data.frame(shape,freq_jinju_20,perc_jinju_20)

ggplot(data=pie_jinju_20,aes(x="",y=perc_jinju_20,fill=shape))+
  geom_bar(stat="identity",width=0.5)+
  coord_polar('y',start = 0)+
  labs(title = "진주시 20대 1인 여성가구 주거 형태")+
  geom_text(aes(label=paste0(round(perc_jinju_20,2),"%")),position = position_stack(vjust=.5), check_overlap = T)+
  scale_fill_manual(values = c("#55DDE0","#33658A","#2F4858","#F6AE2D","#F26419","#999999"),
                    name = c(),
                    labels = c("다세대","단독주택","비주거용 건물내 주택","아파트","연립주택","주택이외 거처"))+
  theme_void()+
  theme(axis.line = element_blank(), axis.text = element_blank(),axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5,color="#666666"))

#진주시 65세 이상 1인 여성가구
jinju_age_65 <- subset(jinju,
                       select = v_name,
                       subset = (age == "65세 이상" ))
freq_jinju_65 <- c(sum(jinju_age_65$detached_house),sum(jinju_age_65$apartment),
                   sum(jinju_age_65$townhouse),sum(jinju_age_65$multiple0_house),
                   sum(jinju_age_65$house_in_commercial),sum(jinju_age_65$not_in_house))

perc_jinju_65 <- freq_jinju_65/sum(freq_jinju_65)*100
pie_jinju_65 <- data.frame(shape,freq_jinju_65,perc_jinju_65)

ggplot(data=pie_jinju_65,aes(x="",y=perc_jinju_65,fill=shape))+
  geom_bar(stat="identity",width=0.5)+
  coord_polar('y',start = 0)+
  labs(title = "진주시 65세 이상 1인 여성가구 주거 형태")+
  geom_text(aes(label=paste0(round(perc_jinju_65,2),"%")),position = position_stack(vjust=.5), check_overlap = T)+
  scale_fill_manual(values = c("#55DDE0","#33658A","#2F4858","#F6AE2D","#F26419","#999999"),
                    name = c(),
                    labels = c("다세대","단독주택","비주거용 건물내 주택","아파트","연립주택","주택이외 거처"))+
  theme_void()+
  theme(axis.line = element_blank(), axis.text = element_blank(),axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5,color="#666666"))


# 창원시 마산합포구 1인 여성가구 연령 추출
changwon_mh<- subset(df,
               select= v_name,
               subset = (sgg_nm == "창원시마산합포구" & s0=="여자" & date=="2019"))
changwon_mh <- changwon_mh[-1,]

changwon_mh_age<- aggregate(single_person_hh ~ age,changwon_mh,sum)

#창원시 마산합포구 1인 여성가구 연령 분포 막대그래프 시각화
ggplot(data=changwon_mh_age,aes(x=age,y=single_person_hh))+
  geom_bar(stat = "identity",fill="#F2728C",colour="black")+
  labs(x="연령",
       y="빈도",
       title="창원시 마산합포구 1인 여성가구 연령")+
  scale_x_discrete(limits=c( "20대 미만","20대","30대","40대","50대","60대","65세 이상"))+
  geom_text(aes(label=single_person_hh))

#창원시 마산합포구 65세 이상 1인 가구 주거 형태 파이 차트
changwon_age_65 <- subset(changwon_mh,
                       select = v_name,
                       subset = (age == "65세 이상" ))
freq_changwon_65 <- c(sum(changwon_age_65$detached_house),sum(changwon_age_65$apartment),
                   sum(changwon_age_65$townhouse),sum(changwon_age_65$multiple0_house),
                   sum(changwon_age_65$house_in_commercial),sum(changwon_age_65$not_in_house))

perc_changwon_65 <- freq_changwon_65/sum(freq_changwon_65)*100
pie_changwon_65 <- data.frame(shape,freq_changwon_65,perc_changwon_65)

ggplot(data=pie_changwon_65,aes(x="",y=perc_changwon_65,fill=shape))+
  geom_bar(stat="identity",width=0.5)+
  coord_polar('y',start = 0)+
  labs(title = "창원시 마산 합포구 65세 이상 1인 여성가구 주거 형태")+
  geom_text(aes(label=paste0(round(perc_changwon_65,2),"%")),position = position_stack(vjust=.5), check_overlap = T)+
  scale_fill_manual(values = c("#55DDE0","#33658A","#2F4858","#F6AE2D","#F26419","#999999"),
                    name = c(),
                    labels = c("다세대","단독주택","비주거용 건물내 주택","아파트","연립주택","주택이외 거처"))+
  theme_void()+
  theme(axis.line = element_blank(), axis.text = element_blank(),axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5,color="#666666"))


# 창원시 의창구  1인 여성가구 연령 추출
changwon_y<- subset(df,
                     select= v_name,
                     subset = (sgg_nm == "창원시의창구" & s0=="여자" & date=="2019"))
changwon_y <- changwon_y[-1,]

changwon_y_age<- aggregate(single_person_hh ~ age,changwon_y,sum)

#창원시 의창구 1인 여성가구 연령 분포 막대그래프 시각화
ggplot(data=changwon_y_age,aes(x=age,y=single_person_hh))+
  geom_bar(stat = "identity",fill="#F2728C",colour="black")+
  labs(x="연령",
       y="빈도",
       title="창원시 의창구 1인 여성가구 연령")+
  scale_x_discrete(limits=c( "20대 미만","20대","30대","40대","50대","60대","65세 이상"))+
  geom_text(aes(label=single_person_hh))

#창원시 의창구 50대 여성 1인 가구 주거 형태 파이 차트
changwon_age_50 <- subset(changwon_y,
                          select = v_name,
                          subset = (age == "50대" ))
freq_changwon_50 <- c(sum(changwon_age_50$detached_house),sum(changwon_age_50$apartment),
                      sum(changwon_age_50$townhouse),sum(changwon_age_50$multiple0_house),
                      sum(changwon_age_50$house_in_commercial),sum(changwon_age_50$not_in_house))

perc_changwon_50 <- freq_changwon_50/sum(freq_changwon_50)*100
pie_changwon_50 <- data.frame(shape,freq_changwon_50,perc_changwon_50)

ggplot(data=pie_changwon_50,aes(x="",y=perc_changwon_50,fill=shape))+
  geom_bar(stat="identity",width=0.5)+
  coord_polar('y',start = 0)+
  labs(title = "창원시 의창구 50대 이상 1인 여성가구 주거 형태")+
  geom_text(aes(label=paste0(round(perc_changwon_50,2),"%")),position = position_stack(vjust=.5), check_overlap = T)+
  scale_fill_manual(values = c("#55DDE0","#33658A","#2F4858","#F6AE2D","#F26419","#999999"),
                    name = c(),
                    labels = c("다세대","단독주택","비주거용 건물내 주택","아파트","연립주택","주택이외 거처"))+
  theme_void()+
  theme(axis.line = element_blank(), axis.text = element_blank(),axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5,color="#666666"))





