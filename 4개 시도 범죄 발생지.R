library(treemap)
library(treemapify)
library(ggplot2)
library(dplyr)
library(scales)
library(colorspace)

crime_place <- read.csv("C://LJH/Suwon_Security/data/범죄 발생지.csv",header = T, encoding = "ANSI")

#연도별 데이터 선택
v_name <- names(crime_place)
crime_2016 <- subset(crime_place,
                     select = v_name,
                     subset = (year =="2016년"))


crime_2017 <- subset(crime_place,
                     select = v_name,
                     subset = (year =="2017년"))


crime_2018 <- subset(crime_place,
                     select = v_name,
                     subset = (year =="2018년"))


crime_2019 <- subset(crime_place,
                     select = v_name,
                     subset = (year =="2019년"))

# 2016,2017,2018,2019,트리맵 차트 데이터 준비

dept <- rep(c(crime_2016$dept),4)

place <- rep(c("서울","수원","진주","창원"),each=15)
palette <- rep(c("Teal","Red-Yellow","Greens","Purples"),each=15)

freq_2016 <- c(crime_2016$seoul,crime_2016$suwon,crime_2016$jinju,crime_2016$changwon)
freq_2017 <- c(crime_2017$seoul,crime_2017$suwon,crime_2017$jinju,crime_2017$changwon)
freq_2018 <- c(crime_2018$seoul,crime_2018$suwon,crime_2018$jinju,crime_2018$changwon)
freq_2019 <- c(crime_2019$seoul,crime_2019$suwon,crime_2019$jinju,crime_2019$changwon)



crime_t_2016 <- data.frame(dept,place,freq_2016)
crime_t_2017 <- data.frame(dept,place,freq_2017)
crime_t_2018 <- data.frame(dept,place,freq_2018)
crime_t_2019 <- data.frame(dept,place,freq_2019)


crime_t_2016 <- mutate(crime_t_2016,
                       palette = palette) %>%
  group_by(palette)%>%
  mutate(
    max_freq_2016 = max(freq_2016),
    color = gradient_n_pal(sequential_hcl(8,palette=palette)[1:8])(freq_2016/max_freq_2016)
    
  )


# 2016년 트리맵 차트 시각화
treemap(crime_t_2016,vSize="freq_2016",index = c("place","dept"))

ggplot(crime_t_2016,
        aes(area = freq_2016,fill= color ,label = dept, subgroup = place))+
  geom_treemap()+
  geom_treemap_text(colour = "white", place = "topleft",grow=F, reflow = T)+
  geom_treemap_subgroup_border(colour= "white")+
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour =
                               "white",colour="FAFAFA" ,fontface = "italic", min.size = 0)+
  scale_fill_identity()
  

