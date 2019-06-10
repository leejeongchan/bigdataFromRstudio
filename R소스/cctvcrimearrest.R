#빅데이터 과제
#라이브러리 호출출
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggmap)
####################################################################################
#1 CCTV Lee Jeong Chan
#1-1. 연도별+지역별 CCTV 설치 비율 파악
#1-2. 추가적 전체 지도
#우선 연도별로 CCTV 개수들을 파악해야함
cctv_year <- read.csv("After_ccty_data.csv",header=T)

cctv_year_df <- subset(cctv_year,select=c(소재지주소,설치년월)) #소재지주소, 설치년월 열을 뽑음

cctv_year_df$소재지주소 <- as.character(cctv_year_df$소재지주소) #소재지주소를 문자화시킴

cctv_year_df$설치년월 <- substr(cctv_year_df$설치년월,1,4) #설치년월에서 1부터4까지 문자열 뽑음 즉 년도만

#주소 공백으로 분리하여 새로 만듬
cctv_local_split <- data.frame(do.call('rbind',strsplit(cctv_year_df$소재지주소,split=" ",fixed=TRUE)))

#그래프를 그리기 위한 준비로써 각 문자열 분리를 통해 주소와 설치년월 새로운 데이터프레임 생성
cctv_year_df2 <- data.frame(local=cctv_local_split$X1,year=cctv_year_df$설치년월)

#지역과 년도를 문자화시키기기
cctv_year_df2$local <- as.character(cctv_year_df2$local)
cctv_year_df2$year<- as.character(cctv_year_df2$year)



#각 지역별 년도별 CCTV 개수 파악 프레임 만들기(합치는거 포함) =>최종 incheon_gang2 프레임임
incheon_cnt_2017 <-length(which(cctv_year_df2$local=="인천광역시" & cctv_year_df2$year=="2017"))
incheon_cnt_2016 <-length(which(cctv_year_df2$local=="인천광역시" & cctv_year_df2$year=="2016"))
incheon_cnt_2015 <-length(which(cctv_year_df2$local=="인천광역시" & cctv_year_df2$year=="2015"))
incheon_cnt_2014 <-length(which(cctv_year_df2$local=="인천광역시" & cctv_year_df2$year=="2014"))
incheon_cnt_2013 <-length(which(cctv_year_df2$local=="인천광역시" & cctv_year_df2$year=="2013"))
cctv_year_cnt<-data.frame(지역="인천",y2017=incheon_cnt_2017,y2016=incheon_cnt_2016,y2015=incheon_cnt_2015,y2014=incheon_cnt_2014,y2013=incheon_cnt_2013)

gangwondo_cnt_2017 <-length(which(cctv_year_df2$local=="강원도" & cctv_year_df2$year=="2017"))
gangwondo_cnt_2016 <-length(which(cctv_year_df2$local=="강원도" & cctv_year_df2$year=="2016"))
gangwondo_cnt_2015 <-length(which(cctv_year_df2$local=="강원도" & cctv_year_df2$year=="2015"))
gangwondo_cnt_2014 <-length(which(cctv_year_df2$local=="강원도" & cctv_year_df2$year=="2014"))
gangwondo_cnt_2013 <-length(which(cctv_year_df2$local=="강원도" & cctv_year_df2$year=="2013"))
gangwond<-data.frame(지역="강원도",y2017=gangwondo_cnt_2017,y2016=gangwondo_cnt_2016,y2015=gangwondo_cnt_2015,y2014=gangwondo_cnt_2014,y2013=gangwondo_cnt_2013)
incheon_gang<-bind_rows(cctv_year_cnt,gangwond)

gyeonggido_cnt_2017 <-length(which(cctv_year_df2$local=="경기도" & cctv_year_df2$year=="2017"))
gyeonggido_cnt_2016 <-length(which(cctv_year_df2$local=="경기도" & cctv_year_df2$year=="2016"))
gyeonggido_cnt_2015 <-length(which(cctv_year_df2$local=="경기도" & cctv_year_df2$year=="2015"))
gyeonggido_cnt_2014 <-length(which(cctv_year_df2$local=="경기도" & cctv_year_df2$year=="2014"))
gyeonggido_cnt_2013 <-length(which(cctv_year_df2$local=="경기도" & cctv_year_df2$year=="2013"))
gyeonggido<-data.frame(지역="경기도",y2017=gyeonggido_cnt_2017,y2016=gyeonggido_cnt_2016,y2015=gyeonggido_cnt_2015,y2014=gyeonggido_cnt_2014,y2013=gyeonggido_cnt_2013)
incheon_gang2<-bind_rows(incheon_gang,gyeonggido)

gyeongsangnamdo_cnt_2017 <-length(which(cctv_year_df2$local=="경상남도" & cctv_year_df2$year=="2017"))
gyeongsangnamdo_cnt_2016 <-length(which(cctv_year_df2$local=="경상남도" & cctv_year_df2$year=="2016"))
gyeongsangnamdo_cnt_2015 <-length(which(cctv_year_df2$local=="경상남도" & cctv_year_df2$year=="2015"))
gyeongsangnamdo_cnt_2014 <-length(which(cctv_year_df2$local=="경상남도" & cctv_year_df2$year=="2014"))
gyeongsangnamdo_cnt_2013 <-length(which(cctv_year_df2$local=="경상남도" & cctv_year_df2$year=="2013"))
gyeongsangnamdo<-data.frame(지역="경상남도",y2017=gyeongsangnamdo_cnt_2017,y2016=gyeongsangnamdo_cnt_2016,y2015=gyeongsangnamdo_cnt_2015,y2014=gyeongsangnamdo_cnt_2014,y2013=gyeongsangnamdo_cnt_2013)
incheon_gang2<-bind_rows(incheon_gang2,gyeongsangnamdo)

gyeongsangbukdo_cnt_2017 <-length(which(cctv_year_df2$local=="경상북도" & cctv_year_df2$year=="2017"))
gyeongsangbukdo_cnt_2016 <-length(which(cctv_year_df2$local=="경상북도" & cctv_year_df2$year=="2016"))
gyeongsangbukdo_cnt_2015 <-length(which(cctv_year_df2$local=="경상북도" & cctv_year_df2$year=="2015"))
gyeongsangbukdo_cnt_2014 <-length(which(cctv_year_df2$local=="경상북도" & cctv_year_df2$year=="2014"))
gyeongsangbukdo_cnt_2013 <-length(which(cctv_year_df2$local=="경상북도" & cctv_year_df2$year=="2013"))
gyeongsangbukdo<-data.frame(지역="경상북도",y2017=gyeongsangbukdo_cnt_2017,y2016=gyeongsangbukdo_cnt_2016,y2015=gyeongsangbukdo_cnt_2015,y2014=gyeongsangbukdo_cnt_2014,y2013=gyeongsangbukdo_cnt_2013)
incheon_gang2<-bind_rows(incheon_gang2,gyeongsangbukdo)

gwangju_cnt_2017 <-length(which(cctv_year_df2$local=="광주광역시" & cctv_year_df2$year=="2017"))
gwangju_cnt_2016 <-length(which(cctv_year_df2$local=="광주광역시" & cctv_year_df2$year=="2016"))
gwangju_cnt_2015 <-length(which(cctv_year_df2$local=="광주광역시" & cctv_year_df2$year=="2015"))
gwangju_cnt_2014 <-length(which(cctv_year_df2$local=="광주광역시" & cctv_year_df2$year=="2014"))
gwangju_cnt_2013 <-length(which(cctv_year_df2$local=="광주광역시" & cctv_year_df2$year=="2013"))
gwangju<-data.frame(지역="광주광역시",y2017=gwangju_cnt_2017,y2016=gwangju_cnt_2016,y2015=gwangju_cnt_2015,y2014=gwangju_cnt_2014,y2013=gwangju_cnt_2013)
incheon_gang2<-bind_rows(incheon_gang2,gwangju)

daegu_cnt_2017 <-length(which(cctv_year_df2$local=="대구광역시" & cctv_year_df2$year=="2017"))
daegu_cnt_2016 <-length(which(cctv_year_df2$local=="대구광역시" & cctv_year_df2$year=="2016"))
daegu_cnt_2015 <-length(which(cctv_year_df2$local=="대구광역시" & cctv_year_df2$year=="2015"))
daegu_cnt_2014 <-length(which(cctv_year_df2$local=="대구광역시" & cctv_year_df2$year=="2014"))
daegu_cnt_2013 <-length(which(cctv_year_df2$local=="대구광역시" & cctv_year_df2$year=="2013"))
daegu<-data.frame(지역="대구광역시",y2017=daegu_cnt_2017,y2016=daegu_cnt_2016,y2015=daegu_cnt_2015,y2014=daegu_cnt_2014,y2013=daegu_cnt_2013)
incheon_gang2<-bind_rows(incheon_gang2,daegu)

daejeon_cnt_2017 <-length(which(cctv_year_df2$local=="대전광역시" & cctv_year_df2$year=="2017"))
daejeon_cnt_2016 <-length(which(cctv_year_df2$local=="대전광역시" & cctv_year_df2$year=="2016"))
daejeon_cnt_2015 <-length(which(cctv_year_df2$local=="대전광역시" & cctv_year_df2$year=="2015"))
daejeon_cnt_2014 <-length(which(cctv_year_df2$local=="대전광역시" & cctv_year_df2$year=="2014"))
daejeon_cnt_2013 <-length(which(cctv_year_df2$local=="대전광역시" & cctv_year_df2$year=="2013"))
daejeon<-data.frame(지역="대전광역시",y2017=daejeon_cnt_2017,y2016=daejeon_cnt_2016,y2015=daejeon_cnt_2015,y2014=daejeon_cnt_2014,y2013=daejeon_cnt_2013)
incheon_gang2<-bind_rows(incheon_gang2,daejeon)

busan_cnt_2017 <-length(which(cctv_year_df2$local=="부산광역시" & cctv_year_df2$year=="2017"))
busan_cnt_2016 <-length(which(cctv_year_df2$local=="부산광역시" & cctv_year_df2$year=="2016"))
busan_cnt_2015 <-length(which(cctv_year_df2$local=="부산광역시" & cctv_year_df2$year=="2015"))
busan_cnt_2014 <-length(which(cctv_year_df2$local=="부산광역시" & cctv_year_df2$year=="2014"))
busan_cnt_2013 <-length(which(cctv_year_df2$local=="부산광역시" & cctv_year_df2$year=="2013"))
busan<-data.frame(지역="부산광역시",y2017=busan_cnt_2017,y2016=busan_cnt_2016,y2015=busan_cnt_2015,y2014=busan_cnt_2014,y2013=busan_cnt_2013)
incheon_gang2<-bind_rows(incheon_gang2,busan)

seoul_cnt_2017 <-length(which(cctv_year_df2$local=="서울특별시" & cctv_year_df2$year=="2017"))
seoul_cnt_2016 <-length(which(cctv_year_df2$local=="서울특별시" & cctv_year_df2$year=="2016"))
seoul_cnt_2015 <-length(which(cctv_year_df2$local=="서울특별시" & cctv_year_df2$year=="2015"))
seoul_cnt_2014 <-length(which(cctv_year_df2$local=="서울특별시" & cctv_year_df2$year=="2014"))
seoul_cnt_2013 <-length(which(cctv_year_df2$local=="서울특별시" & cctv_year_df2$year=="2013"))
seoul<-data.frame(지역="서울특별시",y2017=seoul_cnt_2017,y2016=seoul_cnt_2016,y2015=seoul_cnt_2015,y2014=seoul_cnt_2014,y2013=seoul_cnt_2013)
incheon_gang2<-bind_rows(incheon_gang2,seoul)

sejong_cnt_2017 <-length(which(cctv_year_df2$local=="세종특별자치시" & cctv_year_df2$year=="2017"))
sejong_cnt_2016 <-length(which(cctv_year_df2$local=="세종특별자치시" & cctv_year_df2$year=="2016"))
sejong_cnt_2015 <-length(which(cctv_year_df2$local=="세종특별자치시" & cctv_year_df2$year=="2015"))
sejong_cnt_2014 <-length(which(cctv_year_df2$local=="세종특별자치시" & cctv_year_df2$year=="2014"))
sejong_cnt_2013 <-length(which(cctv_year_df2$local=="세종특별자치시" & cctv_year_df2$year=="2013"))
sejong<-data.frame(지역="세종특별자치시",y2017=sejong_cnt_2017,y2016=sejong_cnt_2016,y2015=sejong_cnt_2015,y2014=sejong_cnt_2014,y2013=sejong_cnt_2013)
incheon_gang2<-bind_rows(incheon_gang2,sejong)

ulsan_cnt_2017 <-length(which(cctv_year_df2$local=="울산광역시" & cctv_year_df2$year=="2017"))
ulsan_cnt_2016 <-length(which(cctv_year_df2$local=="울산광역시" & cctv_year_df2$year=="2016"))
ulsan_cnt_2015 <-length(which(cctv_year_df2$local=="울산광역시" & cctv_year_df2$year=="2015"))
ulsan_cnt_2014 <-length(which(cctv_year_df2$local=="울산광역시" & cctv_year_df2$year=="2014"))
ulsan_cnt_2013 <-length(which(cctv_year_df2$local=="울산광역시" & cctv_year_df2$year=="2013"))
ulsan<-data.frame(지역="울산광역시",y2017=ulsan_cnt_2017,y2016=ulsan_cnt_2016,y2015=ulsan_cnt_2015,y2014=ulsan_cnt_2014,y2013=ulsan_cnt_2013)
incheon_gang2<-bind_rows(incheon_gang2,ulsan)

jeonnam_cnt_2017 <-length(which(cctv_year_df2$local=="전라남도" & cctv_year_df2$year=="2017"))
jeonnam_cnt_2016 <-length(which(cctv_year_df2$local=="전라남도" & cctv_year_df2$year=="2016"))
jeonnam_cnt_2015 <-length(which(cctv_year_df2$local=="전라남도" & cctv_year_df2$year=="2015"))
jeonnam_cnt_2014 <-length(which(cctv_year_df2$local=="전라남도" & cctv_year_df2$year=="2014"))
jeonnam_cnt_2013 <-length(which(cctv_year_df2$local=="전라남도" & cctv_year_df2$year=="2013"))
jeonnam<-data.frame(지역="전라남도",y2017=jeonnam_cnt_2017,y2016=jeonnam_cnt_2016,y2015=jeonnam_cnt_2015,y2014=jeonnam_cnt_2014,y2013=jeonnam_cnt_2013)
incheon_gang2<-bind_rows(incheon_gang2,jeonnam)

jeonbuk_cnt_2017 <-length(which(cctv_year_df2$local=="전라북도" & cctv_year_df2$year=="2017"))
jeonbuk_cnt_2016 <-length(which(cctv_year_df2$local=="전라북도" & cctv_year_df2$year=="2016"))
jeonbuk_cnt_2015 <-length(which(cctv_year_df2$local=="전라북도" & cctv_year_df2$year=="2015"))
jeonbuk_cnt_2014 <-length(which(cctv_year_df2$local=="전라북도" & cctv_year_df2$year=="2014"))
jeonbuk_cnt_2013 <-length(which(cctv_year_df2$local=="전라북도" & cctv_year_df2$year=="2013"))
jeonbuk<-data.frame(지역="전라북도",y2017=jeonbuk_cnt_2017,y2016=jeonbuk_cnt_2016,y2015=jeonbuk_cnt_2015,y2014=jeonbuk_cnt_2014,y2013=jeonbuk_cnt_2013)
incheon_gang2<-bind_rows(incheon_gang2,jeonbuk)

jeju_cnt_2017 <-length(which(cctv_year_df2$local=="제주특별자치도" & cctv_year_df2$year=="2017"))
jeju_cnt_2016 <-length(which(cctv_year_df2$local=="제주특별자치도" & cctv_year_df2$year=="2016"))
jeju_cnt_2015 <-length(which(cctv_year_df2$local=="제주특별자치도" & cctv_year_df2$year=="2015"))
jeju_cnt_2014 <-length(which(cctv_year_df2$local=="제주특별자치도" & cctv_year_df2$year=="2014"))
jeju_cnt_2013 <-length(which(cctv_year_df2$local=="제주특별자치도" & cctv_year_df2$year=="2013"))
jeju<-data.frame(지역="제주특별자치도",y2017=jeju_cnt_2017,y2016=jeju_cnt_2016,y2015=jeju_cnt_2015,y2014=jeju_cnt_2014,y2013=jeju_cnt_2013)
incheon_gang2<-bind_rows(incheon_gang2,jeju)

chungnam_cnt_2017 <-length(which(cctv_year_df2$local=="충청남도" & cctv_year_df2$year=="2017"))
chungnam_cnt_2016 <-length(which(cctv_year_df2$local=="충청남도" & cctv_year_df2$year=="2016"))
chungnam_cnt_2015 <-length(which(cctv_year_df2$local=="충청남도" & cctv_year_df2$year=="2015"))
chungnam_cnt_2014 <-length(which(cctv_year_df2$local=="충청남도" & cctv_year_df2$year=="2014"))
chungnam_cnt_2013 <-length(which(cctv_year_df2$local=="충청남도" & cctv_year_df2$year=="2013"))
chungnam<-data.frame(지역="충청남도",y2017=chungnam_cnt_2017,y2016=chungnam_cnt_2016,y2015=chungnam_cnt_2015,y2014=chungnam_cnt_2014,y2013=chungnam_cnt_2013)
incheon_gang2<-bind_rows(incheon_gang2,chungnam)

chungbuk_cnt_2017 <-length(which(cctv_year_df2$local=="충청북도" & cctv_year_df2$year=="2017"))
chungbuk_cnt_2016 <-length(which(cctv_year_df2$local=="충청북도" & cctv_year_df2$year=="2016"))
chungbuk_cnt_2015 <-length(which(cctv_year_df2$local=="충청북도" & cctv_year_df2$year=="2015"))
chungbuk_cnt_2014 <-length(which(cctv_year_df2$local=="충청북도" & cctv_year_df2$year=="2014"))
chungbuk_cnt_2013 <-length(which(cctv_year_df2$local=="충청북도" & cctv_year_df2$year=="2013"))
chungbuk<-data.frame(지역="충청북도",y2017=chungbuk_cnt_2017,y2016=chungbuk_cnt_2016,y2015=chungbuk_cnt_2015,y2014=chungbuk_cnt_2014,y2013=chungbuk_cnt_2013)
incheon_gang2<-bind_rows(incheon_gang2,chungbuk)


#중첩 막대(그래프) 그리기 
df_long$count<-as.numeric(df_long$count) # 수량을 수치화 시킴 => 그래야 그래프 그림
df_long <- gather(incheon_gang2,year,count,-지역) # gather로 열들을 행으로 바꿈
cctv_graph<-ggplot(df_long,aes(x=지역,y=count,fill=year))+geom_bar(stat="identity") 
+ylab("CCTV수")+ggtitle("연도와지역별 CCTV 수")+theme(axis.text.x=element_text(angle=90, hjust=1))
cctv_graph

#위도 경도 전체 지도

register_google(key='AIzaSyB58hpbBeFizjTpohjL4wnen_1KfZz_Vr8') # 부여받은 키 등록

cctv_map <- cctv_year_df2 
cctv_lat_lot <- cctv_year[,c(10,11)] ## cctv 위도, 경도를 뽑음 
cctv_map2 <- bind_cols(cctv_map,cctv_lat_lot) # 합침
cctv_map2$위도 <-as.double(cctv_map2$위도) # 위도 경도를 실수화
cctv_map2$경도 <-as.double(cctv_map2$경도)
cctv_map2 <- cctv_map2[cctv_map2$경도<=130,] 
#결측치 확인(제거)
table(is.na(cctv_map2$year))
replace(cctv_map2$year,cctv_map2$year=="",NA)->cctv_map2$year
cctv_map4 <- cctv_map2 %>% filter(!is.na(cctv_map2$year))
cctv_map4
#1905,2026 제거
cctv_map5 <- cctv_map4[!(cctv_map4$year=="1905" | cctv_map4$year=="2026"),]
cctv_map5
map1 =get_map("daegu",zoom=7,maptype = 'watercolor')
map1
#지도 플로팅
map2 = ggmap(map1)
map2

#지도에 포인트 추가 
map3=map2 + geom_point(aes(x=경도,y=위도,colour=year),data=cctv_map5)
map3

#qmplot(경도,위도,data=cctv_map2,maptype = "toner-lite",color=year)

#####################################################################################

#2 연도별 범죄율 Hwang In seong
#우선 모든 범죄가 CCTV의 영향을 받지 않기때문에 CCTV의 영향을 받는 범죄인
#강력 범죄, 절도, 폭력범죄, 교통 범죄만을 뽑아서 본다.
#1. 2017년도,2016년도,2015년도, 2014년도 




####################################################################################

#3 연도별 검거율 Lee Jeong Chan
# 마찬가지로 CCTV와 관련있는 범죄들 위주로 한다. 꺽쇄그래프들 그리기
# x축: 년도 y축: 검거율 선 그래프 각각: 죄(강력범죄,살인기수,살인미수등,준강도,특수강도,인질강도)
Arrest_rate <- read.csv("년도별검거율.csv",header=T) #불러오기

names(Arrest_rate) <- c('죄','항목','단위','2011년','2012년','2013년','2014년','2015년','2016년','2017년') #컬럼명 다시 지정

crime_rate <- subset(Arrest_rate,Arrest_rate$죄=='총계'|Arrest_rate$죄=='강력범죄'|Arrest_rate$죄=='살인기수'|
                       Arrest_rate$죄=='살인미수등'|Arrest_rate$죄=='준강도'|Arrest_rate$죄=='특수강도'|Arrest_rate$죄=='인질강도') #조건으로 뽑기

crime_rate2 <- subset(crime_rate,crime_rate$항목=='검거율') #검거율만 뽑아서 넣기


#총계 수정...
modarrestRate<- edit(crime_rate2)
#총계 수정한 것 백업 시키기
write.csv(modarrestRate,"modarrestRate.csv",row.names = T)
#modarrestRate2 로 새롭게 백업을 불러옴
modarrestRate2 <- read.csv("modarrestRate.csv",header=T)
#moarrestRate2의 열 이름을 재지정
names(modarrestRate2) <- c('죄','항목','단위','2011년','2012년','2013년','2014년','2015년','2016년','2017년')

arrest_rate <- gather(modarrestRate2,year,rate,-죄,-항목,-단위) #gater로 년도 열들을 행으로 교체

#각 레벨화 시키기 (순서) => 그래프그릴때 필요함.
arrest_rate$죄 <-factor(arrest_rate$죄,levels=c("총계","강력범죄","살인기수","살인미수등","준강도","특수강도","인질강도"),ordered=TRUE)
arrest_rate$year <-factor(arrest_rate$year,levels=c('2011년','2012년','2013년','2014년','2015년','2016년','2017년'),ordered=TRUE)
#각 수치형으로 변환시켜주기(그래프를 그려야함)
arrest_rate$rate <-as.numeric(arrest_rate$rate)

#분포 그래프 확인
arrest_distri_graph <- ggplot(arrest_rate,aes(x=year,y=rate,colour=죄))+geom_point()+theme(text=element_text(family = '나눔명조'))
arrest_distri_graph

#선형 그래프 그룹별로 그려주기 
arrest_graph <- ggplot(arrest_rate,aes(x=year,y=rate,colour=죄))+
  geom_point()+geom_line(aes(group=죄),size=1)+theme(text=element_text(family="나눔명조")) +ggtitle("연도별 검거율")+xlab("year(2011~2017)") +ylab("검거율")+ylim(min(arrest_rate$rate),max(arrest_rate$rate))

arrest_graph
x11()

####################################################################################
#추가: 어린이 CCTV 보호구역
#분석결과 강원도 지역에 빈약한 것으로 파악

library(RColorBrewer)
#불러오기 
school_cctv <- read.csv("after_school.csv",header = T)
#소재지주소 문자화
school_cctv$소재지주소 <-as.character(school_cctv$소재지주소)
#tㅗ재지주소에서 공백 기준으로 나눠서 열로 만들어줌
school_cctv_local_split <- data.frame(do.call('rbind',strsplit(school_cctv$소재지주소,split=" ",fixed=TRUE)))
#데이터프레임 3열 17열 18열을 따로 뽑음
school_cctv_lon_lat<-school_cctv[,c(3,17,18)]
school_cctv_lon_lat
#각각 학교명, 위도 , 경도를 문자화 실수화 시킴
school_cctv_lon_lat$학교명<- as.character(school_cctv_lon_lat$학교명)
school_cctv_lon_lat$위도 <-as.double(school_cctv_lon_lat$위도)
school_cctv_lon_lat$경도 <-as.double(school_cctv_lon_lat$경도)
#학교명, 위도, 경도, 지역 프레임 생성
school_lon_lat_addr <- data.frame(학교명=school_cctv_lon_lat$학교명,위도=school_cctv_lon_lat$위도,경도=school_cctv_lon_lat$경도,지역=school_cctv_local_split$X1 )
school_lon_lat_addr
#지도 플로팅해서 그려주기
school_map1 <-get_map("daegu",zoom=7,maptype = 'watercolor')
school_map2 <-ggmap(school_map1)
school_map2
school_map3 <-school_map2 + ggtitle("학교CCTV보호구역위치")+geom_point(aes(x=경도,y=위도,colour=지역),data=school_lon_lat_addr)
school_map3



sc_map<-qmplot(경도,위도,data=school_lon_lat_addr,maptype = "toner-lite",color=지역)+ggtitle("학교CCTV보호구역위치")
+stat_density_2d(aes(fill=stat(level)),geom="polygon",alpha=.4,bins=15)+scale_fill_gradientn(colors=brewer.pal(7,"YlOrRd"))
sc_map
sc_map2 <- sc_map + scale_fill_gradient(low='yellow', high='red', guide=F) + scale_alpha(range=c(0.02, 0.8), guide=F)
sc_map2

#각 지역별 초등학교 cctv 개수 비교적 작은 지역 뽑을 예정 
gyeongido_school_cnt<-length(which(school_lon_lat_addr$지역=="경기도"))

gangwondo_school_cnt<-length(which(school_lon_lat_addr$지역=="강원도"))

gyeongsangnam_school_cnt<-length(which(school_lon_lat_addr$지역=="경상남도"))

gyeongsangbuk_school_cnt<-length(which(school_lon_lat_addr$지역=="경상북도"))

gwangju_school_cnt<-length(which(school_lon_lat_addr$지역=="광주광역시"))

daegu_school_cnt<-length(which(school_lon_lat_addr$지역=="대구광역시"))

daejeon_school_cnt<-length(which(school_lon_lat_addr$지역=="대전광역시"))

busan_school_cnt<-length(which(school_lon_lat_addr$지역=="부산광역시"))

seoul_school_cnt<-length(which(school_lon_lat_addr$지역=="서울특별시"))

sejong_school_cnt<-length(which(school_lon_lat_addr$지역=="세종특별자치시"))

ulsan_school_cnt<-length(which(school_lon_lat_addr$지역=="울산광역시"))

incheon_school_cnt<-length(which(school_lon_lat_addr$지역=="인천광역시"))

junnam_school_cnt<-length(which(school_lon_lat_addr$지역=="전라남도"))

junbuk_school_cnt<-length(which(school_lon_lat_addr$지역=="전라북도"))

jeju_school_cnt<-length(which(school_lon_lat_addr$지역=="제주특별자치도"))

chungnam_school_cnt<-length(which(school_lon_lat_addr$지역=="충청남도"))

chungbuk_school_cnt<-length(which(school_lon_lat_addr$지역=="충청북도"))

school_addr <- c("강원도","경기도","경상남도","경상북도","광주광역시","대구광역시","대전광역시",
                 "부산광역시","서울특별시","세종특별자치시",
                 "울산광역시","인천광역시","전라남도","전라북도","제주특별자치도","충청남도","충청북도")


school_cctv_cnt <-c(gangwondo_school_cnt,gyeongido_school_cnt,gyeongsangnam_school_cnt,gyeongsangbuk_school_cnt,gwangju_school_cnt,daegu_school_cnt
                    ,daejeon_school_cnt,busan_school_cnt,seoul_school_cnt,sejong_school_cnt,ulsan_school_cnt,incheon_school_cnt,junnam_school_cnt,junbuk_school_cnt,jeju_school_cnt
                    ,chungnam_school_cnt,chungbuk_school_cnt)
school_cctv_cnt <-as.numeric(school_cctv_cnt)



#학교 cctv 개수 지역별 데이터프레임 (정렬 사용 원형그래프 정렬 위해)
school_cctv_cnt_df <-data.frame(지역=school_addr,개수=school_cctv_cnt)

school_cctv_cnt_df_order <- school_cctv_cnt_df[order(school_cctv_cnt_df$개수),]

#퍼센트 표시를 위해 데이터프레임에 추가하기 위함
school_percent <- round(school_cctv_cnt_df_order$개수/sum(school_cctv_cnt_df_order$개수)*100)

#지역이름 뒤에 % 붙이기

school_cctv_cnt_df_order$지역 <- paste(school_cctv_cnt_df_order$지역,school_percent)
school_cctv_cnt_df_order$지역 <- paste(school_cctv_cnt_df_order$지역,"%",sep="")
install.packages("plotrix")
library(plotrix)


#pie 원형그래프 그려주기
pie2d <-pie(school_cctv_cnt_df_order$개수, labels = school_cctv_cnt_df_order$지역, main = "school cctv pie chart")
pie3d <-pie3D(school_cctv_cnt_df_order$개수, labels = school_cctv_cnt_df_order$지역, explode = 0.1, main = "school cctv pie chart")



###################################################################################

#추가: 워드클라우드로 뉴스기사 수집 후 방향성 제시
library("KoNLP")
library("wordcloud")

cctv_news <- file("cctv_news2.txt",blocking=F)

txtLines <- readLines(cctv_news)

nouns <- sapply(txtLines,extractNoun,USE.NAMES = F) #행렬 변환
head(unlist(nouns),20)

write(unlist(nouns),"news_data_backup2.txt")
revised <-read.table("news_data_backup2.txt")

nrow(revised)
wordcount <-table(revised)
length(wordcount)
head(sort(wordcount,decreasing = T),10)

pal <- brewer.pal(9,"Set1")
x11()
cctv_wordcloud<-wordcloud(names(wordcount),freq=wordcount,scale=c(5,1),rot.per=0.25,min.freq = 1,random.order = F,random.color = T,colors=pal)
cctv_wordcloud
################################################################################

#추가 아동성범죄 관련 기사 워드클라우드
library("KoNLP")
library("wordcloud")

child_news <- file("child_crime_news.txt",blocking=F)

txtLines2 <- readLines(child_news)

nouns2 <- sapply(txtLines2,extractNoun,USE.NAMES = F) #행렬 변환
head(unlist(nouns2),20)

write(unlist(nouns2),"child_news_data_backup2.txt")
revised2 <-read.table("child_news_data_backup2.txt")

nrow(revised2)
wordcount2 <-table(revised2)
length(wordcount2)
head(sort(wordcount2,decreasing = T),10)

pal <- brewer.pal(9,"Set1")
x11()
child_wordcloud<-wordcloud(names(wordcount2),freq=wordcount2,scale=c(5,1),rot.per=0.25,min.freq = 1,random.order = F,random.color = T,colors=pal)
child_wordcloud

