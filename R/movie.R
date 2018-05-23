#######################################################
####### 일자 : 2017년 12월 19일                 #######
####### 내용 : 사용한 패키지                    #######
#######################################################

install.packages("jsonlite")
install.packages("curl")
install.packages("plyr")
install.packages("dplyr")
install.packages("stringr")
install.packages("rvest")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("corrplot")
install.packages("ggthemes")
install.packages("randomForest")
install.packages("reshape")
install.packages("xgboost")

library(jsonlite)
library(curl)
library(dplyr)
library(plyr)
library(stringr)
library(rvest)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(ggthemes)
library(randomForest)
library(reshape)
library(xgboost)

#### Multiple plot function ####
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#######################################################
####### 일자 : 2017년 12월 19일                 #######
####### 내용 : OPEN API활용 데이터 수집         #######
#######################################################

key <- "6d0b3e8f80316cb4830be6dc5ac7bee6"
key <- "12f4366e429626ac9f8a2dae510db28b"

url <- paste0("http://www.kobis.or.kr/kobisopenapi/webservice/rest/boxoffice/searchDailyBoxOfficeList.json?key=",key,"&targetDt=20140101")
json <- fromJSON(url)
json

startdate <- as.Date("2014-01-01")
enddate <- as.Date("2017-12-31")
date <- seq(startdate, enddate, by="1 days")
date <- format(date, format = "%Y%m%d")
rawdata <- NULL
for(i in 1:length(date)){
  
  url <- paste0("http://www.kobis.or.kr/kobisopenapi/webservice/rest/boxoffice/searchDailyBoxOfficeList.json?key="
                ,key,"&targetDt=",date[i])
  tmp <- fromJSON(url)
  rawdata <- rbind(rawdata, tmp$boxOfficeResult$dailyBoxOfficeList)
  cat("수집완료-",i,"번째 : ",date[i],"\n")
  
}

date <- rep(date, each=10)
data <- cbind(rawdata, date)

movieCd <- unique(data$movieCd)
mvdata <- NULL
for(i in 1:length(movieCd)){
  url <- paste0("http://www.kobis.or.kr/kobisopenapi/webservice/rest/movie/searchMovieInfo.json?key="
                ,key,"&movieCd=",movieCd[i])
  tmp <- fromJSON(url)
  mnname <- tmp$movieInfoResult$movieInfo$movieNm
  mvcode <- tmp$movieInfoResult$movieInfo$movieCd
  actor <- tryCatch(tmp$movieInfoResult$movieInfo$actors$peopleNm[c(1,2,3,4)], error=function(e){})
  gen <- tmp$movieInfoResult$movieInfo$genres$genreNm
  nations <- tmp$movieInfoResult$movieInfo$nations$nationNm
  watchGrade <- tmp$movieInfoResult$movieInfo$audits$watchGradeNm
  direct <- tmp$movieInfoResult$movieInfo$directors$peopleNm
  company <- tryCatch(tmp$movieInfoResult$movieInfo$companys
                      [which(tmp$movieInfoResult$movieInfo$companys$companyPartNm == '배급사'),]$companyNm,
                      error=function(e){})
  mvset <- cbind(list(mnname), list(mvcode), list(actor), list(gen), list(direct), list(company), list(nations), list(watchGrade))
  mvdata <- rbind(mvdata,mvset)
  cat("수집완료 -",i,"번째 :",mnname,"\n")
}
mvdata <- as.data.frame(mvdata)
colnames(mvdata) <- c("movieNm","movieCd","actor","genre","direct","company", "nations", "watchGrade")

mvdata[,"movieCd"] <- sapply(mvdata[,"movieCd"], function(x){paste(x, collapse = ",")})
mvdata[,"movieNm"] <- sapply(mvdata[,"movieNm"], function(x){paste(x, collapse = ",")})
mvdata[,"actor"] <- sapply(mvdata[,"actor"], function(x){paste(x, collapse = ",")})
mvdata[,"genre"] <- sapply(mvdata[,"genre"], function(x){paste(x, collapse = ",")})
mvdata[,"direct"] <- sapply(mvdata[,"direct"], function(x){paste(x, collapse = ",")})
mvdata[,"company"] <- sapply(mvdata[,"company"], function(x){paste(x, collapse = ",")})
mvdata[,"nations"] <- sapply(mvdata[,"nations"], function(x){paste(x, collapse = ",")})
mvdata[,"watchGrade"] <- sapply(mvdata[,"watchGrade"], function(x){paste(x, collapse = ",")})
data <- merge(data, mvdata, by = 'movieCd')
data <- rename(data, c(movieNm.x = "movieNm"))

#######################################################
####### 일자 : 2017년 12월 19일                 #######
####### 내용 : 네이버 영화 보고싶어요 크롤링    #######
#######################################################

# #### 해당 영역은 네이버 OPEN API를 활용한 크롤링(한글 깨짐) ####
# 
# Client_Id<-"mXOh6M_J3B21pA5hBpl_"
# Client_Secret<-"wiMKeAKx0_"
# 
# url<-"https://openapi.naver.com/v1/search/movie.xml?query="
# movieNm<-unique(data$movieNm)
# tmp<-GET(str_c(url,URLencode(movieNm[i])),
#          add_headers("X-Naver-Client-Id" = Client_Id,"X-Naver-Client-Secret"=Client_Secret))
# xml<-xmlParse(tmp,encoding="UTF-8")

#### 네이버 영화별 코드 크롤링하기 ####

url1<-"http://movie.naver.com/movie/search/result.nhn?query="
url2<-"&section=all&ie=utf8"
movieNm<-unique(data$movieNm)
codes<-NULL
for(i in 1:length(movieNm)){
  
  url<-paste(url1,URLencode(movieNm[i]),url2,sep="")
  txt<-readLines(url)
  
  tmp<-txt[which(str_detect(txt,"class=\"result_thumb\""))+1]
  code<-str_sub(tmp,str_locate(tmp, "code=")[,2]+1,str_locate(tmp, "\"><img")[,1]-1)
  
  tmp1<-txt[which(str_detect(txt,"cuser_cnt"))]
  join<-str_sub(tmp1,str_locate(tmp1,"참여")[,2]+2,str_locate(tmp1,"명")[,1]-1)
  
  tryCatch(codes[i]<-code[which.max(join)], error=function(e){})
  cat(movieNm[i],": 완료\n")
  
}

# #### 첫번째 코드 실행 이유 : UTF-8 타입이 아님 ####
# movieNm<-"용의자"
# URLencode(iconv(movieNm,to="UTF-8"))
# #### 두번째 코드 NA 이유 : 변수에서 추출한 문자열이 이미 UTF-8 타입 ####
# mvnm<-unique(data$movieNm)[1]
# URLencode(iconv(movieNm,to="UTF-8"))
# URLencode(movieNm)


#### 네이버 영화별 보고싶어요 지수 크롤링하기 ####

url1<-"http://movie.naver.com/movie/bi/mi/point.nhn?code="
url2<-as.numeric(codes)
like <- NULL
for(i in 1:length(codes)){
  
  url<-paste0(url1,url2[i],sep="")
  txt<-readLines(url, encoding="UTF-8")
  tmp<-txt[which(str_detect(txt,"exp_info"))+2]
  tryCatch(like[i]<-str_sub(tmp,str_locate(tmp,"보고싶어요</em>")[,2]+1,str_length(tmp)-7),
           error=function(e){})
  cat(movieNm[i], ": 완료\n")
  
}

mvcd<-cbind(movieNm,codes)
mvlike <- cbind(mvcd,like)
data <- merge(data,mvlike, by="movieNm") 

#######################################################
####### 일자 : 2017년 12월 19일                 #######
####### 내용 : 배우,감독 스코어링               #######
#######################################################


t.data <- ddply(data,.(movieNm,actor,direct),summarise,audiAcc=max(as.numeric(audiAcc)))
tmp<-NULL
for(i in 1:nrow(t.data)){
  tmp_1<-t.data$audiAcc[i]
  tmp_2<-unlist(str_split(t.data$actor[i],","))[c(1,2)]
  tmp_3<-merge(tmp_2,tmp_1)
  tmp<-rbind(tmp,tmp_3)
}

tmp_actor<-ddply(tmp,.(x),summarise,sum_Acc=sum(y))
actor<-na.omit(tmp_actor)
actor$x<-as.character(actor$x)
actor<-filter(actor,x!="")

for(i in 1:nrow(data)){
  tmp_1<-filter(actor,actor$x==str_split(data$actor,",")[[i]][1])$sum_Acc
  tmp_2<-filter(actor,actor$x==str_split(data$actor,",")[[i]][2])$sum_Acc
  tryCatch(data$actorscore[i]<-tmp_1+tmp_2, error=function(e){})
}

tmp<-NULL
for(i in 1:nrow(t.data)){
  tmp_1<-t.data$audiAcc[i]
  tmp_2<-unlist(str_split(t.data$direct[i],","))[c(1,2)]
  tmp_3<-merge(tmp_2,tmp_1)
  tmp<-rbind(tmp,tmp_3)
}

tmp_director<-ddply(tmp,.(x),summarise,sum_Acc=sum(y))
director<-na.omit(tmp_director)
director<-filter(director, x!="")
director$x<-as.character(director$x)

for(i in 1:nrow(data)){
  tmp_1<-filter(director,director$x==str_split(data$direct,",")[[i]][1])$sum_Acc
  tryCatch(data$directorscore[i]<-tmp_1, error=function(e){})
}

#######################################################
####### 일자 : 2017년 12월 19일                 #######
####### 내용 : 장르, 배급사, 관람등급 정제      #######
#######################################################

for(i in 1:nrow(data)){
  
  data$distributor[i] <- str_split(data$company, ",")[[i]][1]
  data$gen[i] <- str_split(data$genre, ",")[[i]][1]
  data$nation[i] <- str_split(data$nations, ",")[[i]][1]
  data$Grade[i] <- str_split(data$watchGrade, ",")[[i]][1]
  cat(nrow(data),"중",i,"번째 진행중\n")
  
}

rawdata <- data
data <- rawdata

#######################################################
####### 일자 : 2017년 12월 19일                 #######
####### 내용 : 자료 정제                        #######
#######################################################

#### 날짜에 해당하는 요일 변수 입력하기 ####

data<-data[c(order(data$date)),]
rownames(data) <- NULL
data$weekdays <- as.factor(weekdays(as.Date(data$date, format = "%Y%m%d")))
data$date <- as.Date(data$date, format = "%Y%m%d")

#### 황금연휴 ####

# 2014년 : 1.1(수), 1.30(목),1.31(금), 2.1(토), 2.2(일), 3.1(토), 5.3(토), 5.4(일),  5.5(월), 5.6(화), 6.4(수), 6.5(목), 6.6(금), 6.7(토), 6.8(일), 8.15(금), 9.7(일), 9.8(월), 9.9(화), 9.10(수), 10.3(금), 10.9(목), 12.25(목)
# 
# 2015년 : 1.1(목), 2.18(수), 2.19(목), 2.20(금), 2.21(토), 2.22(일), 3.1(토), 5.5(수), 5.25(월), 6.6(토), 8.14(금), 8.15(토), 9.26(토), 9.27(일), 9.28(월), 9.29(화), 10.3(토), 10.9(금), 12.25(금)
# 
# 2016년 : 1.1(금), 2.7(일), 2.8(월), 2.9(화), 2.10(수), 3.1(화), 4.13(수), 5.5(목), 5.14(토), 6.6(월), 8.15(월), 9.14(수), 9.15(목), 9.16(금), 10.3(월), 10.9(일), 12.25(일)
# 
# 2017년 : 1.1(일), 1.27(금), 1.28(토), 1.29(일), 1.30(월), 3.1(수), 5.3(수), 5.5(금), 5.9(화), 6.6(화), 8.15(화), 10.2(월), 10.3(화), 10.4(수), 10.5(목), 10.6(금), 10.9(월), 12.25(월)


holiday <- as.Date(c("2014-01-01","2014-01-30","2014-01-31","2014-02-01","2014-02-02","2014-03-01","2014-05-01","2014-05-03","2014-05-04","2014-05-05","2014-05-06","2014-06-04","2014-06-06","2014-06-07","2014-06-08","2014-08-15","2014-09-06","2014-09-07","2014-09-08","2014-09-09","2014-09-10","2014-10-03","2014-10-09","2014-12-25",
                     "2015-01-01","2015-02-18","2015-02-19","2015-02-20","2015-02-21","2015-02-22","2015-03-01","2015-05-01","2015-05-02","2015-05-03","2015-05-04","2015-05-05","2015-05-25","2015-06-06","2015-08-15","2015-09-26","2015-09-27","2015-09-28","2015-09-29","2015-10-03","2015-10-09","2015-12-25",
                     "2016-01-01","2016-02-06","2016-02-07","2016-02-08","2016-02-09","2016-02-10","2016-03-01","2016-04-13","2016-05-05","2016-05-06","2016-05-07","2016-05-08","2016-05-14","2016-06-06","2016-08-15","2016-09-14","2016-09-15","2016-09-16","2016-09-17","2016-09-18","2016-10-03","2016-10-09","2016-12-25",
                     "2017-01-01","2017-01-27","2017-01-28","2017-01-29","2017-01-30","2017-03-01","2017-05-01","2017-05-03","2017-05-05","2017-05-06","2017-05-07","2017-05-09","2017-06-06","2017-08-15","2017-09-30","2017-10-01","2017-10-02","2017-10-03","2017-10-04","2017-10-05","2017-10-06","2017-10-07","2017-10-08","2017-10-09","2017-12-25"))

data$holiday<- ifelse(data$date %in% holiday, "Y", "N")

a <- data[which(data$date %in% as.Date(c("2017-04-29","2017-04-30","2017-05-01","2017-05-02","2017-05-03","2017-05-04","2017-05-05","2017-05-06","2017-05-07","2017-05-08","2017-05-09","2017-05-10"))),]
b <- data[which(data$date %in% as.Date(c("2014-06-02","2014-06-03","2014-06-04","2014-06-05","2014-06-06","2014-06-07","2014-06-08"))),]
c <- data[which(data$date %in% as.Date(c("2014-09-05","2014-09-06","2014-09-07","2014-09-08","2014-09-09","2014-09-10"))),]

#### 시사회로 집계된것 제거

data$openDt <- as.Date(data$openDt)
data <- arrange(data, movieNm)
data <- filter(data, ddply(data, .(movieNm), summarise, preview = openDt <= date)$preview==T)




data <- data[which(as.Date(data$openDt) >= as.Date("2014-01-01")),] # 2014년 이전 개봉영화 제거
tmp <- count(data$movieNm)
tmp <- tmp[which(count(data$movieNm)$freq >= 7),]$x
data <- data[which(data$movieNm %in% tmp == T),] # 개봉후 상영일수 7일 미만 영화 제거



#### 개봉 후 며칠이 지났는지 확인하는 변수

data <- data[order(data$movieNm, decreasing = F),]
data <- ddply(data, .(movieNm), mutate, days=order(movieNm, decreasing = F))

#### 개봉후 1주차 2주차 3주차 4주차 5주차이상 5개범주로 나누기

data$weeks <- ifelse(data$days <=7, 1,
                     ifelse(data$days <=14, 2,
                            ifelse(data$days <=21, 3,
                                   ifelse(data$days <=28, 4, 5))))

#### Like 변수 시간에 따라 감소 시키기

data[which(is.na(data$like)),]
# data <- data[-which(is.na(data$like)),]
data$like <- as.numeric(gsub(",","",as.character(data$like)))

data <- ddply(data, .(movieNm), mutate, desc_days_like = like/(days*weeks), desc_weeks_like = like/weeks)




str(data)

data$audiAcc <-  as.numeric(data$audiAcc)
data$audiCnt <-  as.numeric(data$audiCnt)
data$distributor <- gsub("\\(주\\)","", data$distributor)
data$distributor <- gsub("\\(유\\)","", data$distributor)
data$distributor <- gsub("\\(재\\)","", data$distributor)
data$distributor <- gsub("\\(NEW\\)","", data$distributor)
data$distributor <- gsub("[A-Za-z]","", data$distributor)
data$distributor <- gsub("\\&","", data$distributor)
data$distributor <- gsub("㈜","", data$distributor)
data$distributor <- gsub(" ","", data$distributor)
data$distributor <- as.factor(data$distributor)
data$gen <- gsub("\\(호러\\)","", data$gen)
data$gen <- gsub("/로맨스","", data$gen)
data$gen <- as.factor(data$gen)
data$nation <- as.factor(data$nation)
data$Grade <- as.factor(data$Grade)
data$holiday <-  as.factor(data$holiday)


datamart <- select(data, "movieNm","date","weekdays","holiday","weeks","days","audiCnt","audiAcc",
                   "actor","direct","actorscore","directorscore","like","desc_days_like","desc_weeks_like","distributor","gen","nation","Grade")


#### 탐색적 자료분석 ####

tmp <- ddply(datamart,.(movieNm),summarise,audiAcc=max(as.numeric(audiAcc)))

ggplot(data = tmp, mapping = aes(x=audiAcc)) +
  geom_histogram(binwidth = 100000) +
  scale_x_continuous(breaks= seq(0, 20000000, by=2500000)) +
  scale_y_continuous(breaks= seq(0, 250, by=25))

## 추석, 설날 영화 관객수 추이 살펴보기

tmp_2014.2 <- data[which(data$date %in% as.Date(c("2014-01-30","2014-01-31","2014-02-01"))),] #2014년 설날
tmp_2014.5 <- data[which(data$date %in% as.Date(c("2014-05-01","2014-05-02","2014-05-03","2014-05-04","2014-05-05","2014-05-06"))),] #2014년 5월
tmp_2014.9 <- data[which(data$date %in% as.Date(c("2014-09-07","2014-09-08","2014-09-09","2014-09-10"))),] #2014년 추석
tmp_2014.12 <- data[which(data$date %in% as.Date(c("2014-12-23","2014-12-24","2014-12-25","2014-12-26"))),] #2014년 크리스마스

tmp_2015.2 <- data[which(data$date %in% as.Date(c("2015-02-18","2015-02-19","2015-02-20","2015-02-21","2015-02-22"))),] #2015년 설날
tmp_2015.5 <- data[which(data$date %in% as.Date(c("2015-05-01","2015-05-02","2015-05-03","2015-05-04","2015-05-05"))),] #2015년 5월
tmp_2015.9 <- data[which(data$date %in% as.Date(c("2015-09-26","2015-09-27","2015-09-28","2015-09-29","2015-09-30"))),] #2015년 추석
tmp_2015.12 <- data[which(data$date %in% as.Date(c("2015-12-23","2015-12-24","2015-12-25","2015-12-26"))),] #2014년 크리스마스

tmp_2016.2 <- data[which(data$date %in% as.Date(c("2016-02-06","2016-02-07","2016-02-08","2016-02-10"))),] #2016년 설날
tmp_2016.5 <- data[which(data$date %in% as.Date(c("2016-05-05","2016-05-06","2016-05-07","2016-05-08"))),] #2016년 5월
tmp_2016.9 <- data[which(data$date %in% as.Date(c("2016-09-14","2016-09-15","2016-09-16","2016-09-17","2016-09-18"))),] #2016년 추석
tmp_2016.12 <- data[which(data$date %in% as.Date(c("2016-12-23","2016-12-24","2016-12-25","2016-12-26"))),] #2016년 크리스마스

tmp_2017.2 <- data[which(data$date %in% as.Date(c("2017-01-27","2017-01-28","2017-01-29","2017-01-30"))),] #2017년 설날
tmp_2017.5 <- data[which(data$date %in% as.Date(c("2017-05-03","2017-05-04","2017-05-05","2017-05-06","2017-05-07","2017-05-08","2017-05-09"))),] #2017년 5월
tmp_2017.9 <- data[which(data$date %in% as.Date(c("2017-09-30","2017-10-01","2017-10-02","2017-10-03","2017-10-04","2017-10-05","2017-10-06","2017-10-07","2017-10-08","2017-10-09"))),] #2017년 추석
tmp_2017.12 <- data[which(data$date %in% as.Date(c("2017-12-23","2017-12-24","2017-12-25","2017-12-26"))),] #2017년 크리스마스

tmp <- list(
  data[which(data$movieNm %in% tmp_2014.2$movieNm),],
  data[which(data$movieNm %in% tmp_2014.5$movieNm),],
  data[which(data$movieNm %in% tmp_2014.9$movieNm),],
  data[which(data$movieNm %in% tmp_2014.12$movieNm),],
  data[which(data$movieNm %in% tmp_2015.2$movieNm),],
  data[which(data$movieNm %in% tmp_2015.5$movieNm),],
  data[which(data$movieNm %in% tmp_2015.9$movieNm),],
  data[which(data$movieNm %in% tmp_2015.12$movieNm),],
  data[which(data$movieNm %in% tmp_2016.2$movieNm),],
  data[which(data$movieNm %in% tmp_2016.5$movieNm),],
  data[which(data$movieNm %in% tmp_2016.9$movieNm),],
  data[which(data$movieNm %in% tmp_2016.12$movieNm),],
  data[which(data$movieNm %in% tmp_2017.2$movieNm),],
  data[which(data$movieNm %in% tmp_2017.5$movieNm),],
  data[which(data$movieNm %in% tmp_2017.9$movieNm),],
  data[which(data$movieNm %in% tmp_2017.12$movieNm),]
)


tmp_plots <- list()
for(i in 1:4){
  tmp_plots[[i]] <- ggplot(tmp[[4*i-3]], mapping = aes(x=as.character(date), y=audiCnt, group=movieNm, color=movieNm)) +
    geom_line(size=1) + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none") +
    scale_y_continuous(breaks = seq(0, 400000, by=100000))
}
multiplot(plotlist = tmp_plots, cols=1)
grid.arrange(tmp_plots[[1]],tmp_plots[[2]], ncol=1)
grid.draw(rbind(ggplotGrob(tmp_plots[[1]]),ggplotGrob(tmp_plots[[2]]),ggplotGrob(tmp_plots[[3]]),ggplotGrob(tmp_plots[[4]]), size="last"))


tmp_1 <- ddply(datamart, .(movieNm), summarise, ratio = max(audiAcc[max(which(weeks==1))])/max(audiAcc))
tmp_2 <- ddply(datamart, .(movieNm), summarise, ratio = max(audiAcc[max(which(weeks==2))])/max(audiAcc))
tmp_3 <- ddply(datamart, .(movieNm), summarise, ratio = max(audiAcc[max(which(weeks==3))])/max(audiAcc))

plots <- list()
p1 <- ggplot(data = tmp_1, mapping = aes(x = ratio)) + geom_histogram()
p2 <- ggplot(data = tmp_2, mapping = aes(x = ratio)) + geom_histogram()
p3 <- ggplot(data = tmp_3, mapping = aes(x = ratio)) + geom_histogram()

multiplot(p1, p2, p3, cols = 3)


datamart <- datamart[which(datamart$weeks <= 2),]


### 모델링

options(scipen = 100, digits = 0)


################ train, test, target three movie ##################

train_data <- datamart[which(datamart$date <= "2017-09-15"),]
test_data <- datamart[which(datamart$date > "2017-09-15"),]
tmp <- datamart[which(datamart$movieNm %in% c("킹스맨: 골든 서클","남한산성","넛잡 2")),]
test_data <- filter(tmp, date <= "2017-10-10")

####


tmp <- NULL

for(i in 1:30){
  t_movieNm <- sample(unique(train_data$movieNm),length(unique(train_data$movieNm))*0.7)
  t_index <- which(train_data$movieNm %in% t_movieNm)
  train <- train_data[t_index,]
  test <- train_data[-t_index,]
  
  #### LM #####
  
  fit.lm <- lm(audiCnt ~ actorscore + directorscore + weekdays + days + holiday + like, data = train)
  
  ####### RF ######
  
  fit.rf <- randomForest(audiCnt ~ actorscore + directorscore + weekdays + days + holiday + like + gen, data = train,
                         mtry=3, importance = T, ntree = 500)
  
  ############ xgboost ####
  
  v1 <- c("actorscore","directorscore","weekdays","days","holiday","like","gen")
  train.input <- data.matrix(train[,v1])
  train.output <- data.matrix(train$audiCnt)
  fit.xg <- xgboost(data        = train.input,
                    label       = train.output,
                    booster     = "gbtree",
                    eta         = 0.025, #gradient descent 알고리즘에서의 learning rate
                    depth       = 3,
                    nrounds     = 2500, #maximum number of iterations (steps) required for gradient descent to converge. (?)
                    objective   = "reg:linear",
                    eval_metric = "rmse", #회귀모델에서는 RMSE를 모델 accuracy 평가지표로 활용 
                    verbose = 0) # rmse 출력메시지 제거
  
  # ####### test data 검증
  # 
  # prediction.lm <- predict(fit.lm, newdata = test)
  # prediction.rf <- predict(fit.rf, newdata = test)
  # prediction.xg <- predict(fit.xg, newdata = data.matrix(test[,v1]))
  # 
  # test_table <- data.frame(movieNm = test$movieNm, date = test$date, weekdays = test$weekdays, holi = test$holiday, true = test$audiCnt, prediction.lm, prediction.rf, prediction.xg)
  # 
  # ######## 시각적으로 확인해보기
  # 
  # t_movieNm <- sample(unique(test_table$movieNm),4)
  # t_index <- which(test_table$movieNm %in% t_movieNm)
  # tmp <- test_table[t_index,]
  # 
  # p1 <- filter(tmp ,movieNm == unique(movieNm)[1])
  # ggplot(data = p1, mapping = aes(x = as.character(date), y = true)) + geom_line()
  # 
  # ggplot(data = tmp, mapping = aes(x = date, y = true, colour = movieNm)) + geom_line() + facet_grid(~movieNm)
  
  prediction.lm <- predict(fit.lm, newdata = test_data)
  prediction.rf <- predict(fit.rf, newdata = test_data)
  prediction.xg <- predict(fit.xg, newdata = data.matrix(test_data[,v1]))
  
  rst_table <- data.frame(movieNm=test_data$movieNm, date=test_data$date, true = test_data$audiCnt, prediction.lm, prediction.rf, prediction.xg)
  tmp <- rbind(tmp,rst_table)
  
  cat("Bootstrap :", i, "번째 진행중\n")
}

rst_days <- ddply(tmp, .(movieNm,date), summarise, audiCnt = mean(true), reg = mean(prediction.lm), rf = mean(prediction.rf), xg = mean(prediction.xg))
rst_total <- ddply(rst_days, .(movieNm), summarise, audiCnt = sum(audiCnt), reg = sum(reg), rf = sum(rf), xg = sum(xg))



# 해야할것
# 근로자의날, 징검다리, 3일연속연휴(주말 포함, 비포함), 4일연속연휴, 5일연속연휴, 그이상의연휴, 장기연휴의 마지막날도 연휴? 패턴찾기 --> 해당날짜에 Y or N






