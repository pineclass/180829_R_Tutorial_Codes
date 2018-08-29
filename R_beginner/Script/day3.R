library(foreign)
setwd("C:/Users/USER/Dropbox/R-work/2018-intermediate/R_beginner/data")

# https://www.data.go.kr/dataset/15004274/fileData.do
# 한국행정연구원 설문조사 데이터 현황 

data<-as.data.frame(read.spss("survey_data_2015.sav", reencode='utf-8'))
head(data)
dim(data)
names(data)

# Q1_1 : 업무량
# Q1_2 : 근무시간
# Q6 : 업무자율성

# SQ2 : 성별
# SQ3 : 소속기관
# SQ8 : 채용 방식
# SQ11 : 결혼 유무

# t검정의 예)
t.test(as.numeric(Q1_1)~SQ2, data=data)
t.test(as.numeric(Q1_2)~SQ2, data=data)

t.test(as.numeric(Q1_1)~SQ3, data=data)
t.test(as.numeric(Q1_2)~SQ3, data=data)

t.test(as.numeric(Q1_1)~SQ8, data=data[data$SQ8 %in% c("공개경쟁채용","경력경쟁채용"),])
t.test(as.numeric(Q1_2)~SQ8, data=data[data$SQ8 %in% c("공개경쟁채용","경력경쟁채용"),])

summary(data$SQ11)
t.test(as.numeric(Q1_1)~SQ11, data=data[data$SQ11 != "무응답",])
t.test(as.numeric(Q1_2)~SQ11, data=data[data$SQ11 != "무응답",])

data$Q6 <- (as.numeric(data$Q6_1)+as.numeric(data$Q6_2)+as.numeric(data$Q6_3)+as.numeric(data$Q6_4))/4
t.test(Q6~SQ2, data=data)
t.test(Q6~SQ3, data=data)
t.test(Q6~SQ8, data=data[data$SQ8 %in% c("공개경쟁채용","경력경쟁채용"),])
t.test(Q6~SQ11, data=data[data$SQ11 != "무응답",])


