if(!require(stringr)) install.packages("stringr");library(stringr)
if(!require(mapdata)) install.packages("mapdata");library(mapdata)
if(!require(maps)) install.packages("maps");library(maps)
if(!require(ggmap)) install.packages("ggmap");library(ggmap)
if(!require(gstat)) install.packages("gstat");library(gstat)
if(!require(sp)) install.packages("sp");library(sp)
if(!require(RCurl)) install.packages("RCurl");library(RCurl)
if(!require(e1071)) install.packages("e1071");library(e1071)
if(!require(randomForest)) install.packages("randomForest");library(randomForest)
if(!require(caret)) install.packages("caret");library(caret)

setwd("D:/2018_R_intermediate-master/data")
load(file="a.data1.rda") # 과거 해상사고 위험지수 부여
load(file="public_data2.rda") # 과거 해상사고 일자의 기상과, 관측소 위치


#기상관련, 기상비관련, 관측소 위치 지도#
a1.data <- a.data1[a.data1$type=="기타" & a.data1$weather != "양호" | a.data1$reason=="기상악화",]
a2.data <- a.data1[!(a.data1$type=="기타" & a.data1$weather != "양호" | a.data1$reason=="기상악화"),]

hdf <- get_map(location= "Southkorea", zoom=6, maptype="toner-lite", source="stamen")
a<-ggmap(hdf) + geom_point(data=a2.data, aes(a2.data$lon, a2.data$lat), shape= 10,colour="tan", size=1) 
a2<-a+geom_point(data=a1.data, aes(a1.data$lon, a1.data$lat), shape= 16,colour="yellow", size=1) 
a3<-a2+geom_point(data=buoy.stat2, aes(buoy.stat2$lon, buoy.stat2$lat), shape= 12,colour="blue", size=1)
a4<-a3+geom_point(data=light.stat2, aes(light.stat2$lon, light.stat2$lat), shape= 16,colour="purple", size=1)
a5<-a4+geom_point(data=wave.stat2, aes(wave.stat2$lon, wave.stat2$lat), shape= 10,colour="red1", size=1)
a5


###해양기상부이-실시간###
url1 <- paste0("http://www.weather.go.kr/mini/marine/inner_marine_buoy.jsp")
line1 <- readLines(url1, encoding = "euc-kr")   #HTML source code 불러오기
line2<-line1[132:148]

line2 <- gsub("</td>|\t", "", line2) 
line2 <- gsub("&nbsp;", "", line2) 

buoy.real <- matrix(rep(NA,17*12),ncol=12)

for(i in 1:17){
  list.locate<-str_locate_all(line2[i],"<td>")
  buoy.real[i,1] <- substr(line2[i],(list.locate[[1]][1,2]+1),(list.locate[[1]][2,1]-1))
  buoy.real[i,2] <- substr(line2[i],(list.locate[[1]][2,2]+1),(list.locate[[1]][3,1]-1))
  buoy.real[i,3] <- substr(line2[i],(list.locate[[1]][3,2]+1),(list.locate[[1]][4,1]-1))
  buoy.real[i,4] <- substr(line2[i],(list.locate[[1]][4,2]+1),(list.locate[[1]][5,1]-1))
  buoy.real[i,5] <- substr(line2[i],(list.locate[[1]][5,2]+1),(list.locate[[1]][6,1]-1))
  buoy.real[i,6] <- substr(line2[i],(list.locate[[1]][6,2]+1),(list.locate[[1]][7,1]-1))
  buoy.real[i,7] <- substr(line2[i],(list.locate[[1]][7,2]+1),(list.locate[[1]][8,1]-1))
  buoy.real[i,8] <- substr(line2[i],(list.locate[[1]][8,2]+1),(list.locate[[1]][9,1]-1))
  buoy.real[i,9] <- substr(line2[i],(list.locate[[1]][9,2]+1),(list.locate[[1]][10,1]-1))
  buoy.real[i,10] <- substr(line2[i],(list.locate[[1]][10,2]+1),(list.locate[[1]][11,1]-1))
  buoy.real[i,11] <- substr(line2[i],(list.locate[[1]][11,2]+1),(list.locate[[1]][12,1]-1))
  buoy.real[i,12] <- substr(line2[i],(list.locate[[1]][12,2]+1),(list.locate[[1]][13,1]-1))
}

buoy.real

###파고부이-실시간###
url1<-paste0("http://www.weather.go.kr/mini/marine/marine_buoy_cosmos.jsp")
raw = iconv(readLines(url1), from="EUC-KR", to='UTF-8')
location <- raw[which(str_detect(raw, "<td>"))]
raw2<-location[1:168]
raw2<-gsub("\t\t\t\t\t\t\t\t\t\t<td>", "", raw2) 
raw2<-gsub("</td>", "", raw2) 
raw2
d<-data.frame(seq(1,163,6),seq(6,168,6))

for(i in 1:nrow(d))
  assign(paste0('a',i),raw2[d[i,1]:d[i,2]])

raw2<-rbind(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28)
raw2

##############
####국내등표-실시간###
url1 <- paste0("http://www.weather.go.kr/mini/marine/marine_beacon.jsp?type=table&stn=0")
line1 <- readLines(url1, encoding = "euc-kr")   #HTML source code 불러오기
line2<-line1[742:which(str_detect(line1, 'scope="row"'))[2]-1]


line2 <- gsub("&nbsp;", "", line2) 
line2 <- gsub("</td>|\t", "", line2) 
line2 <- gsub(" ", "", line2)
line3 <- line2[which(str_detect(line2, "<td>|</tr>|<tr>"))]

line4 <- rep(NA,9)

line4[1] <- line3[1]
line4[2] <- paste0(line3[2],line3[3],line3[4],line3[5],line3[6],line3[7],line3[8],line3[9],line3[10],line3[11],line3[12],line3[13])
line4[3] <- paste0(line3[14],line3[15],line3[16],line3[17],line3[18],line3[19],line3[20],line3[21],line3[22],line3[23],line3[24],line3[25])
line4[4] <- line3[26]
line4[5] <- line3[27]
line4[6] <- line3[28]
line4[7] <- line3[29]
line4[8] <- line3[30]
line4[9] <- line3[31]



light.real <- matrix(rep(NA,9*10),ncol=10)
i=1
for(i in 1:9){
  list.locate<-str_locate_all(line4[i],"<td>")
  light.real[i,1] <- substr(line4[i],(list.locate[[1]][1,2]+1),(list.locate[[1]][2,1]-1))
  light.real[i,2] <- substr(line4[i],(list.locate[[1]][2,2]+1),(list.locate[[1]][3,1]-1))
  light.real[i,3] <- substr(line4[i],(list.locate[[1]][3,2]+1),(list.locate[[1]][4,1]-1))
  light.real[i,4] <- substr(line4[i],(list.locate[[1]][4,2]+1),(list.locate[[1]][5,1]-1))
  light.real[i,5] <- substr(line4[i],(list.locate[[1]][5,2]+1),(list.locate[[1]][6,1]-1))
  light.real[i,6] <- substr(line4[i],(list.locate[[1]][6,2]+1),(list.locate[[1]][7,1]-1))
  light.real[i,7] <- substr(line4[i],(list.locate[[1]][7,2]+1),(list.locate[[1]][8,1]-1))
  light.real[i,8] <- substr(line4[i],(list.locate[[1]][8,2]+1),(list.locate[[1]][9,1]-1))
  light.real[i,9] <- substr(line4[i],(list.locate[[1]][9,2]+1),(list.locate[[1]][10,1]-1))
  light.real[i,10] <- substr(line4[i],(list.locate[[1]][10,2]+1),nchar(line4[i])-5)
}

light.real

###관측소 위치####
buoy.stat <- read.csv("buyeo_stnInfo.csv",header=T)
mbuoy.stat2 <- buoy.stat[,c(4,6,7)]
names(mbuoy.stat2) <-c("region","lat","lon")

light.stat <- read.csv("lighthouse_stnInfo.csv",header=T)
mlight.stat2 <- light.stat[,c(4,6,7)]
names(mlight.stat2) <-c("region","lat","lon")

wave.stat <- read.csv("wave_stnInfo.csv",header=T)
wave.stat1 <- wave.stat[is.na(wave.stat$종료일),]
mwave.stat2 <- wave.stat1[,c(4,6,7)]
names(mwave.stat2) <-c("region","lat","lon")

###################

#실시간####
buoy.real
buoy.real2<-buoy.real[,c(-2,-5,-6,-11)]
colnames(buoy.real2)<-c("region","ws","gust", "temp", "SST", "max_wave", "sig_wave", "pri_wave")
buoy.real3<-as.data.frame(buoy.real2)

colnames(raw2)<-c("region","sig_wave","max_wave","mean_wave","pri_wave","SST")
raw3<-raw2[,-4]
raw4<-as.data.frame(raw3)
class(raw4)
raw4$ws <- raw4$gust <- raw4$temp <- NA
raw5 <- raw4[,c("region", "ws","gust","temp","SST","max_wave","sig_wave","pri_wave")]
wave.real3<-raw5

light.real
colnames(light.real) <- c("region", "wd", "ws", "temp", "hpa", "SST", "max_wave", "sig_wave", "pri_wave", "mm")
light.real2 <- light.real[,c(-2,-5,-10)]
light.real22 <- as.data.frame(light.real2)
light.real22$gust <- NA
class(light.real22)
light.real23 <- light.real22[,c("region", "ws","gust","temp","SST","max_wave","sig_wave","pri_wave")]
light.real3 <- light.real23


mbuoy.stat3<-mbuoy.stat2[which(mbuoy.stat2$region %in% buoy.real3$region),]
mwave.stat3<-mwave.stat2[which(mwave.stat2$region %in% wave.real3$region),]
mlight.stat3<-mlight.stat2[which(mlight.stat2$region %in% light.real3$region),]

dim(mwave.stat3)

#실시간 자료와 위치 합치기
data.b <- merge(mbuoy.stat3, buoy.real3, by="region")
data.w <- merge(mwave.stat3, wave.real3, by="region")
data.l <- merge(mlight.stat3, light.real3, by="region")

fdata <- rbind(data.b, data.w, data.l)
coordinates(fdata) <- ~lon+lat
##########

####격자 데이터###
lon.1 <- seq(124,132,0.2)
lat.1 <- seq(32,38.5,0.2)

lonlat<-expand.grid(lon.1,lat.1)
u <- map.where(database="worldHires", x=lonlat$Var1, y=lonlat$Var2)
a <- rep(0, nrow(lonlat))
a[u %in% c("South Korea","North Korea")] <- 1
lonlat2 <- lonlat[a==0,]
#plot(lonlat2, pch=16, col=rep(2:5, each=350))
colnames(lonlat2)<-c("lon","lat")
coordinates(lonlat2) <- ~lon+lat

#IDW로 실시간 기상예측#
lonlat2$ws<-idw(ws~1, fdata[complete.cases(fdata$ws),], newdata=lonlat2)$var1.pred
lonlat2$gust<-idw(gust~1, fdata[complete.cases(fdata$gust),], newdata=lonlat2)$var1.pred
lonlat2$temp<-idw(temp~1, fdata[complete.cases(fdata$temp),], newdata=lonlat2)$var1.pred
lonlat2$SST<-idw(SST~1, fdata[complete.cases(fdata$SST),], newdata=lonlat2)$var1.pred
lonlat2$max_wave<-idw(max_wave~1, fdata[complete.cases(fdata$max_wave),], newdata=lonlat2)$var1.pred
lonlat2$sig_wave<-idw(sig_wave~1, fdata[complete.cases(fdata$sig_wave),], newdata=lonlat2)$var1.pred
lonlat2$pri_wave<-idw(pri_wave~1, fdata[complete.cases(fdata$pri_wave),], newdata=lonlat2)$var1.pred

lonlat3<-as.data.frame(lonlat2)


load(file="a.temp2.rda") #a.temp2
load(file="a.data1.rda")
head(a.temp) # 과거 사고 일자의 기상을 idw 이용해서 예측한 자료
head(a.data1) # 과거 사고의 위험지수 표시

t.data <- merge(a.data1, a.temp, by="date")
head(t.data)

table(t.data$score) # 기상관련해서 위험하면 1 아니면 0으로 위험지수 재설정
t.data[t.data$score==3,"score"]=1
t.data[t.data$score==2,"score"]=1


t.data1 <- t.data[t.data$reason.x != "정비불량" & t.data$reason.x != "화기취급부주의" &
                    t.data$reason.x != "연료고갈" &t.data$reason.x != "적재불량" &
                    t.data$reason.x != "운항부주의" ,] 

t.data2<-t.data1[,c(-1:-16,-18:-29)]
head(t.data2)

set.seed(2352135)
s1 <- sample(1:nrow(t.data2),nrow(t.data2)*0.7)

train.data <- t.data2[s1,]
test.data <- t.data2[-s1,]


set.seed(67365437)
rf <- randomForest(score ~ ., data=train.data, importance=TRUE,
                   proximity=TRUE, ntree=1000)
print(rf)
## Look at variable importance:
round(importance(rf), 2)
varImpPlot(rf)


pred1<-as.numeric(round(predict(rf, newdata=train.data),0))
pred2<-as.numeric(round(predict(rf, newdata=test.data),0))


pred3<-as.numeric(round(predict(rf, newdata=lonlat3),0))

lonlat3$pred3<-as.numeric(round(predict(rf, newdata=lonlat3),0))



confusionMatrix(as.factor(train.data$score),as.factor(pred1))
confusionMatrix(as.factor(test.data$score),as.factor(pred2))

lonlat3_1<-lonlat3[which(lonlat3$pred3==1),]
lonlat3_2<-lonlat3[which(lonlat3$pred3==0),]

###실시간 해상위험지도###
hdf <- get_map(location= "Southkorea", zoom=6, maptype="toner-lite", source="stamen")
a<-ggmap(hdf) + geom_point(data=lonlat3_1, aes(lonlat3_1$lon, lonlat3_1$lat), shape= 16,colour="red", size=3) 
a2<-a+geom_point(data=lonlat3_2, aes(lonlat3_2$lon, lonlat3_2$lat), shape= 16,colour="blue", size=3) 
a2

#빨강이 위험, 파랑이 안전

