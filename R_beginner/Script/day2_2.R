setwd("C:/Users/USER/Dropbox/R-work/2018-intermediate/R_beginner/data")

mydata3 = read.csv("anova_one_way.csv") # 파일을 불러와 mydata3로 저장
head(mydata3) # 불러온 데이터를 R Studio에서 확인
names(mydata3) # 불러온 데이터에 포함된 변수의 이름을 확인

boxplot(birth_rate~ad_layer, data=mydata3) #상자그림을 통해 지역별 차이를 시각적을 확인

library(dplyr)
mydata3 %>% group_by(ad_layer) %>% 
  summarise(n=n(), mean=mean(birth_rate), sd=sd(birth_rate))

info.mydata3<- as.data.frame(mydata3 %>% group_by(ad_layer) %>% 
  summarise(n=n(), mean=mean(birth_rate), sd=sd(birth_rate)))

# 분산분석을 위한 가정 
# 1. 정규성   : 그룹 내 표본이 충분히 많다면 정규성 검정을 하지 않아도 괜찮음
# 2. 독립성   
# 3. 등분산성


bartlett.test(mydata3$birth_rate~mydata3$ad_layer) #Bartlett 등분산성 검정
if(!require(car)) install.packages("car"); library(car)
leveneTest(mydata3$birth_rate~mydata3$ad_layer) # levene's 등분산성 검정

#일원분산분석
aov(mydata3$birth_rate~mydata3$ad_layer) # 종속변수 : birth_rate, 독립변수 : ad_layer
aov1 = aov(mydata3$birth_rate~mydata3$ad_layer) # 분산분석 결과를 aov1에 저장
aov1 = aov(birth_rate~ad_layer, data =mydata3) 

summary(aov1) #aov1 결과 출력 

# 분산분석표 결과 만들기 (수학식을 통해 계산 가능)
SST <- sum((mydata3$birth_rate-mean(mydata3$birth_rate))^2)
SSR <-sum((info.mydata3[,3]-mean(mydata3$birth_rate))^2*info.mydata3[,2])
SSE <- SST-SSR

(SS <- c(SSR, SSE))
(MS <- c(SSR, SSE)/c(2,223))

F.value=MS[1]/MS[2]
1-pf(F.value, 2, 223)

# 만약 통계적으로 유의미한 차이가 있다면 사후 분석 실시

TukeyHSD(aov1) # aov1의 사후검정을 시행
out1<-TukeyHSD(aov1) # aov1의 사후검정을 시행
out1
plot(out1)

if(!require(agricolae)) install.packages("agricolae"); library(agricolae)
out2<-LSD.test(aov1,"ad_layer",p.adj="bonferroni",group=F)
out2
out3<-LSD.test(aov1,"ad_layer",p.adj="bonferroni",group=T)
out3
plot(out3)
plot(out3, variation = "SD") #차이를 시각적으로 확인

# 등분산성을 만족하지 않으므로 위의 방법으로 분석하면 문제점 발생
oneway1<-oneway.test(mydata3$birth_rate~mydata3$ad_layer)
oneway1<-oneway.test(birth_rate~ad_layer, data=mydata3)

oneway1

# 정규성을 만족하지 않는다면, 비모수방법이용
kruskal1<-kruskal.test(mydata3$birth_rate~mydata3$ad_layer)
kruskal1<-kruskal.test(birth_rate~ad_layer, data=mydata3)

kruskal1

####################################################
# Example

diet = read.csv("diet.csv",row.names=1)
diet$weight.loss = diet$pre.weight - diet$weight6weeks 
boxplot(weight.loss~Diet,data=diet,col="light gray",
        ylab = "Weight loss (kg)", xlab = "Diet type")

head(diet)

diet %>% group_by(Diet) %>% 
  summarise(n=n(), mean=mean(weight.loss), sd=sd(weight.loss))

#정규성 검정
shapiro.test(diet[diet$Diet==1,'weight.loss'])
shapiro.test(diet[diet$Diet==2,'weight.loss'])
shapiro.test(diet[diet$Diet==3,'weight.loss'])

#등분산성 검정
bartlett.test(weight.loss~Diet, data=diet)

diet.out<-aov(weight.loss~Diet, data=diet)
summary(diet.out)

diet.post<-LSD.test(diet.out,"Diet",p.adj="bonferroni",group=T)
diet.post
plot(diet.post)
plot(diet.post, variation = "SD")

#이원배치분산분석
#이원배치분산분석을 통해 교호작용 여부를 확인 할 수 있다.
mydata4 = read.csv("anova_two_way.csv") 
head(mydata4) # 불러온 데이터를 R Studio에서 확인

summary(mydata4)

aov1 = aov(birth_rate~ad_layer, data=mydata4)  
aov2 = aov(birth_rate~multichild, data=mydata4)  
aov3 = aov(birth_rate~multichild*ad_layer, data=mydata4)  

summary(aov1) # aov1 결과 출력 
summary(aov2)
summary(aov3)

mydata4 %>% group_by(ad_layer, multichild) %>% 
      summarise(n=n(), mean=mean(birth_rate))

info.mydata4 <- as.data.frame(mydata4 %>% group_by(ad_layer, multichild) %>% 
                                summarise( mean=mean(birth_rate))
)

info.mydata4

info.multi1 <- info.mydata4[info.mydata4$multichild=="NO",]
info.multi2 <- info.mydata4[info.mydata4$multichild=="YES",]


plot(info.multi1$mean, type="l", ylim=c(1.1,1.9), xaxt="n")
axis(1, labels=info.multi1[,1], at=1:3)
lines(info.multi2$ad_layer, info.multi2$mean, col="blue")
# 기울기가 다를 경우 교호작용 존재