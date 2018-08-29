setwd("C:/Users/USER/Dropbox/R-work/2018-intermediate/R_beginner/data")

mydata = read.csv("onesample.csv")
head(mydata)
str(mydata)

summary(mydata$birth_rate) # birth_rate의 기술통계량을 구함
sd(mydata$birth_rate) # birth_rate(합계출산율)의 표준편차를 구함
hist(mydata$birth_rate)
abline(v=1.34, col="blue")

qqnorm(mydata$birth_rate)
qqline(mydata$birth_rate)
shapiro.test(mydata$birth_rate) #정규성 검정 (p-value가 0.05보다 작으면 정규분포를 따르지 않는다)

t.result<-t.test(mydata$birth_rate,mu=1.34) # 단일표본 t-test의 시행(모집단의 평균 : 1.34)
t.result
t.result$statistic # t 통계량
t.result$p.value # 유의확률
t.result$conf.int # 신뢰구간
#t값 : -3.2988, 자유도 19, 유의확률 : 0.003775

# 통계 계산식을 활용한 t값 계산 및 유의확률 계산
t.value = (mean(mydata$birth_rate)-1.34)/sqrt(var(mydata$birth_rate)/length(mydata$birth_rate)) # t값
pt(t.value, df=19)*2

# boostrap 방법을 이용하여 가상의 분포를 생성하여 결과 비교
mean_boot <- rep(NA, 10000)
set.seed(14)
for (i in 1:10000) { 
  mean_boot[i] <- mean(sample(mydata$birth_rate,15,replace=TRUE)) 
} 

quantile(mean_boot, probs =c(0.025, 0.975))
sum(mean_boot>1.34)/10000*2

hist(mean_boot)
abline(v=1.34, col="blue")

# 결과를 표로 정리
nrow(mydata)
mean(mydata$birth_rate)
sd(mydata$birth_rate)
t.result$statistic
t.result$p.value

out<-c(nrow(mydata),   mean(mydata$birth_rate),   sd(mydata$birth_rate),   t.result$statistic,   t.result$p.value)
out1<-round(out,3)
names(out1)<-c("n", "mean", "sd", "t", "p-value")
out1

# 자료가 소표본(30미만)이고 정규분포를 따르지 않을 때에는 비모수 검정실시
set.seed(142134)
x <- rexp(20) 
hist(x)
mean_x <- mean(x) 

qqnorm(x)
qqline(x)

shapiro.test(x) # 정규분포를 따르지 않음

t.test(x, mu=1.1) # t 검정 시 평균차이가 없다는 결과

wilcox.test(x, mu = 1.1, alternative = "two.sided") # 비모수방법을 통한 가설검정, 차이가 있다는 결과
sum(x<1.1)
sum(x>1.1)

# 자료가 대표본일 경우 중심극한 정리에 의해 표본평균의 분포는 정규분포를 따른다.

#######################################################################################
# 독립표본 t검정
mydata1 = read.csv("independent.csv") # 파일을 불러와 mydata1로 저장

head(mydata1) 
names(mydata1) # mydata1안에 포함되어 있는 변수의 이름을 확

boxplot(birth_rate~dummy, data=mydata1)

qqnorm(mydata1[mydata1$dummy==0,"birth_rate"])
qqline(mydata1[mydata1$dummy==0,"birth_rate"])

shapiro.test(mydata1[mydata1$dummy==0,"birth_rate"]) # 정규분포를 따르지 않음
shapiro.test(mydata1[mydata1$dummy==1,"birth_rate"]) # 정규분포를 따르지 않음
# 하지만 각 집단의 수가 충분히(n>30) 대표본으로 볼 수 있으므로 t검정 가능
# 이유 : 중심극한의 정리

t.out1<-t.test(mydata1$birth_rate~mydata1$dummy, alt="two.sided",conf=0.95, paired=F, var.eq=T)
t.out2<-t.test(mydata1$birth_rate~mydata1$dummy, alt="two.sided",conf=0.95, paired=F, var.eq=F)

t.out1
t.out2

var.test(birth_rate~dummy, data=mydata1) #등분산성 검정
summary(mydata1)

t.out2$statistic #등분산이 아닌 경우 검정통계량 t 계산
diff<-mean(mydata1[mydata1$dummy==0,"birth_rate"])-mean(mydata1[mydata1$dummy==1,"birth_rate"])
var1<-var(mydata1[mydata1$dummy==0,"birth_rate"])
n1<-length(mydata1[mydata1$dummy==0,"birth_rate"])

var2<-var(mydata1[mydata1$dummy==1,"birth_rate"])
n2<-length(mydata1[mydata1$dummy==1,"birth_rate"])

diff/sqrt(var1/n1+var2/n2) == t.out2$statistic

t.out1$statistic #등분산을 가정했을 경우 검정통계량 t 계산
diff<-mean(mydata1[mydata1$dummy==0,"birth_rate"])-mean(mydata1[mydata1$dummy==1,"birth_rate"])
var1<-var(mydata1[mydata1$dummy==0,"birth_rate"])
n1<-length(mydata1[mydata1$dummy==0,"birth_rate"])

var2<-var(mydata1[mydata1$dummy==1,"birth_rate"])
n2<-length(mydata1[mydata1$dummy==1,"birth_rate"])

var3 <- ((n1-1)*var1+(n2-1)*var2)/(n1+n2-2)
diff/sqrt(var3/n1+var3/n2) == t.out1$statistic

# 요약 
# 각 집단의 수가 30미만일 경우 정규성 검정을 실시한다.
# 만약 하나의 집단이라도 정규분포를 따르지 않는다면 비모수 검정을 실시한다.

wilcox.test(mydata1$birth_rate~mydata1$dummy)

# t 검정을 실시하기 전, 등분산 검정(var.test)를 실시한다.
var.test(mydata1$birth_rate~mydata1$dummy) # 유의확률이 0.05이상이면 등분산이다.

# 등분산을 만족하면 var.eq=T, 만족하지 않으면 var.eq=F를 옵션을 준다.
# 계산된 p-value를 계산하면 된다.

#결과를 표로 정리하기 
library(dplyr) 
mydata1 %>% group_by(dummy) %>% 
  summarise(n=n(), mean=mean(birth_rate), sd=sd(birth_rate))

t.out2<-t.test(mydata1$birth_rate~mydata1$dummy, alt="two.sided",conf=0.95, paired=F, var.eq=F)
round(t.out2$statistic,3)
round(t.out2$p.value,3)

# 대응 t 검정
mydata2 = read.csv("paired.csv")

head(mydata2)

boxplot(mydata2$birth_rate_2015, mydata2$birth_rate_2010)

t.test(mydata2$birth_rate_2010,mydata2$birth_rate_2015,paired = T)

mydata2$diff = mydata2$birth_rate_2010- mydata2$birth_rate_2015
t.test(mydata2$diff, mu=0)

# 자료가 소표본이라면 정규성 검정은 어떻게??
shapiro.test(mydata2$diff)

# 자료가 소표본이고 정규분포를 따르지 않을 경우 비모수검정을 실시
wilcox.test(mydata2$birth_rate_2010,mydata2$birth_rate_2015,paired=TRUE) 

# 분석결과를 위한 자료 정리
nrow(mydata2)
c(mean(mydata2$birth_rate_2010), sd(mydata2$birth_rate_2010))
c(mean(mydata2$birth_rate_2015), sd(mydata2$birth_rate_2015))
t.out <-t.test(mydata2$birth_rate_2010,mydata2$birth_rate_2015,paired = T)

t.out$statistic
t.out$p.value