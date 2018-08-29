# 회귀분석을 위해 필요한 팩키지 설치 또는 불러오기
if(!require(car)) install.packages("car"); library(car)
if(!require(lmtest)) install.packages("lmtest"); library(lmtest)

# 분석을 위한 자료 불러오기

load(file='data/data2.rda')
head(data2)
# 위 자료는 엽장(l)과 관측된 기상요소 간 관계성을 규명하고자 수집된 자료입니다.
# 다중회귀모형을 통해 엽장(l)을 위한 최적회귀모형을 찾으세요

# Q1. 산점도를 통한 설명변수와 반응변수 간 관계 파악

# Q2. lm()함수를 이용한 회귀모형 세우기
out=lm(l~.-id,data=data2)
summary(out) # 얻어진 결과를 해석해보세요

# Q3. vif()함수를 이용한 다중 공선성 확인
vif(out)

# Q4. 정규성 검정 (정규성을 만족하지 않음)
qqnorm(out$residuals)
qqline(out$residuals)
shapiro.test(lm$residuals)

boxCox(out, lambda=seq(3,9,0.5))

# Q5. 잔차도표와 DurbinWatson을 통한 독립성 검정(독립성을 만족하지 않음)
plot(out$fitted.values, out$residuals)
durbinWatsonTest(out)

# Q6. 등분산성 검토 (등분산성을 만족하지 않음)
spreadLevelPlot(out)
ncvTest(out)

power=spreadLevelPlot(out)$PowerTransformation # 적합한 power 

# 반응변수 변환
out2=lm(l^power~.-id,data=data2)
summary(out2)

shapiro.test(out2$residuals) #정규성 만족
durbinWatsonTest(out2) # 독립성은 만족되지 않음
ncvTest(out2) #등분산성 만족

influencePlot(out2)
outlierTest(out2)

# 3 영향점, 48 이상점, 69번째는 영항점이면서 이상점으로 의심됨

k=10
#hat(값의 임계값)
2*(k+1)/nrow(data2)
#cookD 임계값
4/(nrow(data2)-k-1)

# 자료를 제거하고 다시 회귀모형 세우기
data3=data2[-c(3,48, 69),]

rownames(data3)<-1:nrow(data3)
out3=lm(l^power~.-id,data3)
summary(out3)

shapiro.test(out3$residuals)
durbinWatsonTest(out3)
ncvTest(out3)
influencePlot(out3)
# 분석결과 64번째 자료가 영향점이면서 이상점으로 보임

# 64번째를 다시 제거하고 회귀모형 세우
data4=data3[-c(64),]
rownames(data4)<-1:nrow(data4)
out4=lm(l^power~.-id,data4)
summary(out4)

shapiro.test(out4$residuals)
durbinWatsonTest(out4)
ncvTest(out4)
influencePlot(out4)

# 관측값과 예측값 간 그림 그려보기
obs.lm <- data4$l
pred.lm <- out4$fitted.values^(1/power)

(lm.rmse<-sqrt(sum((obs.lm-pred.lm)^2,na.rm=T)/length(out$residuals)))
plot(obs.lm, pred.lm, pch=16)

# randomforest모형과 비교해보기
library(randomForest)
colnames(data2)
rf_l=randomForest(l~.-id,data2)
pred.rf<-predict(rf_l,newdata=data2)
obs.rf <- data2$l
(rf.rmse<-sqrt(sum((obs.rf-pred.rf)^2,na.rm=T)/length(obs.rf)))

plot(obs.rf, pred.rf, pch=15)
