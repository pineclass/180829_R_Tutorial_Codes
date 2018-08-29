# 회귀분석 기초와 상관분석

# 회귀분석의 이해
# http://www.shodor.org/interactivate/activities/Regression/

# 회귀분석 기초1(Simple regression analysis 1)
# 가장 간단한 형태의 회귀분석은 한 개의 설명변수와 한 개의 반응변수 간의 관계식을 찾는 문제
# 이러한 경우를 단순회귀분석이라고 한다. 

# 단순회귀분석을 위해선 가장 먼저 산점도를 통해 설명변수와 반응변수 간 선형 관계가 있는지 확인해야한다.
# 산점도는 plot()함수를 이용

data(cars) # 속도에 따른 정지거리(dist)에 관한 데이터

cars
plot(cars$speed, cars$dist)

# 산점도를 그려보면 속도(speed)와 정지거리(dist) 간 선형관계가 있어보인다.
# 설명변수와 반응변수 간 상관정도를 정량적으로 확인하기 위해 상관분석을 실시한다.
# R에서는 cor()함수를 이용

cor(cars$speed, cars$dist)
cor.test(cars$speed, cars$dist) # 가설검정까지 원하는 경우

# 그러면 최적 선형식은 어떻게 구할까? (모수추정)
# 최적 선형식을 위해선 dist = beta_0 + beta_1 * speed의 beta_0와 beta_1을 추정해야 한다. 

# 만약 beta_0= -2, beta_1= 4 를 넣는다면
# dist = -2 + 4* speed로 예측된다. 

hat.dist = -2 + 3.8 * cars$speed

# 실제값과 예측값의 차이를 만들어보자. 
diff = cars$dist - hat.dist

# 실제값과 예측값이 차이의 제곱 합을 계산하면 다음과 같다. 
sum(diff^2)  

# 그러면 위의 값을 최소로 하는 식이 최적식이 아닐까? (최소제곱법)
f.out <- function(x) {
  hat.dist = x[1] + x[2] * cars$speed
  diff = cars$dist - hat.dist
  return(sum(diff^2))
}

f.out(x=c(-2,4))
f.out(x=c(-17,4))
f.out(x=c(-18,4))
f.out(x=c(-19,4))
f.out(x=c(-20,4))
f.out(x=c(-21,4))

# 대충 최적값을 짐작하면 beta_0 = -19, beta_1 =4 이다. 
# 하지만 이 값이 최적점일까? NO

optim(c(0,1), f.out)$par # 수치적 방법을 이용하여 최적값을 찾을 수 있다.

# 이러한 모수 추정방법을 최소제곱법이라 한다. 
# 하지만 위의 방법으로 수치적 방법을 이용하지 않더라도 lm()함수를 이용하면 결과 확인 가능

out <- lm(cars$dist~cars$speed) # 반응변수 ~ 설명변수 식을 lm()에 넣어주면 된다.
out <- lm(dist~speed, data=cars) # 또 다른 표현 

summary(out)  # 결과 확인

# 모수의 추정방법으로 우도함수를 이용한 최대우도법이 있으나, 모수추정치는 동일하여 설명은 생략한다.

# 모형평가
out <- lm(dist~speed, data=cars) # 또 다른 표현 
summary(out)

# summary() 함수의 가장 처음에는 함수식을 나타낸다.
# Residuals 부분은 실제 데이터에서 관측된 잔차를 보여준다.
# Residual =  관측값 - 예측값

obs <- cars$dist # 실제 정지거리
pred <- -17.5791+3.9324*cars$speed # 추정된 선형식을 통한 예측값
resd <- obs- pred

summary(resd) # summary(out)의 Residuals값과 동일
out$residuals # 잔차를 불러오기
residuals(out)


out$fitted.values # 추정된 선형식으로 예측된 값을 선형식을 세우지 않고 불러올 수 있다.
fitted(out) 

# Coefficients 에서는 회귀모형의 계수와 이 계수의 통계적 유의성을 보여준다.
# Estimate 열은 절편과 계수의 추정치
# dist = -17.5791+3.9324*speed
# Pr(>|t|)는 t 분포를 이용하여 각 변수가 유의한지 판단. 기준은 일반적으로 0.05
# 만약, p-value가 0.05보다 크면 혜당 계수가 0이라는 귀무가설을 기각할 수 없으므로 0으로 봐야한다.


# 마지막으로 결정계수 (Multiple R-squared) 와  회귀모형의 유의성을 의미하는 F통계량이 제시됨
# 여기서 결정계수란?? 선형모형의 설명력으로 해석 

var(cars$dist) # Var(관측값)
var(cars$dist-fitted(out)) + var(fitted(out)) # Var(관측값-예측값)+Var(예측값)

SST = sum((cars$dist - mean(cars$dist))^2)
SSE = sum((cars$dist - fitted(out))^2)
SSR = sum((fitted(out)-mean(cars$dist))^2)
SST == SSE+ SSR  # 논리 확인

# 모형이 잘 맞는다는건 관측값과 예측값이 비슷하다고 볼 수 있다. 즉 SSE가 0에 가까워짐
# SSR/SST는 전제분산 중 예측값으로 설명되는 분산의 비
# Multiple R-squared로 의미는 반응변수의 분산 중 설명변수로 설명되는 분산의 비율

SSR/SST 

# R^2 는 0과 1범위에 존재하며 단순회귀모형의 경우 상관계수의 제곱과 같다.
cor(cars$speed, cars$dist)^2

# F 통계량은 full model : dist = beta_0 + beta_1 * speed
#            Reduced model : dist = beta_0 
# 간 차이를 비교한 값. 즉 통계적으로 유의미한다는건 설명변수가 반응변수에 영향을 미침

model1 <- lm(dist~speed, data=cars)
model2 <- lm(dist~1, data=cars)
anova(model1, model2)
# 즉 speed가 유의미한 설명변수이다. 

anova(out) #회귀모형에서의 분산분석결과

# 새로운 값이 있을 때 예측은? predict()함수 이용
out <- lm(dist~speed, data=cars)
predict(out, newdata=data.frame(speed=c(3,4,5)))

predict(out, newdata=data.frame(speed=c(3,4,5)), interval="confidence") 
# 신뢰구간의 하한과 상한 제시, 평균적인 차량에 대한 신뢰구간

predict(out, newdata=data.frame(speed=c(3,4,5)), interval="prediction") 
# 특정 속도를 가잔 차량 한대의 제동거리는 평균적인 차량에 비해 오차가 크므로 범위가 더 넓어짐

# 단순회귀모형의 시각화
plot(cars$speed, cars$dist)
abline(coef(out), col="blue")


speed <- seq(min(cars$speed), max(cars$speed),.1)
pred.dist <- predict(out, newdata=data.frame(speed=speed), interval="confidence")
matplot(speed, pred.dist, type='n')
matlines(speed, pred.dist, lty=c(1,2,2), col=1) # 선형 회귀식은 직선, 신뢰구간은 점선으로 표현
matpoints(cars$speed, cars$dist, pch=1)
###########################################################
# 단순회귀분석 Summary 1

# 1. 설명변수와 반응변수의 산점도를 그린다. plot()
#    - 설명변수와 반응변수 간 1차 선형관계가 있는지 확인한다.

# 2. 상관분석을 통해 설명변수와 반응변수의 1차 선형관계를 확인한다. cor()
#    - p-value<0.05 이하 이면 유의미한 관계가 있다고 판단.

# 3. lm()함수를 이용하여 1차 선형식을 추정한다.

# 4. F통계량으로 설명변수의 회귀모형의 유의성을 확인한다.

# 5. 결정계수를 통해 선형회귀모형의 설명력을 정량적으로 계산한다.

# 6. 추정된 회귀계수를 통해 선형식을 구한다.

# 7. predict()함수로 새로운 값의 예측값을 계산한다. 

alligator = data.frame(
  lnLength = c(3.87, 3.61, 4.33, 3.43, 3.81, 3.83, 3.46, 3.76,
               3.50, 3.58, 4.19, 3.78, 3.71, 3.73, 3.78),
  lnWeight = c(4.87, 3.93, 6.46, 3.33, 4.38, 4.70, 3.50, 4.50,
               3.58, 3.64, 5.90, 4.43, 4.38, 4.42, 4.25)
)

# Q.1 다음은 악어의 길이와 무게로 구성된 자료이다.
#      연구자가 악어의 길이로 무게를 예측하기 위한 선형식을 구한다고 한다. 
#      적합한 선형식은?
# Q.2. summary()함수를 이용하여 회귀분석 결과에 대해 해석하세요.
# Q.3 길이가 4.5인 악어가 잡혔다고 한다. 이 악어의 예상 무게는? 
# Q.4 회귀분석 결과를 시각화 하세요 

plot(alligator$lnWeight, alligator$lnLength) # 산점도