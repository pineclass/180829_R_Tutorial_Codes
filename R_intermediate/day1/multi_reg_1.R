if(!require(car)) install.packages("car"); library(car)

# 다중회귀모형
# 단순회귀모형은 반응변수와 설명변수가 각각 1개인 경우
# 설명변수가 여러개와 반응변수 간 선형관계식을 세우는 분석이 다중회귀분석
par(mfrow=c(1,1))

data(iris)
head(iris)

# Sepal.Length = Sepal.width + Petal.length+Petal.width + species 로 구성된 회귀식을 세워보자. 
# 회귀분석 전 설명변수와 반응변수의 관계를 시각적으로 확인하기 위한 산점도를 그려보자.
# 여러 변수 간 상관성을 살펴보기 위해 pairs.panels() 함수 이용

if (!require(psych)) install.packages("psych"); library(psych)
pairs.panels(iris[,1:4], scale=FALSE)
pairs.panels(iris[,1:4], scale=TRUE)

out.i <- lm(Sepal.Length ~ Sepal.Width + Petal.Length+ Petal.Width, data=iris)
summary(out.i)
# 다중회귀모형의 경우 설명변수가 증가할 수록 R^2가 증가한다.
# 불필요한 독립변수가 모형에 반영될 필요가 없으므로 독립변수의 수에 벌점을 준 수정된 R^2로 최적모형 결정한다.
# adj. R^2 = 1 - (SSE/(n-k-1)) / (SST/(n-1))
# 여기서 n은 데이터의 수, k는 독립 변수의 수이다. 

# 정규성 검정
qqnorm(out.i$residuals)
qqline(out.i$residuals)
shapiro.test(out.i$residuals)

# 독립성 검정
plot(out.i$fitted.values, out.i$residuals)
durbinWatsonTest(out.i)

# 등분산성 검정
spreadLevelPlot(out.i)
ncvTest(out.i)

# 다중공선성 문제, 독립변수들 간 높은 상관관계가 있는 경우 다중회귀모형 추정이 어려움
# 확인하기 위해 분산팽창지수 (vif)를 확인해야함.
# 만약 vif>5이상이면 변수선택, 능형회귀모형, 주성분회귀모형 등을 통해 다중공선성 문제 해결이 필요

vif(out.i) #  Petal.Length와  Petal.Width의 vif>5이상임 

out.i2 <- lm(Sepal.Length ~ Sepal.Width + Petal.Width, data=iris) # 높은 vif 순으로 제거
vif(out.i2) # 다중공선성 문제 해결

summary(out.i2) 

# 회귀모형의 진단 및 보정
# 지렛대점, 영향점, 이상점을 알아보자.
influencePlot(out.i2)

# StudRes : 이상점 여부 확인
# Hat : 모자행렬을 이용한 지렛대 점 확인
# CookD : 영향점 확인

# 이상점은 outlierTest()함수를 통해 확인
# 이상점이 많을 수록 모형의 결정계수인 R^2 가 감소됨
outlierTest(out.i2)

# Hat이 2(k+1)/n 이면 지렛대 점으로 주의깊게 볼 필요가 있음
2*(2+1)/nrow(iris) 

# 쿡의 거리는 4/(n-k-1)보다 크면 영향점으로 판단 
4/(nrow(iris)-2-1)

# 영향점 : 101, 107, 123, 132
# 지렛대점 : 16, 61, 132
# 이상점 : 107 

# 영향점과 이상점은 회귀모형에 안 좋은 영향을 미치는 자료로 제거시 더 좋은 회귀모형식을 얻을 수 있다.
# 단, 두 개 이상의 이상점이 이웃하면 서로의 이상점 효과가 상쇄될 수 있다. 
# 모형이 변하면 이상점을 다시 조사해야 한다.
# 데이터셋이 클 경우 한 두개의 이상점은 문제되지 않지만,
# 이상점이 그룹을 형성하는 경우에는 분석에 주의해야 한다.

# 이 자료의 경우 101, 107, 123, 132가 회귀모형에 악영향을 미친다고 판단되므로 제거하여 회귀모형을 다시 세우면

iris2 <- iris[c(-101,-107, -123, -132),]

out.i3 <- lm(Sepal.Length^-2 ~ Sepal.Width + Petal.Width, data=iris2) 
summary(out.i3) 

# 정규성 검정
qqnorm(out.i3$residuals)
qqline(out.i3$residuals)
shapiro.test(out.i3$residuals)

# 독립성 검정
plot(out.i3$fitted.values, out.i3$residuals)
durbinWatsonTest(out.i3)

# 등분산성 검정
spreadLevelPlot(out.i3)
ncvTest(out.i3)

# 추가그림은 반응변수와 설명변수의 관계를 2차식으로 표현한 그림이다.
avPlots(out.i3)

# 다중공선선, 영향점, 이상점, 지렛대점을 확인


#########################################################################
summary(iris) # Species 는 범주형 변수임
# 범주형 자료는 factor로 변환하여 분석 
# factor로 변환시 가변수로 처리하여 회귀모형을 세움

out.i2 <- lm(Sepal.Length ~ Sepal.Width + Petal.Length+ Petal.Width+as.factor(Species), data=iris)
summary(out.i2)
# 범주형 결과에 대한 해석
# default : setosa
# versicolor : setosa종에 비해 versicolor종의  Sepal.Length가 -0.72356 작다
# virginica  : setosa종에 비해 virginica종의  Sepal.Length가 -1.02350 작다

# 아이리스의 종별 Sepal.Length의 식을 다시 정리하면 아래와 같다.
# setosa : 2.17+Sepal.width*0.49 + petal.length*0.82+Petal.width*-0.31
# versicolor : 2.17 -0.72356 +Sepal.width*0.49 + petal.length*0.82+Petal.width*-0.31
# virginica : 2.17 - 1.02350 +Sepal.width*0.49 + petal.length*0.82+Petal.width*-0.31

##########################################################################
# 회귀모형의 설명변수가 많은 경우 중요한 설명변수를 선택해야한다.
# 1. 전진선택법
# 2. 후진제거법
# 3. 단계적 방법
# 위의 세가지 방법은 step()함수를 통해 진행할 수 있다.

if(!require(mlbench)) install.packages("mlbench"); library(mlbench)

data("BostonHousing")

m <- lm (medv ~ ., data=BostonHousing) # 여기서 .은  medv를 제외한 모든 변수를 설명변수로 간
m2 <- step(m, ddirection="forward") # 전진선택법
m3 <- step(m, ddirection="bacward") # 후진제거법
m4 <- step(m, ddirection="both")    # 단계적방법

formula(m2)
formula(m3)
formula(m4)

# 모든 가능한 경우를 고려한 최적모형 찾기
# leaps::regsubsets()함수는 2N개의 회귀 모형을 만들어 비교를 수행하는 함수

if(!require(leaps)) install.packages("leaps"); library(leaps)

m5 <- regsubsets(medv ~., data=BostonHousing)
summary(m5)

summary(m5)$bic
summary(m5)$adjr2

plot(m5, scale="adjr2")
plot(m5, scale="bic")

