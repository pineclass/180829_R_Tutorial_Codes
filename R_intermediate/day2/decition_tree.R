# 분류함수로 logistic regression외 다른 기계학습방법(데이터마이닝, 빅데이터방법)이 존재

# 1. 의사결정나무 (Decision Tree)
# 의사결정나무는 지니 불순도(Gini Impurity) 또는 정보이득 (Information Gain) 등의 기준을 사용하여
# 노드를 재귀적으로 분할하면서 나무 모형을 만든다. 

# example
if(!require(rpart)) install.packages("rpart"); library(rpart)

data(iris)
dt <- rpart(Species ~., data=iris)
table(iris$Species)

dt
# 총 150개의 자료이며 기본적으로 각 종의 비율을 1/3, 1/3, 1/3
# 2) 기준에 의해 즉 Petal.Length<2.45로 setosa가 판별됨
# setosa 50개 분류
# 6) Petal.width <1.75를 기준으로 54개의 versicolor라 분류 (분류가 잘못된 자료 5개)
# 7) 로 verginica 46개 예측 (분류가 잘못된 자료 1개)

# plot(dt)
plot(dt, compress=TRUE, margin=.2)
# compress로 나무를 좀 더 조밀하게 그릴 수 있다.

text(dt, cex=0.9)
# cex는 글자의 크기를 의미

pred.dt<-predict(dt, newdat=iris, type="class") 

xtabs(~pred.dt+iris$Species)

# Petal.Length<2.45, Petal.width<1.75 가 노드(Node)
# 노드 수 결정 
# 가지치기 : 오분류 가능성이 높아지는 경우 노드를 정지
# 언제까지 나무모형을 성장시킬 것이냐?
# 너무 큰 나무는 자료를 과대적합하고, 반대로 너무 작은 나무는 자료를 과소 적합
# 일반적으로 사용되는 방법은 마디에 속하는 자료가 일정 수 이하일 때 분할 정시 (thumb rule)

# another package
library(MASS)
if(!require(tree)) install.packages("tree");library(tree)

tree.iris <- tree(Species ~., data=iris)
summary(tree.iris)

plot(tree.iris)
text(tree.iris, cex=0.9)

# 과적합화의 문제가 있으므로 가지치기단계가 필요함

cv.tree.out<-cv.tree(tree.iris,FUN=prune.tree, K = 5 )
plot(cv.tree.out)

tree.iris2<-prune.tree(tree.iris, best=4)
plot(tree.iris2)
text(tree.iris2, cex=0.9)

# 의사결정나무의 장단점
# 장점 : 설명력이 높다.
#        결과에 대한 근거를 나무가지 형태로 쉽게 추적할 수 있다.
#        계산이 빠르고 변수 선택 능력이 있다.

# 단점 : 반응변수가 연속형일 때 사용하기 어렵다.
#        설명변수가 연속형일 때 낮은 예측능력을 보일 수 있다. 
#        자료가 추가되면 나무구조가 바뀔 수 있다.
#        비선형데이터는 나무모형으로 잘 설명되지 않는다.

# 조건부 추론 나무는 조건부 분포로 분류하는 방법으로 과적합의 문제를 해결해준다.

if(!require(party)) install.packages("party"); library(party)
ct <- ctree(Species~., data=iris)
ct
plot(ct)

predict(ct, newdata=iris)

# Randomforest는 앙상블(Ensemble) 학습을 사용한 모델
# 앙상블 학습은 주어진 데이터로부터 여러개의 모델을 학습한 다음, 
# 예측 시 여러 모델의 예측 결과들을 종합해 정확도를 높이는 기법

# 랜덤 포레스트는 두가지 방법을 사용해 다양한 의사결정 나무를 만듬
# 첫번째, 데이터의 일부를 복원추출로 사용하여 의사결정나무 생성
# 두번째 노드 나누는 기준에 사용되는 변수를 일부만 사용

# 랜덤포레스트는 성능이 뛰어난 의사결정나무 하나로 모형을 예측하지 않고
# 여러개를 사용하여 예측하므로 과적합므이 문제가 피해간다.

if(!require(randomForest)) install.packages("randomForest"); library(randomForest)
rf.iris <- randomForest(Species~., data=iris, importance=TRUE)

rf.iris
plot(rf.iris) #적절한 ntress를 판단할 수 있다.

importance(rf.iris) #변수별 중요도 확인
varImpPlot(rf.iris)

# 랜덤포레스트를 개선한 RRF (Regularized Random Forest)도 있음
# package(RRF)

# 랜덤포레스트 모형개선
# randomforest()함수에서는 나무개수, 고려할 변수 mtry 등의 파라메터가 있다. 
# 기본적으로 자동자로 잘 부여되지만 모형 성능을 개선하고 싶다면 이 값을  조정할 수 있다.

grid <- expand.grid(ntree=c(10,100,200), mtry=c(2,3,4))
grid

# 최적 조합을 찾기 위해 10개로 분할된 데이터를 이용해보자.

if(!require(cvTools)) install.packages("cvTools"); library(cvTools)
if(!require(foreach)) install.packages("foreach"); library(foreach)

set.seed(3513)
K=10
R=3

cv <- cvFolds(nrow(iris), K=K, R=R)

grid <- expand.grid(ntree=c(10,100,200), mtry=c(3,4))

result <- foreach(g=1:nrow(grid), .combine=rbind) %do% {
  foreach(r=1:R, .combine = rbind) %do% {
    foreach(k=1:K, .combine=rbind) %do% {
    
      validation_idx <- cv$subsets[which(cv$which == k), r]
      
      train <- iris[-validation_idx,]
      validation <- iris[validation_idx,]
      
      m <- randomForest(Species~., data=train, ntree=grid[g, "ntree"],
                        mtry=grid[g,"mtry"])
      
      predicted <- predict(m, newdata=validation)
      
      precision <- sum(predicted == validation$Species) / length(predicted)
      return(data.frame(g=g, precision=precision))
    }
  }
}

result    

if(!require(plyr)) install.packages("plyr"); library(plyr)
#grid에 따른 결과를 살펴보자.
ddply(result, ~g, summarize, mean_precision=mean(precision))

      