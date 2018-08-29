if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org"); library(rpart)

# randomforest, svm, decision tree 등의 machine learning(기계학습)을 현실적으로 적용시키는데
# 가장 많이 발생하는 문제는 클래스의 불균형이다.

# 분류에 해당하는 데이터의 비율이 반반이 아닌 경우 훈련 데이터 내 비율이 높은 분류 쪽으로 결과를 내놓는 모델을 만든다.
# 예) 돌발홍수가 발생한 자료 50, 발생되지 않은 자료 950개인 경우
# 모형에서 모두 돌발홍수가 발생되지 않는다로 예측하더라도 예측정확도는 95%임.
# 따라서 분류를 잘하는 모형을 개발하기 위해선 데이터의 비율을 비슷하게 만들 필요가 있다.

if(!require(mlbench)) install.packages("mlbench", repos = "http://cran.us.r-project.org"); library(mlbench)
data(BreastCancer)

table(BreastCancer$Class)
# 양성 benign이 458개, 악성 malignant가 241개
# 그냥 모형을 세우개 되면 benign을 잘 예측하는 모형을 만들 가능성이 높다. 

# 클래스 불균형을 해결하기 위한 방법은 관찰 데이터가 적은 쪽에 더 큰 가중치(Weight)를 주는 방버
# 데이터가 적은 쪽으로 잘못 불류했을 때 더 많은 비용(cost 또는 loss)을 부과하는 방법 등이 있다. 

# 또 방법으로 훈련데이터를 동일하게 만드는 방법이 있다.

# 업샘플링, 다운샘플링
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org"); library(caret)

x <- upSample(subset(BreastCancer, select= -Class),BreastCancer$Class)
table(x$Class)
# x내의 행의 상당수는 중복되어 생성되어 있다.
    
y <- downSample(subset(BreastCancer, select= -Class),BreastCancer$Class)
table(y$Class)
# y 의 Class가 benign인 경우 일부가 임의로 제거되었다. 

# UpSample으로 자료를 생성한 경우와 그냥 사용했을 경우 모형성능은 정말 차이가 날까?

data <- subset(BreastCancer, select=-Id)
set.seed(124)
parts <- createDataPartition(data$Class, p=0.8) # 80%는 훈련, 20%는 테스트 데이터
data.train <- data[parts$Resample1,]
data.test <- data[-parts$Resample1,]
m.rpart <- rpart(Class ~., data=data.train)
confusionMatrix(data.test$Class, predict(m.rpart, newdat=data.test, type="class"))

data2 <- upSample(subset(data.train, select= -Class),data.train$Class)
m.rpart2 <- rpart(Class ~., data=data2)
confusionMatrix(data.test$Class, predict(m.rpart2, newdat=data.test, type="class"))

# SMOTE함수
# 인접값들을 찾아 추가하는 방법으로, 비율이 낮은 분류의 데이터를 추가로 생성하거나 높은 쪽ㅇ 데이터를 적게 샘플링 해준다.

if(!require(DMwR)) install.packages("DMwR", repos = "http://cran.us.r-project.org"); library(DMwR)

## A small example with a data set created artificially from the IRIS
## data 
data(iris)
data <- iris[, c(1, 2, 5)]
data$Species <- factor(ifelse(data$Species == "setosa","rare","common")) 
## checking the class distribution of this artificial data set
table(data$Species)

## now using SMOTE to create a more "balanced problem"
newData <- SMOTE(Species ~ ., data, perc.over = 600,perc.under=100)
table(newData$Species)

## Checking visually the created data
## Not run: 
par(mfrow = c(1, 2))
plot(data[, 1], data[, 2], pch = 19 + as.integer(data[, 3]),
     main = "Original Data")
plot(newData[, 1], newData[, 2], pch = 19 + as.integer(newData[,3]),
     main = "SMOTE'd Data")

## End(Not run)

## Now an example where we obtain a model with the "balanced" data
classTree <- SMOTE(Species ~ ., data, perc.over = 600,perc.under=100,
                   learner='rpartXse',se=0.5)
## check the resulting classification tree
classTree
## The tree with the unbalanced data set would be
rpartXse(Species ~ .,data,se=0.5)


######################################################################################
if(!require(rpart)) install.packages("rpart");library(rpart)
if(!require(rpart.plot)) install.packages("rpart.plot");library(rpart.plot)

set.seed(42)
index <- createDataPartition(BreastCancer$Class, p = 0.7, list = FALSE)

train_data <- BreastCancer[index, -1]
test_data  <- BreastCancer[-index, -1]

train_data[,1:9] <- apply(train_data[, 1:9], 2, function(x) as.numeric(as.character(x)))
test_data[,1:9] <- apply(test_data[, 1:9], 2, function(x) as.numeric(as.character(x)))


set.seed(42)
fit <- rpart(Class ~ .,
             data = train_data,
             method = "class",
             control = rpart.control(xval = 10, 
                                     minbucket = 2, 
                                     cp = 0), 
             parms = list(split = "information"))

plot(fit)
text(fit, cex=0.8)

rpart.plot(fit)

# logistic regression으로 세운다면 그리고 그 결과
# randomforest 로 세운다면
# SVM으로 세운다면

# UpSample된 자료로 rpart, randomforest을 돌려보자.