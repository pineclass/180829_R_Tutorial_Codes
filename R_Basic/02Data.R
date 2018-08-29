#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
# 수업내용 ; R Data
#                                  #
# 작성일자 :            #
# 작성자 : jhw  #
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# 1. Vector      *******
# 2. Factor      **
# 3. Matrix
# 4. Array
# 5. Data.Frame  *******
# 6. List        **

# 1. Vector----
# 하나의 열로 구성
# 하나의 데이터 유형만 가짐
# 데이터 분석의 가장 최소 단위

# 1.1 벡터 만들기
# (1) 하나의 값(element)으로 이루어진 벡터
# 데이터의 초기값
v1 <- 10
v2 <- "Female"
v3 <- TRUE

# (2) 2개 이상의 값으로 이루어진 벡터
# i. c(e1, e2, ...)
# c : combine, concatenate의 약자
# charavter, numeric, logical vector에 사용
v4<-c(1, 10, 5)
v4

v5 <- c("kim", "lee", "park")
v5
v6 <- c(TRUE, FALSE)
V6

c(1,"Lee",FALSE)
c(1, FALSE)

V1 <- 1:5
v7 <- 1:100
v7

v8 <- 10:1
v8

v9 <- -2.3:1
v9

v10 <- 5:-3.3
v10

1:-2.3

-3.5:2

seq(from=1, to=5, by=0.3)
seq(from=1, to=100, by=5)
seq(from=100, to=1, by=-5)

sequence(20)

sequence(0)

sequence(-3)
sequence(5.7)
sequence(1)

v1<-c(1)

c(1, 1:2, 1:3, 1:4, 1:5)
c(sequence(1), sequence(2), sequence(3), sequence(4), sequence(5))
sequence(1:5)

rep(1, times = 5)  
rep(1, each = 5)  

rep(1:3, times = 5)
rep(1:3, each = 5)

rep("a", "c", times = 3)
rep(c("a", "b", "c"), times = 5)

rep(1:3, times = 3, each = 5)
rep(c(1, 2, 3), times = c(100, 23, 8))
rep(1:3, times = c(100, 23, 8))

age <- c(29, 28, 35, 23, 50, 32)
length(age)
mode(age)
is.numeric(age)

names(age)
names(age) <- c("김이박", "최통진", "임수정", "아이언", "아이맘", "황비홍")
age

names(age) <- NULL


age[1]
age[2]
#question. 

age[1, 2, 5]
age[c(1, 2, 5)]

age[2:5]

age[seq(from=2, to = 6, by = 2)]

age[seq(from=2, to = length(age), by = 2)]

# 2.5    ----
v1 <- c(50, 100, 150, 50)
v2 <- c(45, 40, 27, 30)

v3 <- v1 + v2
v3

v4 <- 1:8
v5 <- v1 + v4
v5

v6 <- 1:3
v7 <- 1:5
bt <- c("b","a", "o", "o", "ab", "a")
bt.factor <- factor(bt)
bt.factor

bt.factor2 <- factor(bt, labels = c("A형", "AB형", "B형", "O형"))

bt.factor2

bt.factor3 <- factor(bt, levels = c("o", "a", "b", "ab"))

bt.factor3

bt.factor5 <- factor(bt, levels = c("o", "a", "b", "ab"), 
                     labels = c("O형", "A형", "B형", "AB형"),
                     ordered = TRUE)

bt.factor5

# 3. matrix----
# 행렬로 구성
# 2차원 구조

# 3.1 matrix 만들기

v1 <- 1:3
v2 <- 4:6
v3 <- 1:6
m1 <- rbind(v1, v2)
m1
m2 <- cbind(v1, v2)
m2

rbind(v1, v2, v3)

cbind(v1, v2, v3)

matrix(1:6, nrow = 2, ncol = 3)
matrix(1:6, nrow = 2, ncol = 3, byrow = TRUE)

A <- matrix(1:4, nrow = 2, ncol = 2)
B <- matrix(5:8, nrow = 2, ncol = 2)
A + B
A - B

A %*% B

solve(A)
A %*% solve(A)
solve(A) %*% A

x - y = 4
A <- matrix(c(1, 2, -1, 1), nrow = 2, ncol = 2)
B <- matrix(c(4, 2), nrow = 2, ncol = 1)
A
B
solve(A) %*% B

A <- matrix(1:6, nrow = 2, ncol =3)
A
t(A)

array(1:5, dim = 5)
array(1:5, dim = 8)
array(1:5, dim = c(2, 5))
al <- array(1:5, dim = c(2, 5, 3))

# 5.1 list 만들기
result <- list(v1, A, al)
result

# 5.2 slicing

result[1]
result[[1]]
result[2]
result[[2]]
result[3]
result[[3]]

id <- 1:3
age <- c(25, 26, 27)
company <- c("국립해양생물자원관", "충청지방통계청", "동남지방통계청")
marry <- c(FALSE, FALSE, FALSE)
df1 <- data.frame(id, age, company, marry)
df1


m1 <- cbind(id, age, company, marry)
m1






