# Data wrangling
# Data Handling








# 패키지 설치하기
install.packages("ggplot2") 
install.packages("DT")
install.packages("dplyr")
install.packages("writexl")
library(ggplot2)
library(DT)
library(dplyr)
library(writexl)

# 작업공간 설정하기
setwd("d:/RBasic/")


# 예제 데이터 : ggplot2::diamonds


# 1. 데이터 전체보기----
! 1.1 dataname : console에 출력이 됨----
diamonds

# 1.2 View(data) : Editor window----
View(diamonds)


# 2. 데이터 일부 보기----
# 2.1 head(data, n = ) : console 에 출력
head(diamonds)
head(diamonds, n = 20)

# 2.2 tail(data, n = ) : console 에 출력
tail(diamonds)
tail(diamonds, n = 20)


# 2.3 DT::datatable(head(data, n = ) or tail(data, n = ))----
DT::datatable(head(diamonds, n = 20))


# 3. 데이터 구조(structure) 보기----
# str(data)
str(diamonds)


# 4. 간단한 기술통계량----
# summary(data)
summary(diamonds)


# 5. 데이터(data.frame)의 속성----
# 5.1 행의 개수 : nrow(data)----
nrow(diamonds)

# 5.2 열의 개수 : ncol(data)----
ncol(diamonds)

# 5.3 행의 이름 : rownames(data)----
rownames(diamonds)

# 5.4 열의 이름 = 변수명 = colnames(data)----
colnames(diamonds)

# 5.5 차원(Dimension)----
# 행의 개수와 열의 개수를 동시에 알려줌
# dim(data)
dim(diamonds)
dim(diamonds)[1] # 행의 개수
dim(diamonds)[2] # 열의 개수

# 5.6 차원의 이름----
# 행의 이름과 열의 이름을 동시에 알려줌
# dimnames(data)
dimnames(diamonds)
dimnames(diamonds)[1]   # 행의이름 : list
dimnames(diamonds)[[1]] # 행의이름 : vector

dimnames(diamonds)[2]   # 열의 이름 : list
dimnames(diamonds)[[2]] # 열의 이름 : vector


# 6. slicing----
# data[rowIndex, colIndex]
 
# 6.1 열(column)----
# data[ , colIndex]

# (1) 열의 위치를 알 때----
diamonds[ , 1]   # carat
diamonds[ , 2]   # cut

# 문제1 : 1, 2,10번째 열 한 번에 가져오기
diamonds[ , c(1, 2, 10)]

# 문제2 : 3~8번째 열 한 번에 가져오기
diamonds[ , 3:8]

# 문제3 : 홀수번째 열 한 번에 가져오기
diamonds[ , seq(from=1, to=length(diamonds) , by=2)]
diamonds[ , seq(from=1, to=ncol(diamonds) , by=2)]

# (2) 열의 이름(변수명)을 알때
diamonds[ , "carat"]
diamonds[ , "cut"]

# 문제 4 : cut, color, price 열을 한번에
diamonds[ , c("cut", "color", "price")]

# (3) 변수명에 특정한 패턴이 있는 경우
# grep("pattern", character data, value = )
# i. pattern : 특정한 문자를 포함 변수명
colnames(diamonds)

# 변수명 중에서 'c' 글자 포함 변수명
grep("c", colnames(diamonds), value = FALSE)
grep("c", colnames(diamonds), value = TRUE)

diamonds[ , grep("c", colnames(diamonds), value = FALSE)]
diamonds[ , grep("c", colnames(diamonds), value = TRUE)]

# 변수명 중에서 'c' 글자로 시작하는 변수명
grep("^c", colnames(diamonds), value = FALSE)
grep("^c", colnames(diamonds), value = TRUE)

diamonds[ , grep("^c", colnames(diamonds), value = FALSE)]
diamonds[ , grep("^c", colnames(diamonds), value = TRUE)]

# 변수명 중에서 'e' 글자로 끝나는 변수명
grep("e$", colnames(diamonds), value = FALSE)
grep("e$", colnames(diamonds), value = TRUE)

diamonds[ , grep("e$", colnames(diamonds), value = FALSE)]
diamonds[ , grep("e$", colnames(diamonds), value = TRUE)]

# (4) dplyr::select(data, variable) 
dplyr::select(diamonds, carat)
dplyr::select(diamonds, carat, price)
dplyr::select(diamonds, -carat)
dplyr::select(diamonds, color:price)

# (5) dplyr 의 pipe 기능
# %>% : pipe, chain
d1 <- diamonds %>% select(carat, price)
d1


# 6.2 행 
# data[rowindex, ]
diamonds[1, ]
diamonds[c(1, 2, 10), ]
diamonds[seq(from = 1, to = nrow(diamonds), by = 10), ]

# (1) 하나의 조건을 만족하는 행 추출
# cut 이 'ideal' 것만 추출
diamonds[diamonds$cut == "Ideal", ]

# price 18000 이상인 행 추출
diamonds[diamonds$price >= 18000 , ]


# (2) 두 개 이상의 조건 만족 행 추출
# cut 이 ideal, price 18000 이상
diamonds[(diamonds$cut == "Ideal") & (diamonds$price >= 18000) , ]

# cut 이 ideal 이거나 price 18000 이상인 행
diamonds[(diamonds$cut == "Ideal") | (diamonds$price >= 18000) , ]

# cut 이 'Ideal' 이거나 또는 cut 이 'Good' 인 행
diamonds[(diamonds$cut == "Ideal") | (diamonds$cut == "Good") , ]
diamonds[diamonds$cut %in% c("Ideal", "Good") , ]

# (3) dplyr::filter(data, 조건)
dplyr::filter(diamonds, cut == "Ideal")
dplyr::filter(diamonds, price >= 18000)
dplyr::filter(diamonds, cut == "Ideal", price >= 18000)
dplyr::filter(diamonds, cut == "Ideal" | price >= 18000)

diamonds %>% filter(cut == "Ideal")


# 6.3 행과 열----
# data[rowIndex , colIndex]
# ,cut 이 'Ideal" 이고 price 가 18000 이상인 데이터의
# x, y, z 열을 가져오세요
View(diamonds)
diamonds[(diamonds$cut == "Ideal") & (diamonds$price >= 18000) , c("x", "y", "z")]

diamonds %>% 
  filter(cut == "Ideal", price >= 18000) %>% 
  select(x:z)

diamonds %>% 
  filter(cut == "Ideal", price >= 18000) %>% 
  select(x:z) %>% 
  summarise(x.mean = mean(x),
            y.mean = mean(y),
            z.mean = mean(z))

diamonds %>% 
  filter(cut == "Ideal", price >= 18000) %>% 
  group_by(color) %>%  
  summarise(x.mean = mean(x),
            y.mean = mean(y),
            z.mean = mean(z))

levels(diamonds$color)

diamonds %>% 
  filter(cut == "Ideal", price >= 18000) %>% 
  group_by(color) %>%  
  summarise(x.mean = mean(x),
            y.mean = mean(y),
            z.mean = mean(z)) %>% 
  arrange(desc(x.mean))

# 7. 새로운 변수 생성하기----
# 7.1 연산
# data$newVariable <- 연산
# xyz.mean <-  (x +  y + z) / 3
diamonds$xyz.mean <- (diamonds$x + diamonds$y + diamonds$z) / 3

# xyz.mean2 <- rowMeans(data[ , colIndex])
diamonds$xyz.mean2 <- rowMeans(diamonds[, c("x", "y", "z")])

# 루트 x
# x.root
diamonds$x.root <- sqrt(diamonds$x)
View(diamonds)

# logx
diamonds$x.log <- log10(diamonds$x)
View(diamonds)     

# 7.2 구간 정보 변수 생성
# (1) cut()
# data$newVariable <- cut(data$variable,
                          breaks = 구간정보, 
                          right  = )
diamonds$price.group <- cut(diamonds$price,
                            breaks = seq(from = 0, to = 20000, by = 5000),
                            right  = FALSE) # 이상 ~ 미만 구간
                           # right = TRUE : 초과  ~ 이하 구간

str(diamonds)
levels(diamonds$price.group) <- c("5000미만",
                                  "5000이상~10000미만",
                                  "10000이상~15000미만",
                                  "15000이상")


# (2) ifelse(조건, 참일 때의 값, 거짓일 때의 값)
# cut 이 5개의 집단인데
# 이것을 "Ideal" , "Non-Ideal" 2개 그룹으로 만들기

# data@newVariable <- ifelse(diamonds$cut == "Ideal", 


diamonds$cut.group <- ifelse(diamonds$cut == "Ideal", "Ideal", "Non-Ideal")


diamonds$price.group2 <- ifelse(diamonds$price < 5000,
                                "5000미만", 
                                ifelse(diamonds$price < 10000,
                                       "5000이상~10000미만",
                                       ifelse(diamonds$price < 15000,
                                              "10000이상~15000미만", "15000이상")))



# 8. 기존 변수 삭제하기 ----
# data$variable <- NULL
diamonds$xzy.mean2    <- NULL
diamonds$price.group2 <- NULL


# 9. R 데이터를 외부 데이터로 저장
# 9.1 txt 로 저장 ----
# write.table(data, 
#             file = "directory/filename.txt",
              sep  = ",",
              row.names = FALSE)
write.table(diamonds,
            file      = "d:/RBasic/diamonds_0502.txt",
            sep = ",",
            row.names = FALSE)


# 9.2 csv 로 저장
write.csv(diamonds,
          file = "diamonds_0502.csv",
          row.names = FALSE)

# 9.3 excel 로 저장
# R 이 기본 기능으로 못 함
# writexl::write_xlsx(data, 
#                     path = "directory/filename.xlsx")

writexl::write_xlsx(diamonds,
                    path = "diamonds_0502.xlsx")






# 10. R 데이터를 R 데이터로 저장하고 불러오기
# 10.1 RData 로 저장하기
# save(data, file = "directory/filename.RData")
save(diamonds, file = "diamonds_0502.RData")

# 10.2 RData 불러오기
load(file = "diamonds_0502.RData")


# 11. 데이터의 목록 보기 ----
# ls()
ls()


# 12. 데이터 삭제ㅏ기 ----
# 12.1 하나 또는 몇개 지우기
# rm(data1, data2, ...)
rm(diamonds)
rm(d1)

# 12.2 모든 데이터 지우기
# rm(list = ls())
# remove all 기능
rm(list = ls())


head(diamonds)


# 13. 데이터 합치기
# 13.1 rbind(data1, data2)
diamonds.ideal <- diamonds[diamonds$cut == "Ideal", ]
diamonds.good <- diamonds[diamonds$cut == "Good", ]
diamonds.ideal.good <- rbind(diamonds.ideal, diamonds.good)
diamonds.ideal.good


# 13.2 cbind(data1, data2)
 diamonds.12345 <- diamonds[ , 1:5]
 diamonds.678910<- diamonds[ , 6:10]
diamonds.all <- cbind(diamonds.12345, diamonds.678910) 
 diamonds.all
 
# 13.3 merge(data1,data2, by = )
df1 <- data.frame(id = c(1, 2, 7, 8), age= c(10, 20, 70, 80))
df2 <- data.frame(id = c(1, 3,4, 7, 10), bt = c("a", "a", "b", "o", "ab"))

# (1) inner join

merge(df1, df2, by = "id")

# (2) outer join : full join

merge(df1, df2, by = "id", all = TRUE)

# (3) outer join : left join

merge(df1, df2, by = "id", all.x = TRUE)

# (4) outer join : right join

merge(df1, df2, by = "id", all.y = TRUE)

# 14. 데이터 정렬
# 14.1 vector 정렬하기
# sort(vector, decreasing = FALSE) : 오름차순
# sort(vector, decreasing = TRUE) : 내림차순
age <- c(29, 50, 35, 19, 51, 32)
age

sort(age, decreasing = FALSE)
sort(age, decreasing = TRUE)

# 14.4 데이터 프렝임 정렬하기
# data[order(data$variable, decreasing = FALSE) , ] : 오름차순
# data[order(data$variable, decreasing = TRUE) , ] : 내림차순


age[order(age, decreasing = FALSE)]
age[order(age, decreasing = TRUE)]

diamonds[order(diamonds$price, decreasing = FALSE) , ]
diamonds[order(diamonds$price, decreasing = TRUE) , ]

# cut 오름차순, price 오름차순
diamonds[order(diamonds$cut, diamonds$price, decreasing = FALSE) , ]
# cut 내림차순, price 내림차순
diamonds[order(diamonds$cut, diamonds$price, decreasing = TRUE) , ]


# cut 오름차순, price 오름차순
diamonds[order(diamonds$cut, -diamonds$price, decreasing = FALSE) , ]

# cut 오름차순, price 오름차순
diamonds[order(diamonds$cut, -diamonds$price, decreasing = TRUE) , ]


# dplyr : arrangeIdata, variable, desc(variable)
dplyr::arrange(diamonds, cut, desc(color))






