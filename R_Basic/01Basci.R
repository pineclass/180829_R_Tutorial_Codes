#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
# 수업내용 ; #
#                                  #
# 작성일자 :            #
# 작성자 : jhw  #
#

# 1. 연산자(operator----
# 1.1 산술 연산자(arithmatic operator)----
# +, -,*,/. **/ ^, %%, %/%
3 + 4 # 더하기
3 - 4 # 빼기
3 * 4 # 곱하기
3 / 4 # 나누기
3 ** 4 # 거듭제곱
13 %% 4 # 나머지
13 %/% 4 # 몫

# 루트 3
3 ** (1 / 2)
3 ** 1/2
3 ** (1 / 3)

3 -4 ment, 설명
# ; : 명령어의 끝
# enter : 다음 줄 이동
# Ctrl + enter : 명령어 
x <-  10
y = 10
x <-  rnorm(100)
y = 100
hist(x, col = "blue")

# 1.3 비교연산자----
# >, >=, <, <=, ==, !=, !

3 > 4
3 >= 4
3 <4    
3 <= 4  # 작거나 같다
3 == 4  # 같다
3 != 4  # 같지 않다
!(3 == 4)# not

# 1.4 논리연산자(logical operator)----
# &, |(pipe)
# & : and
# | : or

(3 > 4) & (5 > 4)
(3 > 4) | (5 > 4)

# 2. 데이터의 유형 (type of data)
# 수치형 numeric : 정수 integer, 실수 double
# 문자형 character 
# 논리형 logical

x1 <-10
x2 <-10.5
x3 <-  'female'
x4 <-  "male"
x5 <-  TRUE
X6 <-  FALSE

# 2.1 데이터의 유형을 알려주는 함수----
# (1) mode(data)
mode(x1)
mode(x3)
mode(x5)
mode(x2)

# (2) is.xxxx(data)
is.numeric(x1)
is.numeric(x3)
is.character(x3)
is.logical(x5)

# 2.2 강제적으로 데이터 유형 변경하기----
# as.xxxx(data)
x1 <- 10
x2 <- "10"
x3 <- "hqq"
x4 <- FALSE


as.numeric(x2)
as.numeric(x3)
as.character(x1)
as.character(x4)

as.logical(x1)
as.logical(x2)
as.logical(x3)

#2.3 우선순위
# Character > Numeric












