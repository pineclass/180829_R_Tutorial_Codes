# 패키지 설치하기
# 조건 : 인터넷이 연결되어 있어야 함.
# install.package("package name")
install.packages("readxl")
install.packages("d:/RBasic/brt_1.2.0.zip",
                 repos = NULL)
library(brt)




# 패키지 로딩하기
# 패키지와 
#
# 필요할 때마다 실행해 주어야 함
# library(package name)
library(readxl)


# 외부 데이터 읽어오기

# txt, csv, excel, DB

# separator : blank----

read.table(file = "d:/filename.txt", header=)

blank <- read.table(file   = "d:/RBasic/blank.txt",
                    header = TRUE, 
                    sep    = " ")
blank

blank2 <- read.table(file   = "d:/RBasic/blank.txt",
                    header = FALSE, 
                    sep    = " ")
blank2

# separator : comma----

read.table(file = "d:/filename.txt", header=)

comma <- read.table(file   = "d:/RBasic/comma.txt",
                    header = TRUE, 
                    sep    = ",")
comma

# separator : tab----

read.table(file = "d:/filename.txt", header=)

tab <- read.table(file   = "d:/RBasic/tab.txt",
                  header = TRUE, 
                  sep    = "\t")
tab





# 2. csv : comma separator value----
# 엑셀의 특수한 형태
# dataname <-  


# csv

traffic <- read.csv(file   = "d:/RBasic/traffic.csv",
                    header = TRUE)
traffic



# 3. excel----
# r의기본기능에서는 못 읽어옴
# 새로운 패키지를 설치 로딩.

# 패키지명 ; readxl
# 패키지(package)
# 1. 함수(Function) *****
# 2. 데이터(data)
# 3. 
# 4. 






# dataname <- readxl::read_excel(path = "directory/filename.xlsx",
#                                sheet = "sheet name" or index,
#                                col_names = TRUE)
reading <- readxl::read_excel(path      = "d:/RBasic/reading.xlsx",
                              sheet     = "data",
                              col_names = TRUE)
reading

reading2 <- readxl::read_excel(path      = "d:/RBasic/reading.xlsx",
                              sheet     = 1,
                              col_names = TRUE)
reading2




# 4. 작업공간(working directory)----
# 4.1 현재 설정된 작업공간 알아내기----
# getwd()
getwd()

# 4.2 새롭게 작업공간 설정하기
# setwd("dric)
setwd("d:/RBasic/")

reading3 <- readxl::read_excel(path      = "reading.xlsx",
                               sheet     = 1,
                               col_names = TRUE)
reading3


