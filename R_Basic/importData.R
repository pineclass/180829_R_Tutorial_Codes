# ��Ű�� ��ġ�ϱ�
# ���� : ���ͳ��� ����Ǿ� �־�� ��.
# install.package("package name")
install.packages("readxl")
install.packages("d:/RBasic/brt_1.2.0.zip",
                 repos = NULL)
library(brt)




# ��Ű�� �ε��ϱ�
# ��Ű���� 
#
# �ʿ��� ������ ������ �־�� ��
# library(package name)
library(readxl)


# �ܺ� ������ �о����

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
# ������ Ư���� ����
# dataname <-  


# csv

traffic <- read.csv(file   = "d:/RBasic/traffic.csv",
                    header = TRUE)
traffic



# 3. excel----
# r�Ǳ⺻��ɿ����� �� �о��
# ���ο� ��Ű���� ��ġ �ε�.

# ��Ű���� ; readxl
# ��Ű��(package)
# 1. �Լ�(Function) *****
# 2. ������(data)
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




# 4. �۾�����(working directory)----
# 4.1 ���� ������ �۾����� �˾Ƴ���----
# getwd()
getwd()

# 4.2 ���Ӱ� �۾����� �����ϱ�
# setwd("dric)
setwd("d:/RBasic/")

reading3 <- readxl::read_excel(path      = "reading.xlsx",
                               sheet     = 1,
                               col_names = TRUE)
reading3

