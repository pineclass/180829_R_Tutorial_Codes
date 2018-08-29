setwd("C:/Users/USER/Dropbox/R-work/2018-intermediate/R_beginner/data")

mydata = read.csv("anova_two_way.csv") 
head(mydata)

table(mydata$ad_layer, mydata$multichild) #교차분할표 만들기
TAB = table(mydata$ad_layer,mydata$multichild)

margin.table(TAB, 1) # A frequencies (summed over B) 
margin.table(TAB, 2) # B frequencies (summed over A)

prop.table(TAB) # cell percentages
prop.table(TAB, 1) # row percentages 
prop.table(TAB, 2) # column percentages 

chisq.test(TAB) #교차검정 실시

# assocplot(TAB) #assocplot을 통해 시각


#만약 SPSS와 유사한 결과를 실행하고자 할 
if(!require(descr)) install.packages("descr"); library(descr)

ct <-crosstab(mydata$ad_layer, mydata$multichild, expected = TRUE,
         prop.c = T, prop.r=T, prop.t =T, chisq = T)
ct

# 교차분할표를 시각화시키는 그림은 mosaicplot
HairEyeColor
mosaicplot(HairEyeColor, shade=TRUE, legend=TRUE) 
