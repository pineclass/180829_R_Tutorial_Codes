# 아이스크림 섭취량과 아이스크림의 가격, 가족의 월수입, 평균기온의 관계를 연구하기 위해
# 다음의 자료를 수집했다. 
load("icecream.rda")

if (!require(psych)) install.packages("psych"); library(psych)
pairs.panels(ice[,1:4])

r.full = lm(cons~price+income+temp, data=ice)
summary(r.full)

# model selection-stepwise
step(r.full, direction="both")
r.reduce = lm(cons~income+temp, data=ice)

anova(r.full, r.reduce) # 두 모형에 차이가 없다. price 설명변수를 제거해도 된다.

# Q1. 다중공선성 확인

# Q2. 정규성 확인

# Q3. 독립성 확인

# Q4. 등분산성 확인

# Q5. 영향점, 이상점, 지렛대점 확인

# Q6. 이상점을 제거한 회귀모형은?