# 분석 파일 
library(car)

# 데이터 로드 ------------------------------------------------------------------
getwd()
setwd('C:/Users/HPE/데면대면')
rm(list=ls())

SRD <- read.csv('school_report_data.csv')

plot(SRD[, -1])

# 메인 주제 1 -----------------------------------------------------------------
## rpt_youthcrime
boxplot(SRD$rpt_youthcrime)
boxplot(SRD$cnt_bell)
plot(SRD$rpt_youthcrime ~ SRD$cnt_bell)

lm1_1 <- lm(rpt_youthcrime ~ cnt_bell, data = SRD)

# 정규성 검정 
qqnorm(SRD$cnt_bell)
qqline(SRD$cnt_bell, col = 'red')
shapiro.test(SRD$cnt_bell)

# 등분산성 검정
car::ncvTest(lm1_1)

# 단순 선형 회귀 분석
summary(lm1_1)
# 비상벨 개수와 청소년 비행 신고 수 사이에 선형 관계가 없다.


## rpt_violence
boxplot(SRD$rpt_violence)
boxplot(SRD$cnt_bell)
plot(SRD$rpt_violence ~ SRD$cnt_bell)

lm1_2 <- lm(rpt_violence ~ cnt_bell, data = SRD)

# 정규성 검정 
qqnorm(SRD$cnt_bell)
qqline(SRD$cnt_bell, col = 'red')
shapiro.test(SRD$cnt_bell)

# 등분산성 검정
car::ncvTest(lm1_2)

# 단순 선형 회귀 분석
summary(lm1_2)
