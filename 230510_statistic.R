# 분석 파일 
library(tidyverse)
library(car)
library(caret)

# 데이터 로드 ------------------------------------------------------------------
getwd()
setwd('C:/Users/HPE/데면대면')
rm(list=ls())

SRD <- read.csv('school_report_data.csv')

plot(SRD[, -1])

# 메인 주제 1 -----------------------------------------------------------------

# 소주제 1 -------------------------------------------------------------------
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


# 소주제 2 -------------------------------------------------------------------
## rpt_violence
boxplot(SRD$rpt_violence)
boxplot(SRD$cnt_bell)
plot(SRD$rpt_violence ~ SRD$cnt_bell)

lm1_2 <- lm(rpt_violence ~ cnt_bell, data = SRD)

# 정규성 검정 

# 등분산성 검정
car::ncvTest(lm1_2)

# 단순 선형 회귀 분석
summary(lm1_2)


# 메인 주제 2 -----------------------------------------------------------------
SRD2 <- SRD %>% select(-cnt_motel, -cnt_accm_living, -cnt_accm_travel, -cnt_bell)
plot(SRD2[, -c(1:2)])

# 성인 게임장에 대해 이상치로 보였던 관측치 3개의 구 이름 도출 
SRD2 %>% arrange(desc(cnt_adultgame)) %>% head(3)
# 술집에 대해 이상치로 보였던 관측치 3개의 구 이름 도출 
SRD2 %>% arrange(desc(cnt_bar)) %>% head(3)
# 클럽에 대해 이상치로 보였던 관측치 3개의 구 이름 도출 
SRD2 %>% arrange(desc(cnt_club)) %>% head(3)

preProcess(SRD2[, -1])
SRD2[, -c(1:4)] <- SRD2 %>% select(-signgu, -rpt_total, -rpt_youthcrime, -rpt_violence) %>% 
  preProcess(method = c('range')) %>% 
  predict(SRD2[, -c(1:4)])


# 소주제 1 -------------------------------------------------------------------
## rpt_youthcrime
lm2_1 <- lm(rpt_youthcrime ~ cnt_danran + cnt_adultgame + cnt_bar + cnt_yuheung + cnt_club, data = SRD2)
summary(lm2_1)
lm2_1 <- step(lm2_1, direction = 'both')
summary(lm2_1)
# 설명력이 너무 낮아 모델 폐기

# # 정규성 검정 
# qqnorm(SRD2$cnt_danran)
# qqline(SRD2$cnt_danran, col = 'red')
# shapiro.test(SRD2$cnt_danran)
# # 정규성 만족 못함.
# 
# # 등분산성 검정
# car::ncvTest(lm2_1)
# 
# # 유의한 변수는 단란 주점 수 뿐이므로 독립성 검정을 하지 않는다.

# 소주제 2 -------------------------------------------------------------------
## rpt_violence
lm2_2 <- lm(rpt_violence ~ cnt_danran + cnt_adultgame + cnt_bar + cnt_yuheung + cnt_club, data = SRD2)
summary(lm2_2)
lm2_2 <- step(lm2_2, direction = 'both')
summary(lm2_2)
# 유의한 모델이 없으므로 모든 변수가 학교 폭력 신고 수에 선형 영향을 끼치지 않는다.


# 메인 주제 3 -----------------------------------------------------------------



# 메인 주제 4 -----------------------------------------------------------------


