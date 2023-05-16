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
plot(lm1_1, 3)
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
plot(lm1_2, 3)
car::ncvTest(lm1_2)

# 단순 선형 회귀 분석
summary(lm1_2)
# 비상벨 개수와 학교 폭력 신고 수 사이에 선형 관계가 없다.







# 메인 주제 2 -----------------------------------------------------------------
SRD2 <- SRD %>% select(-cnt_motel, -cnt_accm_living, -cnt_accm_travel, -cnt_bell)
plot(SRD2[, -c(1:2)])

# 성인 게임장에 대해 이상치로 보였던 관측치 3개의 구 이름 도출 
SRD2 %>% arrange(desc(cnt_adultgame)) %>% head(3)
# 술집에 대해 이상치로 보였던 관측치 3개의 구 이름 도출 
SRD2 %>% arrange(desc(cnt_bar)) %>% head(3)
# 클럽에 대해 이상치로 보였던 관측치 3개의 구 이름 도출 
SRD2 %>% arrange(desc(cnt_club)) %>% head(3)

# preProcess(SRD2[, -1])
# SRD2[, -c(1:4)] <- SRD2 %>% select(-signgu, -rpt_total, -rpt_youthcrime, -rpt_violence) %>% 
#   preProcess(method = c('range')) %>% 
#   predict(SRD2[, -c(1:4)])


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

# 소주제 1 -------------------------------------------------------------------
SRD3 <- read.csv('school_report_data_korea.csv')
SRD3_rm_zero <- SRD3 %>% filter(cnt_bell > 0)

# 소주제 1 - 청소년 비행 신고 빈도와 비상벨 개수의 관계 ----------------------------------------
plot(rpt_youthcrime ~ cnt_bell, data = SRD3_rm_zero)
lm3_1_1 <- lm(rpt_youthcrime ~ cnt_bell, data = SRD3_rm_zero)
summary(lm3_1_1)
# 정규성 검정 
qqnorm(SRD3_rm_zero$cnt_bell)
qqline(SRD3_rm_zero$cnt_bell, col='red')
shapiro.test(SRD3_rm_zero$cnt_bell)
# 등분산성 검정 
plot(lm3_1_1, 3)
car::ncvTest(lm3_1_1)


# 소주제 1 - 학교 폭력 신고 빈도와 비상벨 개수의 관계 -----------------------------------------
plot(rpt_violence ~ cnt_bell, data = SRD3_rm_zero)
lm3_1_2 <- lm(rpt_violence ~ cnt_bell, data = SRD3_rm_zero)
summary(lm3_1_2)
# 등분산성 검정 
plot(lm3_1_2, 3)
car::ncvTest(lm3_1_2)


# 소주제 2 -------------------------------------------------------------------
SRD3_2 <- SRD3 %>% select(-cnt_motel, -cnt_accm_living, -cnt_accm_travel)
plot(SRD3_2[, -c(1:3)])

# 소주제 2 - 청소년 비행 ----------------------------------------------------------
## 전국 유흥 시설 개수와 청소년 비행 신고 수의 상관관계 
lm3_2_1 <- lm(rpt_youthcrime ~ cnt_danran + cnt_adultgame + cnt_bar + cnt_yuheung + cnt_club, data = SRD3_2)
summary(lm3_2_1)
lm3_2_1 <- step(lm3_2_1, direction = 'both')
summary(lm3_2_1)
# 정규성 검정
plot(lm3_2_1, 2)
shapiro.test(lm3_2_1$residuals)
# 등분산성 검정
plot(lm3_2_1, 3)
car::ncvTest(lm3_2_1)
# 다중공선성 
car::vif(lm3_2_1)

# 소주제 2 - 학교 폭력 -----------------------------------------------------------
## 전국 유흥 시설 개수와 학교 폭력 신고 수의 상관관계 
lm3_2_2 <- lm(rpt_violence ~ cnt_danran + cnt_adultgame + cnt_bar + cnt_yuheung + cnt_club, data = SRD3_2)
summary(lm3_2_2)
lm3_2_2 <- step(lm3_2_2, direction = 'both')
summary(lm3_2_2)
# 등분산성 검정
plot(lm3_2_2, 3)
car::ncvTest(lm3_2_2)
# 다중공선성 
car::vif(lm3_2_2)

# 시각화
SRD3_2$cnt_total <- apply(SRD3_2[, c(6:10)], 1, sum)

plot(rpt_youthcrime ~ cnt_total, data = SRD3_2)
plot(rpt_youthcrime ~ cnt_total, data = SRD3_2,
     xlim = c(0, 250),
     ylim = c(0, 300))

# # 이상치 제거 후 분석 
# SRD3_2_rm <- SRD3_2 %>% arrange(cnt_danran) %>% head(238)
# SRD3_2_rm <- SRD3_2_rm %>% arrange(cnt_adultgame) %>% head(228)
# SRD3_2_rm <- SRD3_2_rm %>% arrange(cnt_bar) %>% head(218)
# SRD3_2_rm <- SRD3_2_rm %>% arrange(cnt_yuheung) %>% head(208)
# SRD3_2_rm <- SRD3_2_rm %>% arrange(cnt_club) %>% head(198)
# SRD3_2_rm <- SRD3_2_rm %>% arrange(rpt_youthcrime) %>% head(188)
# # 회귀분석
# lm3_2_1 <- lm(rpt_youthcrime ~ cnt_danran + cnt_adultgame + cnt_bar + cnt_yuheung + cnt_club, data = SRD3_2_rm)
# lm3_2_1 <- step(lm3_2_1, direction = 'both')
# summary(lm3_2_1)
# # 정규성 검정
# shapiro.test(lm3_2_1$residuals)
# # 정규성 만족 못함
# 
# # 독립변수 표준화 및 종속변수에 log 변환
# SRD3_2_log <- SRD3_2
# ## 표준화
# preProcess(SRD3_2_log[, c(6:10)])
# SRD3_2_log[, c(6:10)] <- SRD3_2_log[, c(6:10)] %>% 
#   preProcess(method = c('range')) %>% 
#   predict(SRD3_2_log[, c(6:10)])
# ## 로그변환 
# log_3_2_1 <- SRD3_2_log %>% select(rpt_youthcrime) %>% filter(rpt_youthcrime != 0) %>% min()
# SRD3_2_log$rpt_youthcrime[SRD3_2_log$rpt_youthcrime == 0] <- log_3_2_1/2
# # 회귀분석
# lm3_2_1_log <- lm(log(rpt_youthcrime) ~ cnt_danran + cnt_adultgame + cnt_bar + cnt_yuheung + cnt_club, data = SRD3_2_log)
# lm3_2_1_log <- step(lm3_2_1_log, direction = 'both')
# summary(lm3_2_1_log)
# # 정규성 검정
# shapiro.test(lm3_2_1_log$residuals)
# # 여전히 정규성 만족하지 않음 
# 
# # 이상치 제거 후 독립변수 표준화 및 종속변수에 log 변환 후 분석 
# SRD3_2_rm_log <- SRD3_2 %>% arrange(cnt_danran) %>% head(238)
# SRD3_2_rm_log <- SRD3_2_rm_log %>% arrange(cnt_adultgame) %>% head(228)
# SRD3_2_rm_log <- SRD3_2_rm_log %>% arrange(cnt_bar) %>% head(218)
# SRD3_2_rm_log <- SRD3_2_rm_log %>% arrange(cnt_yuheung) %>% head(208)
# SRD3_2_rm_log <- SRD3_2_rm_log %>% arrange(cnt_club) %>% head(198)
# SRD3_2_rm_log <- SRD3_2_rm_log %>% arrange(rpt_youthcrime) %>% head(188)
# ## 표준화
# preProcess(SRD3_2_rm_log[, c(6:10)])
# SRD3_2_rm_log[, c(6:10)] <- SRD3_2_rm_log[, c(6:10)] %>% 
#   preProcess(method = c('range')) %>% 
#   predict(SRD3_2_rm_log[, c(6:10)])
# ## 로그변환 
# log_3_2_1 <- SRD3_2_rm_log %>% select(rpt_youthcrime) %>% filter(rpt_youthcrime != 0) %>% min()
# SRD3_2_rm_log$rpt_youthcrime[SRD3_2_rm_log$rpt_youthcrime == 0] <- log_3_2_1/2
# # 회귀분석
# lm3_2_1 <- lm(rpt_youthcrime ~ cnt_danran + cnt_adultgame + cnt_bar + cnt_yuheung + cnt_club, data = SRD3_2_rm_log)
# lm3_2_1 <- step(lm3_2_1, direction = 'both')
# summary(lm3_2_1)
# # 정규성 검정
# shapiro.test(lm3_2_1$residuals)
# # 정규성 만족 못함
# 
# ## 전국 유흥 시설 개수와 청소년 학교 폭력 신고 수의 상관관계 
# lm3_2_2 <- lm(rpt_violence ~ cnt_danran + cnt_adultgame + cnt_bar + cnt_yuheung + cnt_club, data = SRD3_2)
# lm3_2_2 <- step(lm3_2_2, direction = 'both')
# summary(lm3_2_2)
# # 정규성 검정
# plot(lm3_2_2, 2)
# shapiro.test(lm3_2_2$residuals)
# # 등분산성 검정
# plot(lm3_2_2, 3)
# car::ncvTest(lm3_2_2)
# # 다중공선성 
# car::vif(lm3_2_2)








# 메인 주제 4 -----------------------------------------------------------------
## 전국 유흥 시설 개수와 비상벨 개수의 상관관계 분석 
SRD4 <- read.csv('school_report_data_korea.csv')
SRD4_rm_zero <- SRD4 %>% filter(cnt_bell > 0)
plot(SRD4_rm_zero[,-c(1:5)])

SRD4_rm_zero <- SRD4_rm_zero %>% 
  select(cnt_danran, cnt_adultgame, cnt_bar, cnt_yuheung, cnt_club, cnt_bell)

preProcess(SRD4_rm_zero[, -6])
SRD4_rm_zero[, -6] <- SRD4_rm_zero[, -6] %>%
  preProcess(method = c('range')) %>%
  predict(SRD4_rm_zero[, -6])

SRD4_rm_zero$cnt_total <- apply(SRD4_rm_zero[, -6], 1, sum)

plot(cnt_bell ~ cnt_total, data = SRD4_rm_zero)

lm4 <- lm(cnt_bell ~ cnt_total, data = SRD4_rm_zero)
summary(lm4)
