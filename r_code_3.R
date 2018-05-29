getwd()
# setwd("")

# https://bit.ly/2GQNUTn

install.packages("stringr")
install.packages("dplyr")
install.packages("corrplot")
install.packages("psych")
install.packages("GGally")
install.packages("lubridate")
install.packages('DataCombine')
install.packages('ggplot2')
install.packages('car')

####.############### ####
####.# 1. 데이터 준비 # ####
####.############### ####

#### __ ● 데이터 읽기 ####


#### __ ● 데이터 설명 ####
# 자전거 공유 분석 프로젝트 담당자
# 미국 워싱턴 D.C.에서 "자전거 대여 사업"을 하는 회사
# 2011년 시작되어 지속적인 성장세를 누리고 있음
# 날씨, 휴일, 시간 등 자전거 대여와 관련된 정보,  고객 및 거래 정보,
# 오픈소스 데이터 등을 활용하여 "사업 현황을 분석"하고자 함


#### [01] Summarize ####
# 데이터 요약

#### __ ● 데이터 이슈 찾기 ####
# NA가 있는가?

######## [02] Fix ####
# 오류값 수정 
bad_data <- str_subset(bike$humidity, "[a-z A-Z]")
bad_data
location <- str_detect(bike$humidity, bad_data)
bike[location, ]
bike$humidity <- str_replace_all(bike$humidity, bad_data, "61")
bike[location, ]

#### [03] Convert ####
# 데이터 분석에 적합하도록 데이터 속성 변환

#### __ ● humidity를 문자형에서 숫자형으로 수정 ####


#### __ ● factor형으로 바꿀 항목 검토 ####
bike$holiday <- factor(bike$holiday, 
                       levels = c(0,1),
                       labels = c("no", "yes")) 
bike$workingday <- factor(bike$workingday, 
                          levels = c(0, 1),
                          labels = c("no", "yes"))



#### __ ● ordered factor형으로 바꿀 항목 검토 #### 
bike$weather <- factor(bike$weather, 
                       levels = c(1,2,3,4),
                       labels = c('clr_part_cloud', 'mist_cloudy', 'lt_rain_snow', 'hvy_rain_snow'),
                       ordered = TRUE)
bike$season <- factor(bike$season, 
                      levels = c(1,2,3,4),
                      labels = c('spring', 'summer', 'fall', 'winter'),
                      ordered = TRUE)


#### __ ● 날짜와 시간 항목 검토 #### 
library(lubridate)

#### [04] Adapt ####
# 문자열 데이터를 표준형으로 가공 
str(bike)
#### __ ● 범주형으로 변환할 것인가? #### 
#### __ ● 분석에 활용할 가치가 있는가? ####
#### __ ● 몇 개의 출처가 존재하고 몇 개로 분류하는 것이 적절한가 #### 

# 소문자 변환

# 문자열 앞, 뒤의 공백문자 제거

library(DataCombine)
web_sites <- "(www.[a-z]*.[a-z]*)"
current <- unique(str_subset(bike$sources, web_sites))
replace <- rep('web', length(current))
replacements <- data.frame(from = current, to = replace)
bike <- FindReplace(data = bike, Var = 'sources', replacements,from = 'from', to = 'to', exact = FALSE)
unique(bike$sources)
bike$sources <- as.factor(bike$sources)

table(bike$sources)

####.############### ####
####.# 2. 탐색적 데이터 분석 # ####
####.############### ####

#### __ ● 상황 ####
# 자전거 공유 데이터를 바탕으로, 마케팅 부서의 담당자와 데이터 분석을 통하여
# 의사 결정에 도움이 될 내용을 이해하고자 함
# 172개 자전거 대여 업체의 정보를 입수
# google_adwords : 구글 AdWords, facebook : 페이스북 광고, twitter : 트위터 광고 등에 대한 비용
# marketing_total : 총 마케팅 예산, revenues : 매출, employees : 종업원수, pop_density : 타켓 시장의 인구밀도 수준
# 비용은 1 = 1000$을 뜻한다.

#### [01] 단일 변수 분석 ####
marketing <- read.csv("marketing.csv", 
                      stringsAsFactors = FALSE)


#### __ ● 인구밀도 그래프 ####
marketing$pop_density <- factor(marketing$pop_density, 
                                ordered = TRUE, 
                                levels = c("Low", "Medium", "High"))

#### __ ● 구글 애드워즈 마케팅 비용 그래프 ####
# 평균

# 분산

# 표준편차

# summarise

# density plot

par(mfrow = c(1,2))
# 상자수염그림

# 히스토그램


#### __ ● 트위터 마케팅 비용 그래프 ####
# summarise

# 상자수염그림

# 히스토그램

# density plot
par(mfrow=c(1,1))


#### [02] 이변량 분석 ####
# 두 개의  변수 분석

#### __ ● 데이터 요약 ####


# temp_factor 변수 추가



#### __ ● 두 변수 사이의 관계 ####

par(mfrow = c(1,3))
# Factor / Factor
mosaicplot(table(marketing$pop_density, marketing$emp_factor),col = c('gray', 'black'), main = 'Factor / Factor')
# Factor / Numeric
boxplot(marketing$marketing_total ~ marketing$pop_density,main = 'Factor / Numeric')
# Numeric / Numeric'
plot(marketing$google_adwords, marketing$revenues,main = 'Numeric / Numeric')

#### __ ● 두 변수 사이의 상관관계 ####


par(mfrow = c(2,2))
plot(marketing$google_adwords, marketing$revenues,main = paste0('Google AdWords vs Revenues : ',c1))
plot(marketing$facebook, marketing$revenues,main = paste0('Facebook vs Revenues : ',c2))
plot(marketing$twitter, marketing$revenues,main = paste0('Twitter vs Revenues : ', c3))
plot(marketing$google_adwords, marketing$facebook,main = paste0('Google AdWords vs Facebook : ',c4))

#### __ ● 유의성 판단 ####
cor.test(marketing$google_adwords, marketing$revenues)
cor.test(marketing$google_adwords, marketing$facebook)
cor.test(marketing$twitter, marketing$revenues)
cor.test(marketing$facebook, marketing$facebook)
cor.test(marketing$marketing_total, marketing$revenues)

# 임시변수 삭제
marketing$emp_factor <- NULL 

#### [03] 다변량 분석 ####
# 다수의  변수 분석

#### __ ● 데이터 관찰 ####
# 데이터가 어떻게 생겼는가? : str() 함수

#### __ ● 데이터간 상관관계 ####


par(mfrow=c(1,1))
library("corrplot")

corrplot(cor_matrix, method = "color", addCoef.col = TRUE, 
         order = "hclust", hclust.method = "ward.D",
         outline = TRUE, tl.srt = 45)

#### __ ● 데이터 시각화 ####
# 데이터간 관계는? : pairs() 함수

# fancy한 그래프
library(GGally)
library(ggplot2)


####.############### ####
####.# 3. 회귀 분석 # ####
####.############### ####
#### [01] 단순 선형 회귀 ####

par(mfrow = c(1,1))
plot(adverts$marketing_total, adverts$revenues, ylab = 'Revenues',
     xlab = 'marketing Total', main = 'Reveneues and Marketing')

# model <- lm(Y ~ X, data = dataset)

#### __ ● 단순 선형 회귀로 미지의 값 예측 ####
library(dplyr)

# 마케팅 비용은 460만큼 사용했을 경우에 수익이 얼마나 날까?
newdata <- data.frame(marketing_total = 460)
predict.lm(model1, newdata, interval = 'predict')
# 100회 예측하면 95번은 49 ~ 62 사이에 수익이 위치한다.

predict.lm(model1, newdata, interval = 'predict', level = 0.99)
predict.lm(model1, newdata, interval = 'predict')

newdata <- data.frame(marketing_total = c(450, 460, 470))
data = cbind(x=newdata,predict.lm(model1, newdata, interval = 'predict'))
ggplot(adverts, aes(x=marketing_total, y=revenues)) + 
  geom_point() +
  geom_smooth(method = lm, se=TRUE) + 
  geom_point(data = data,aes(x=marketing_total, y=fit), color = 'red') + 
  geom_point(data = data,aes(x=marketing_total, y=lwr), color = 'green') + 
  geom_point(data = data,aes(x=marketing_total, y=upr), color = 'yellow')

#### __ ● 신뢰 구간 ####
# 우리가 현실에서 얻을 수 있는 데이터는 표본일 수 밖에 없다!
# 표본으로 전체를 예측할 수 있는가?
# 30% 무작위 샘플 추출
# 표본집단을 가지고 모집단을 예측하자


# 실제 model1이랑 비교

# 우리가 현실에서 얻을 수 있는 데이터는 표본일 수 밖에 없다!
# 표본으로 전체를 예측할 수 있는가?

#### [02] 다중 선형 회귀 분석 ####
# model <- lm(Y ~ X1 + X2 + …, data = dataset)


####.############### ####
####.# 4. 더 공부하기 # ####
####.############### ####
#### [01] 책 ####
#### __ ● R을 활용한 비즈니스 인텔리전스 ####
#### [02] r 패키지 ####
#### __ ● dplyr, data.table(데이터 핸들링) ####
#### __ ● ggplot2(시각화) ####
#### __ ● lubridate(시계열) ####
#### __ ● caret(기계학습) ####
