getwd()
setwd("")

install.packages("stringr")
install.packages("dplyr")
install.packages("corrplot")
install.packages("psych")
install.packages("GGally")
install.packages("lubridate")
install.packages('DataCombine')

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

cat("
    Attribute Information:
    
    - instant: record index
    - dteday : date
    - season : season (1:springer, 2:summer, 3:fall, 4:winter)
    - yr : year (0: 2011, 1:2012)
    - mnth : month ( 1 to 12)
    - hr : hour (0 to 23)
    - holiday : weather day is holiday or not (extracted from [Web Link])
    - weekday : day of the week(평일)
    - workingday : if day is neither weekend(주말) nor holiday(공휴일) is 1, otherwise is 0.
    + weathersit : 
    - 1: Clear, Few clouds, Partly cloudy
    (맑음, 흐림 없음, 약간 흐림 )
    - 2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
    (안개 + 흐림, 안개 + 구름, 안개)
    - 3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds
    (눈, 비 + 뇌우 + 구름, 비 + 구름 )
    - 4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog
    (폭우 + 뇌우 + 안개, 눈 + 안개 )
    - temp : Normalized temperature in Celsius(기온). The values are derived via (t-t_min)/(t_max-t_min), t_min=-8, t_max=+39 (only in hourly scale)
    - atemp: Normalized feeling temperature in Celsius(체감기온). The values are derived via (t-t_min)/(t_max-t_min), t_min=-16, t_max=+50 (only in hourly scale)
    - hum: Normalized humidity(습도). The values are divided to 100 (max)
    - windspeed: Normalized wind speed(바람세기). The values are divided to 67 (max)
    - casual: count of casual users(임시 사용자 수)
    - registered: count of registered users(등록된 사용자 수)
    - cnt: count of total rental bikes including both casual and registered(임시 및 등록 된 자전거를 포함한 총 자전거 수)
    - sources : 다수가 어디서 광고를 보고 자전거를 대여하려고 왔는지!!
    ")

#### [01] Summarize ####
# 데이터 요약


#### __ ● 데이터 이슈 찾기 ####
# NA가 있는가?

######## [02] Fix ####
# 오류값 수정 

#### [03] Convert ####
# 데이터 분석에 적합하도록 데이터 속성 변환

#### __ ● humidity를 문자형에서 숫자형으로 수정 ####

#### __ ● factor형으로 바꿀 항목 검토 ####

#### __ ● ordered factor형으로 바꿀 항목 검토 #### 

#### __ ● 날짜와 시간 항목 검토 #### 

#### [04] Adapt ####
# 문자열 데이터를 표준형으로 가공 
#### __ ● 범주형으로 변환할 것인가? #### 
#### __ ● 분석에 활용할 가치가 있는가? ####
#### __ ● 몇 개의 출처가 존재하고 몇 개로 분류하는 것이 적절한가 #### 
# 소문자 변환
# 문자열 앞, 뒤의 공백문자 제거



# install.packages('DataCombine')
library(DataCombine)



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


#### __ ● 인구밀도 그래프 ####


#### __ ● 구글 애드워즈 마케팅 비용 그래프 ####
# 평균

# 분산

# 표준편차

# summarise

# density plot


# 상자수염그림

# 히스토그램

#### __ ● 트위터 마케팅 비용 그래프 ####
# summarise

# 상자수염그림

# 히스토그램


#### [02] 이변량 분석 ####
# 두 개의  변수 분석

#### __ ● 데이터 요약 ####

# temp_factor 변수 추가



#### __ ● 두 변수 사이의 관계 ####

# Factor / Factor

# Factor / Numeric

# Numeric / Numeric'


#### __ ● 두 변수 사이의 상관관계 ####

#### __ ● 유의성 판단 ####

# 임시변수 삭제

#### [03] 다변량 분석 ####
# 다수의  변수 분석

#### __ ● 데이터 관찰 ####
# 데이터가 어떻게 생겼는가? : str() 함수

#### __ ● 데이터 시각화 ####
# 데이터간 관계는? : pairs() 함수

# fancy한 그래프

#### __ ● 데이터간 상관관계 ####


#### __ ● 유의성 판단 ####
# install.packages("psych")
library(psych)

# install.packages("corrplot")
library("corrplot")

####.############### ####
####.# 3. 회귀 분석 # ####
####.############### ####
#### [01] 단순 선형 회귀 ####

# model <- lm(Y ~ X, data = dataset)


#### __ ● 단순 선형 회귀로 미지의 값 예측 ####

# 마케팅 비용은 46만큼 사용했을 경우에 수익이 얼마나 날까?

# 100회 예측하면 95번은 49 ~ 62 사이에 수익이 위치한다.


#### __ ● 신뢰 구간 ####
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
