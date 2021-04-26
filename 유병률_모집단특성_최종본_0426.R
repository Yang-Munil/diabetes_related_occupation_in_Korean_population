library(haven)
library(dplyr)
library(ggplot2)
library(gmodels)
library(agricolae)
library(survey)
library(car)
library(sqldf)
library(ROCR)
library(DT)
library(knitr)
library(moonBook)
library(jtools)
library(ggplot2)
library(interactions)
library(sandwich)
library(ResourceSelection)
library(stargazer)
library(effects)

# 데이터 불러오기

hn_all <- read.csv("C:/rproject/startr/hn_all.csv")


hn_all <- subset(hn_all, select = -c(X))

# 경우의 수 보기 위한 추가 전처리

# 연령

# 연령

#hn_all$age_cate[hn_all$age>=19 & hn_all$age<=29]=1
#hn_all$age_cate[hn_all$age>=30 & hn_all$age<=39]=2
#hn_all$age_cate[hn_all$age>=40 & hn_all$age<=49]=3
#hn_all$age_cate[hn_all$age>=50 & hn_all$age<=59]=4
#hn_all$age_cate[hn_all$age>=60 & hn_all$age<=69]=5
#hn_all$age_cate[hn_all$age>=70]=6

hn_all$age_cate[hn_all$age>=19 & hn_all$age<=39]=1
hn_all$age_cate[hn_all$age>=40 & hn_all$age<=49]=2
hn_all$age_cate[hn_all$age>=50 & hn_all$age<=59]=3
hn_all$age_cate[hn_all$age>=60 & hn_all$age<=69]=4
hn_all$age_cate[hn_all$age>=70]=5

# BMI 저체중 / 정상 / 비만으로 나누기.

hn_all$HE_BMI_cate = 1
hn_all$HE_BMI_cate[hn_all$HE_BMI>=18.5]=2
hn_all$HE_BMI_cate[hn_all$HE_BMI>=25]=3

# edu 초중/고/대 3개로 나누기

hn_all$edu3G[hn_all$edu == 1] <- 1
hn_all$edu3G[hn_all$edu == 2] <- 1
hn_all$edu3G[hn_all$edu == 3] <- 2
hn_all$edu3G[hn_all$edu == 4] <- 3

"### 범주형 factor 처리

hn_all$year <- as.factor(hn_all$year)

hn_all$occp <- as.factor(hn_all$occp)

hn_all$edu <- as.factor(hn_all$edu)

hn_all$edu3G <- as.factor(hn_all$edu3G)

hn_all$BS3_1 <- as.factor(hn_all$BS3_1)

hn_all$EC_wht_5 <- as.factor(hn_all$EC_wht_5)

hn_all$marri_1 <- as.factor(hn_all$marri_1)

hn_all$sm_presnt <- as.factor(hn_all$sm_presnt)

hn_all$dr_month <- as.factor(hn_all$dr_month)

hn_all$pa_walk <- as.factor(hn_all$pa_walk)

hn_all$pa_aerobic <- as.factor(hn_all$pa_aerobic)

hn_all$HE_BMI_band <- as.factor(hn_all$HE_BMI_band)

hn_all$HE_BMI_cate <- as.factor(hn_all$HE_BMI_cate)

# hn_all$age_cate <- as.factor(hn_all$age_cate)

# hn_all$age_band <- as.factor(hn_all$age_band)

hn_all$ainc_band <- as.factor(hn_all$ainc_band)

hn_all$incm <- as.factor(hn_all$incm)"

########### 직업군 3개로 묶기 ############

# Manual workers were elementary workers, craft and related trades workers, equipment, machine operation and assembling workers, and agricultural, forestry and fishery workers.

#  Non-manual workers consisted of clerks, professionals and related workers, managers.

# The rest are Service/sales workers.

# 1. 관리자, 전문가 및 관련 종사자
# 2. 사무종사자
# 3. 서비스 및 판매 종사자
# 4. 농림어업 숙련 종사자
# 5. 기능원, 장치․기계조작 및 조립종사자
# 6. 단순노무종사자
# 7. 무직(주부, 학생 등)

# Manual workers(4,5,6) = 1 / Non-manual workers(1,2) = 2 / Service/sales workers(3) = 3 / not employed(7) = 4
# 직업그룹 4개

hn_all$occp4G[hn_all$occp == 4] <- 1
hn_all$occp4G[hn_all$occp == 5] <- 1
hn_all$occp4G[hn_all$occp == 6] <- 1
hn_all$occp4G[hn_all$occp == 1] <- 2
hn_all$occp4G[hn_all$occp == 2] <- 2
hn_all$occp4G[hn_all$occp == 3] <- 3
hn_all$occp4G[hn_all$occp == 7] <- 4

#hn_all$occp4G <- as.factor(hn_all$occp4G)


#  농촌블루칼라(4) = 1 / white(1,2) = 2 / 도시블루칼라(5,6) = 3 / Service/sales workers(3) = 4 / not employed(7) = 5
# 직업그룹 5개

hn_all$occp5G[hn_all$occp == 4] <- 1
hn_all$occp5G[hn_all$occp == 5] <- 3
hn_all$occp5G[hn_all$occp == 6] <- 3
hn_all$occp5G[hn_all$occp == 1] <- 2
hn_all$occp5G[hn_all$occp == 2] <- 2
hn_all$occp5G[hn_all$occp == 3] <- 4
hn_all$occp5G[hn_all$occp == 7] <- 5

hn_all$occp5G <- as.factor(hn_all$occp5G)


# 데이터 분할(남녀)

hn_all_m <- hn_all[hn_all$sex == 1,]
hn_all_f <- hn_all[hn_all$sex == 2,]


# 성별마다 BMI 4분위수로 나눈 HE_BMI_4P 변수 새로 만들기.

# 남자 BMI 3분위수, 4분위수

round(quantile(hn_all_m$HE_BMI, c(0.335, 0.665)), 2) # 3등분
round(quantile(hn_all_m$HE_BMI, c(0.25, 0.50, 0.75)), 2) # 4등분

## 3등분(23.03, 25.56), 4등분(22.31, 24.27, 26.36) 기준으로 새로운 변수 만들기

# 3등분

hn_all_m$HE_BMI_3P = 1
hn_all_m$HE_BMI_3P[hn_all_m$HE_BMI>=23.03]=2
hn_all_m$HE_BMI_3P[hn_all_m$HE_BMI>=25.56]=3

hn_all_m$HE_BMI_3P <- as.factor(hn_all_m$HE_BMI_3P)

# 4등분

hn_all_m$HE_BMI_4P = 1
hn_all_m$HE_BMI_4P[hn_all_m$HE_BMI>=22.31]=2
hn_all_m$HE_BMI_4P[hn_all_m$HE_BMI>=24.27]=3
hn_all_m$HE_BMI_4P[hn_all_m$HE_BMI>=26.36]=4

hn_all_m$HE_BMI_4P <- as.factor(hn_all_m$HE_BMI_4P)


# 여자 BMI 3분위수, 4분위수

round(quantile(hn_all_f$HE_BMI, c(0.335, 0.665)), 2)
round(quantile(hn_all_f$HE_BMI, c(0.25, 0.50, 0.75)), 2) # 4등분

## 3등분(21.7, 24.6), 4등분(20.95, 23.07, 25.62) 기준으로 새로운 변수 만들기

# 3등분

hn_all_f$HE_BMI_3P = 1
hn_all_f$HE_BMI_3P[hn_all_f$HE_BMI>=21.7]=2
hn_all_f$HE_BMI_3P[hn_all_f$HE_BMI>=24.6]=3

hn_all_f$HE_BMI_3P <- as.factor(hn_all_f$HE_BMI_3P)

# 4등분

hn_all_f$HE_BMI_4P = 1
hn_all_f$HE_BMI_4P[hn_all_f$HE_BMI>=20.95]=2
hn_all_f$HE_BMI_4P[hn_all_f$HE_BMI>=23.07]=3
hn_all_f$HE_BMI_4P[hn_all_f$HE_BMI>=25.62]=4

hn_all_f$HE_BMI_4P <- as.factor(hn_all_f$HE_BMI_4P)



#################### 성별마다 영양변수 3,4,5 분위수로 나눈 변수 새로 만들기.


# 히스토그램 살펴보기

hist(hn_all_m$N_EN)
hist(hn_all_m$N_SUGAR)
hist(hn_all_m$N_FAT)

hist(hn_all_f$N_EN)
hist(hn_all_f$N_SUGAR)
hist(hn_all_f$N_FAT)


# 남자 영양변수 3분위수, 4분위수, 5분위수

round(quantile(hn_all_m$N_EN, c(0.335, 0.665)), 2) # 3분위수
round(quantile(hn_all_m$N_EN, c(0.25, 0.50, 0.75)), 2) # 4분위수
round(quantile(hn_all_m$N_EN, c(0.2, 0.4, 0.6, 0.8)), 2) # 5분위수

## 3등분(1824.06, 2557.07), 4등분(1653.06, 2165.99, 2800.62), 5등분(1544.14, 1954.64, 2390.76, 3003.06) 기준으로 새로운 변수 만들기

# 3등분 (1824.06, 2557.07)

hn_all_m$N_EN_3P = 1
hn_all_m$N_EN_3P[hn_all_m$N_EN>=1824.06]=2
hn_all_m$N_EN_3P[hn_all_m$N_EN>=2557.07]=3

hn_all_m$N_EN_3P <- as.factor(hn_all_m$N_EN_3P)

# 4등분 (1653.06, 2165.99, 2800.62)

hn_all_m$N_EN_4P = 1
hn_all_m$N_EN_4P[hn_all_m$N_EN>=1653.06]=2
hn_all_m$N_EN_4P[hn_all_m$N_EN>=2165.99]=3
hn_all_m$N_EN_4P[hn_all_m$N_EN>=2800.62]=4

hn_all_m$N_EN_4P <- as.factor(hn_all_m$N_EN_4P)

# 5등분 (1544.14, 1954.64, 2390.76, 3003.06)

hn_all_m$N_EN_5P = 1
hn_all_m$N_EN_5P[hn_all_m$N_EN>=1544.14]=2
hn_all_m$N_EN_5P[hn_all_m$N_EN>=1954.64]=3
hn_all_m$N_EN_5P[hn_all_m$N_EN>=2390.76]=4
hn_all_m$N_EN_5P[hn_all_m$N_EN>=3003.06]=5

hn_all_m$N_EN_5P <- as.factor(hn_all_m$N_EN_5P)

## 분위수는 불균형이 심함. 따라서 인원수로 4등분하기.


## 영양변수 인원수로 구간 나누기

summary(cut(hn_all_m$N_SUGAR, c(0,30,50,80)))
summary(cut(hn_all_m$N_EN, c(0,1650,2200,2800)))
summary(cut(hn_all_m$N_FAT, c(0,25,40,65)))


# 4등분 (1650, 2200, 2800)

hn_all_m$N_EN_4F = 1
hn_all_m$N_EN_4F[hn_all_m$N_EN>=1650]=2
hn_all_m$N_EN_4F[hn_all_m$N_EN>=2200]=3
hn_all_m$N_EN_4F[hn_all_m$N_EN>=2800]=4

hn_all_m$N_EN_4F <- as.factor(hn_all_m$N_EN_4F)

# 4등분 (30, 50, 80)

hn_all_m$N_SUGAR_4F = 1
hn_all_m$N_SUGAR_4F[hn_all_m$N_SUGAR>=30]=2
hn_all_m$N_SUGAR_4F[hn_all_m$N_SUGAR>=50]=3
hn_all_m$N_SUGAR_4F[hn_all_m$N_SUGAR>=80]=4

hn_all_m$N_SUGAR_4F <- as.factor(hn_all_m$N_SUGAR_4F)

# 4등분 (25, 40, 65)

hn_all_m$N_FAT_4F = 1
hn_all_m$N_FAT_4F[hn_all_m$N_FAT>=25]=2
hn_all_m$N_FAT_4F[hn_all_m$N_FAT>=40]=3
hn_all_m$N_FAT_4F[hn_all_m$N_FAT>=65]=4

hn_all_m$N_FAT_4F <- as.factor(hn_all_m$N_FAT_4F)









# 여자 영양변수(에너지) 3분위수, 4분위수, 5분위수

round(quantile(hn_all_f$N_EN, c(0.335, 0.665)), 2) # 3분위수
round(quantile(hn_all_f$N_EN, c(0.25, 0.50, 0.75)), 2) # 4분위수
round(quantile(hn_all_f$N_EN, c(0.2, 0.4, 0.6, 0.8)), 2) # 5분위수

## 3등분(1327.69, 1848.51), 4등분(1201.09, 1573.86, 2031.91), 5등분(1116.10, 1425.99, 1731.76, 2172.59) 기준으로 새로운 변수 만들기

# 3등분 (1327.69, 1848.51)

hn_all_f$N_EN_3P = 1
hn_all_f$N_EN_3P[hn_all_f$N_EN>=1327.69]=2
hn_all_f$N_EN_3P[hn_all_f$N_EN>=1848.51]=3

hn_all_f$N_EN_3P <- as.factor(hn_all_f$N_EN_3P)

# 4등분 (1201.09, 1573.86, 2031.91)

hn_all_f$N_EN_4P = 1
hn_all_f$N_EN_4P[hn_all_f$N_EN>=1201.09]=2
hn_all_f$N_EN_4P[hn_all_f$N_EN>=1573.86]=3
hn_all_f$N_EN_4P[hn_all_f$N_EN>=2031.91]=4

hn_all_f$N_EN_4P <- as.factor(hn_all_f$N_EN_4P)

# 5등분 (1116.10, 1425.99, 1731.76, 2172.59)

hn_all_f$N_EN_5P = 1
hn_all_f$N_EN_5P[hn_all_f$N_EN>=1116.10]=2
hn_all_f$N_EN_5P[hn_all_f$N_EN>=1425.99]=3
hn_all_f$N_EN_5P[hn_all_f$N_EN>=1731.76]=4
hn_all_f$N_EN_5P[hn_all_f$N_EN>=2172.59]=5

hn_all_f$N_EN_5P <- as.factor(hn_all_f$N_EN_5P)



# 여자 영양변수(당) 3분위수, 4분위수, 5분위수

round(quantile(hn_all_f$N_SUGAR, c(0.335, 0.665)), 2) # 3분위수
round(quantile(hn_all_f$N_SUGAR, c(0.25, 0.50, 0.75)), 2) # 4분위수
round(quantile(hn_all_f$N_SUGAR, c(0.2, 0.4, 0.6, 0.8)), 2) # 5분위수

## 3등분 (36.13, 64.47), 4등분 (29.20, 49.51, 75.46), 5등분 (25.42, 41.05, 58.12, 83.78) 기준으로 새로운 변수 만들기

# 3등분 (36.13, 64.47)

hn_all_f$N_SUGAR_3P = 1
hn_all_f$N_SUGAR_3P[hn_all_f$N_SUGAR>=36.13]=2
hn_all_f$N_SUGAR_3P[hn_all_f$N_SUGAR>=64.47]=3

hn_all_f$N_SUGAR_3P <- as.factor(hn_all_f$N_SUGAR_3P)

# 4등분 (29.20, 49.51, 75.46)

hn_all_f$N_SUGAR_4P = 1
hn_all_f$N_SUGAR_4P[hn_all_f$N_SUGAR>=29.20]=2
hn_all_f$N_SUGAR_4P[hn_all_f$N_SUGAR>=49.51]=3
hn_all_f$N_SUGAR_4P[hn_all_f$N_SUGAR>=75.46]=4

hn_all_f$N_SUGAR_4P <- as.factor(hn_all_f$N_SUGAR_4P)

# 5등분 (25.42, 41.05, 58.12, 83.78)

hn_all_f$N_SUGAR_5P = 1
hn_all_f$N_SUGAR_5P[hn_all_f$N_SUGAR>=25.42]=2
hn_all_f$N_SUGAR_5P[hn_all_f$N_SUGAR>=41.05]=3
hn_all_f$N_SUGAR_5P[hn_all_f$N_SUGAR>=58.12]=4
hn_all_f$N_SUGAR_5P[hn_all_f$N_SUGAR>=83.78]=5

hn_all_f$N_SUGAR_5P <- as.factor(hn_all_f$N_SUGAR_5P)


### 여자그룹 영양변수 생성 완료.



## 영양변수 인원수로 구간 나누기

summary(cut(hn_all_f$N_SUGAR, c(0,30,50,80)))
summary(cut(hn_all_f$N_EN, c(0,1200,1600,2000)))
summary(cut(hn_all_f$N_FAT, c(0,18,30,50)))

# 4등분 (1200, 1600, 2000)

hn_all_f$N_EN_4F = 1
hn_all_f$N_EN_4F[hn_all_f$N_EN>=1200]=2
hn_all_f$N_EN_4F[hn_all_f$N_EN>=1600]=3
hn_all_f$N_EN_4F[hn_all_f$N_EN>=2000]=4

hn_all_f$N_EN_4F <- as.factor(hn_all_f$N_EN_4F)

# 4등분 (30, 50, 80)

hn_all_f$N_SUGAR_4F = 1
hn_all_f$N_SUGAR_4F[hn_all_f$N_SUGAR>=30]=2
hn_all_f$N_SUGAR_4F[hn_all_f$N_SUGAR>=50]=3
hn_all_f$N_SUGAR_4F[hn_all_f$N_SUGAR>=80]=4

hn_all_f$N_SUGAR_4F <- as.factor(hn_all_f$N_SUGAR_4F)

# 4등분 (18, 30, 50)

hn_all_f$N_FAT_4F = 1
hn_all_f$N_FAT_4F[hn_all_f$N_FAT>=18]=2
hn_all_f$N_FAT_4F[hn_all_f$N_FAT>=30]=3
hn_all_f$N_FAT_4F[hn_all_f$N_FAT>=50]=4

hn_all_f$N_FAT_4F <- as.factor(hn_all_f$N_FAT_4F)






#######################변수별 당뇨 구성비를 보기 위한 전처리 데이터 생성 완료#########################



# 종속변수(HE_DM) 비율.

table(hn_all$HE_DM)

cat("total :", margin.table(table(hn_all$HE_DM)))

prop.table(table(hn_all$HE_DM))



# occp4G (1 - manual, 2 - non_manual, 3 - service, 4 - non_employee)

df_occp4G <- hn_all %>% group_by(occp4G) %>% summarise(total = n())

df_occp4G

df_occp4G_int <- hn_all %>% group_by(occp4G, HE_DM) %>% summarise(total = n())

df_occp4G_int

df_occp_per <- df_occp4G_int %>% group_by(occp4G) %>% mutate(percent = total / sum(total))

df_occp_per







###  TABLE 3


################ 남녀 각 각 나이대별 / 직업 4개 각각 유병률 계산 ################

##### 변수별 당뇨 비율을 보기 위해서는 더미변수형태가 아닌 구간으로 나뉘어져 연속형 변수 형태여야 가능.

## 나이

# age_cate (1,2,3,4,5)

df_age_cate_int_m_m <- hn_all_m %>% group_by(age_cate) %>% summarise(total = n())

head(df_age_cate_int_m_m)

df_age_cate_int_m <- hn_all_m %>% group_by(age_cate, HE_DM) %>% summarise(total = n())

head(df_age_cate_int_m)

df_age_cate_per_m <- df_age_cate_int_m %>% mutate(percent = total / sum(total))

df_age_cate_per_m



df_age_cate_int_f_f <- hn_all_f %>% group_by(age_cate) %>% summarise(total = n())

head(df_age_cate_int_f_f)

df_age_cate_int_f <- hn_all_f %>% group_by(age_cate, HE_DM) %>% summarise(total = n())

head(df_age_cate_int_f)

df_age_cate_per_f <- df_age_cate_int_f %>% mutate(percent = total / sum(total))

df_age_cate_per_f







## 직업

# occp4G(1 - manual, 2 - non_manual, 3 - service, 4 - non_employee)

df_occp4G_int_m_m <- hn_all_m %>% group_by(occp4G) %>% summarise(total = n())

df_occp4G_int_m_m

df_occp4G_int_m <- hn_all_m %>% group_by(occp4G, HE_DM) %>% summarise(total = n())

df_occp4G_int_m

df_occp_per_m <- df_occp4G_int_m %>% group_by(occp4G) %>% mutate(percent = total / sum(total))

df_occp_per_m



df_occp4G_int_f_f <- hn_all_f %>% group_by(occp4G) %>% summarise(total = n())

df_occp4G_int_f_f

df_occp4G_int_f <- hn_all_f %>% group_by(occp4G, HE_DM) %>% summarise(total = n())

df_occp4G_int_f

df_occp_per_f <- df_occp4G_int_f %>% group_by(occp4G) %>% mutate(percent = total / sum(total))

df_occp_per_f



# Table 3 chisq-test

# 남녀 직업, 당뇨유무, 연령

chisq.test(hn_all$age_cate, hn_all$sex)
CrossTable(hn_all$age_cate, hn_all$sex, chisq = T)

chisq.test(hn_all$occp4G, hn_all$sex)
CrossTable(hn_all$occp4G, hn_all$sex, chisq = T)

chisq.test(hn_all$HE_DM, hn_all$sex)
CrossTable(hn_all$HE_DM, hn_all$sex, chisq = T)

"
# sex

df_sex_int <- hn_all %>% group_by(sex, HE_DM) %>% summarise(total = n())

head(df_sex_int)

df_sex_per <- df_sex_int %>% group_by(sex) %>% mutate(percent = total / sum(total)) %>% mutate(total_sum = sum(total))

df_sex_per


# BS3_1 (1,3,8,9)

df_BS3_1_int <- hn_all %>% group_by(BS3_1, HE_DM) %>% summarise(total = n())

head(df_BS3_1_int)

df_BS3_1_per <- df_BS3_1_int %>% group_by(BS3_1) %>% mutate(percent = total / sum(total))

df_BS3_1_per

# edu (1,2,3,4)

df_edu_int <- hn_all %>% group_by(edu, HE_DM) %>% summarise(total = n())

head(df_edu_int)

df_edu_per <- df_edu_int %>% group_by(edu) %>% mutate(percent = total / sum(total))

df_edu_per

# marri_1 (1,2)

df_marri_1_int <- hn_all %>% group_by(marri_1, HE_DM) %>% summarise(total = n())

head(df_marri_1_int)

df_marri_1_per <- df_marri_1_int %>% group_by(marri_1) %>% mutate(percent = total / sum(total))

df_marri_1_per

# incm (1,2,3,4)

df_incm_int <- hn_all %>% group_by(incm, HE_DM) %>% summarise(total = n())

head(df_incm_int)

df_incm_per <- df_incm_int %>% group_by(incm) %>% mutate(percent = total / sum(total))

df_incm_per

# EC_wht_5 (1,2,8,88,99)

df_EC_wht_5_int <- hn_all %>% group_by(EC_wht_5, HE_DM) %>% summarise(total = n())

head(df_EC_wht_5_int)

df_EC_wht_5_per <- df_EC_wht_5_int %>% group_by(EC_wht_5) %>% mutate(percent = total / sum(total))

df_EC_wht_5_per

# ainc_band (1,2,3,4)

df_ainc_band_int <- hn_all %>% group_by(ainc_band, HE_DM) %>% summarise(total = n())

head(df_ainc_band_int)

df_ainc_band_per <- df_ainc_band_int %>% group_by(ainc_band) %>% mutate(percent = total / sum(total))

df_ainc_band_per

# HE_BMI_band (1,2)

df_HE_BMI_band_int <- hn_all %>% group_by(HE_BMI_band, HE_DM) %>% summarise(total = n())

head(df_HE_BMI_band_int)

df_HE_BMI_band_per <- df_HE_BMI_band_int %>% group_by(HE_BMI_band) %>% mutate(percent = total / sum(total))

df_HE_BMI_band_per


##  파생변수 당뇨 구성비

# sm_presnt (0,1)

df_sm_presnt_int <- hn_all %>% group_by(sm_presnt, HE_DM) %>% summarise(total = n())

head(df_sm_presnt_int)

df_sm_presnt_per <- df_sm_presnt_int %>% group_by(sm_presnt) %>% mutate(percent = total / sum(total))

df_sm_presnt_per

# dr_month(0,1)

df_dr_month_int <- hn_all %>% group_by(dr_month, HE_DM) %>% summarise(total = n())

head(df_dr_month_int)

df_dr_month_per <- df_dr_month_int %>% group_by(dr_month) %>% mutate(percent = total / sum(total))

df_dr_month_per

# dr_high(0,1)

df_dr_high_int <- hn_all %>% group_by(dr_high, HE_DM) %>% summarise(total = n())

head(df_dr_high_int)

df_dr_high_per <- df_dr_high_int %>% group_by(dr_high) %>% mutate(percent = total / sum(total))

df_dr_high_per

# pa_walk(0,1)

df_pa_walk_int <- hn_all %>% group_by(pa_walk, HE_DM) %>% summarise(total = n())

head(df_pa_walk_int)

df_pa_walk_per <- df_pa_walk_int %>% group_by(pa_walk) %>% mutate(percent = total / sum(total))

df_pa_walk_per

# pa_aerobic(0,1)

df_pa_aerobic_int <- hn_all %>% group_by(pa_aerobic, HE_DM) %>% summarise(total = n())

head(df_pa_aerobic_int)

df_pa_aerobic_per <- df_pa_aerobic_int %>% group_by(pa_aerobic) %>% mutate(percent = total / sum(total))

df_pa_aerobic_per
"










###  TABLE 1



###################### 모집단 특성을 보기 위한 직업그룹별(4개) ANOVA, Chisq-Test 시작 ######################


################ 직업그룹별 연속형 변수 ANOVA-test 시작 ################

### 직업군별 age 비교.

anova(lm(age~occp4G, hn_all)) # 2.2e-16 ***
oneway.test(age~occp4G,hn_all) # p-value < 2.2e-16

# 분산분석 결과, 두 테스트 모두 '직업군별 나이에 대한 평균이 같다고 할 수 없다는 충분한 통계적 근거가 있다.' 라는 결과가 나옴.






### 직업군별 HE_BMI(체질량지수) 비교.


anova(lm(HE_BMI~occp4G, hn_all)) # 2.2e-16 ***
oneway.test(HE_BMI~occp4G,hn_all) # 2.2e-16

# 분산분석 결과, Welch ANOVA에서 '직업군별 BMI에 대한 평균이 같다고 할 수 없다는 충분한 통계적 근거가 있다.' 라는 결과가 나옴.

# 직업군별 BMI 평균과 표준편차

df_HE_BMI_int <- hn_all %>% group_by(occp4G) %>% summarize(mean_HE_BMI = mean(HE_BMI), sd_HE_BMI = sd(HE_BMI))

df_HE_BMI_int

# 전체 BMI 평균과 표준편차

df_HE_BMI_total <- hn_all %>% summarize(mean_HE_BMI = mean(HE_BMI), sd_HE_BMI = sd(HE_BMI))

df_HE_BMI_total

### 직업군별 영양변수 비교.

anova(lm(N_EN ~ occp4G, hn_all)) # 2.2e-16 ***
oneway.test(N_EN ~ occp4G,hn_all) # p-value < 2.2e-16

anova(lm(N_SUGAR ~ occp4G, hn_all)) # 2.2e-16 ***
oneway.test(N_SUGAR ~ occp4G,hn_all) # p-value < 2.2e-16

anova(lm(N_FAT ~ occp4G, hn_all)) # 2.2e-16 ***
oneway.test(N_FAT ~ occp4G,hn_all) # -value < 2.2e-16

# 직업군별 영양변수 평균과 표준편차

# 에너지

df_occp4G_EN <- hn_all %>% group_by(occp4G) %>% summarize(mean_N_EN = mean(N_EN), sd_N_EN = sd(N_EN))

df_occp4G_EN

df_occp4G_EN_total <- hn_all %>% summarize(mean_N_EN = mean(N_EN), sd_N_EN = sd(N_EN))

df_occp4G_EN_total

# 당

df_occp4G_SUGAR <- hn_all %>% group_by(occp4G) %>% summarize(mean_N_SUGAR = mean(N_SUGAR), sd_N_SUGAR = sd(N_SUGAR))

df_occp4G_SUGAR

df_occp4G_SUGAR_total <- hn_all %>% summarize(mean_N_SUGAR = mean(N_SUGAR), sd_N_SUGAR = sd(N_SUGAR))

df_occp4G_SUGAR_total

# 지방

df_occp4G_FAT <- hn_all %>% group_by(occp4G) %>% summarize(mean_N_FAT = mean(N_FAT), sd_N_FAT = sd(N_FAT))

df_occp4G_FAT

df_occp4G_FAT_total <- hn_all %>% summarize(mean_N_FAT = mean(N_FAT), sd_N_FAT = sd(N_FAT))

df_occp4G_FAT_total

### 직업군별 ainc(월평균 가구총소득) 비교.

anova(lm(ainc~occp, hn_all)) # 2.2e-16 ***
oneway.test(ainc~occp,hn_all) # p-value < 2.2e-16






################ 직업그룹별 연속형 변수 ANOVA-test 종료 ################





################ 직업그룹별 범주형 변수 Chisq-test 시작 ################

# sex

chisq.test(hn_all$sex, hn_all$occp4G)
CrossTable(hn_all$sex, hn_all$occp4G, chisq = T)

# edu3G

chisq.test(hn_all$edu3G, hn_all$occp4G)
CrossTable(hn_all$edu3G, hn_all$occp4G, chisq = T)

# age_cate

chisq.test(hn_all$age_cate, hn_all$occp4G)
CrossTable(hn_all$age_cate, hn_all$occp4G, chisq = T)

# HE_BMI_band

chisq.test(hn_all$HE_BMI_band, hn_all$occp)
CrossTable(hn_all$HE_BMI_band, hn_all$occp, chisq = T)

# ainc_band

chisq.test(hn_all$ainc_band, hn_all$occp4G)
CrossTable(hn_all$ainc_band, hn_all$occp4G, chisq = T)

# sm_presnt

chisq.test(hn_all$sm_presnt, hn_all$occp4G)
CrossTable(hn_all$sm_presnt, hn_all$occp4G, chisq = T)

# dr_month

chisq.test(hn_all$dr_month, hn_all$occp4G)
CrossTable(hn_all$dr_month, hn_all$occp4G, chisq = T)


# pa_aerobic

chisq.test(hn_all$pa_aerobic, hn_all$occp4G)
CrossTable(hn_all$pa_aerobic, hn_all$occp4G, chisq = T)


# BP6_10 - 자살

chisq.test(hn_all$BP6_10, hn_all$occp4G)
CrossTable(hn_all$BP6_10, hn_all$occp4G, chisq = T)

# BP5 - 우울

chisq.test(hn_all$BP5, hn_all$occp4G)
CrossTable(hn_all$BP5, hn_all$occp4G, chisq = T)

# mh_stress - 스트레스

chisq.test(hn_all$mh_stress, hn_all$occp4G)
CrossTable(hn_all$mh_stress, hn_all$occp4G, chisq = T)

################ 범주형 변수 Chisq-test 종료 #######################


###################### 모집단 특성을 보기 위한 직업그룹별(4개) ANOVA, Chisq-Test 종료 ######################









###  TABLE 2

###################### 모집단 특성을 보기 위한 성별그룹별  ANOVA, Chisq-Test 시작 ######################


################ 연속형 변수 t-test 시작 ################

### 성별 그룹별 BMI 비교.

hn_all$sex <- as.factor(hn_all$sex)

var.test(HE_BMI ~ sex, hn_all) # p-value = 2.578e-09 등분산성 아님.

t.test(HE_BMI ~ sex, hn_all) # p-value < 2.2e-16


# 성별 그룹별 BMI 평균과 표준편차

sex_HE_BMI_int <- hn_all %>% group_by(sex) %>% summarize(mean_HE_BMI = mean(HE_BMI), sd_HE_BMI = sd(HE_BMI))

sex_HE_BMI_int


### 성별 그룹별 영양변수 비교.

var.test(N_EN ~ sex, hn_all) # p-value < 2.2e-16 등분산성 아님.

t.test(N_EN ~ sex, hn_all) # p-value < 2.2e-16

var.test(N_SUGAR ~ sex, hn_all) # p-value < 2.2e-16 등분산성 아님.

t.test(N_SUGAR ~ sex, hn_all) # p-value < 2.2e-16

var.test(N_FAT ~ sex, hn_all) # p-value < 2.2e-16 등분산성 아님.

t.test(N_FAT ~ sex, hn_all) # p-value < 2.2e-16

# 성별 그룹별 영양변수 평균과 표준편차

# 에너지

df_sex_EN <- hn_all %>% group_by(sex) %>% summarize(mean_N_EN = mean(N_EN), sd_N_EN = sd(N_EN))

df_sex_EN



# 당

df_sex_SUGAR <- hn_all %>% group_by(sex) %>% summarize(mean_N_SUGAR = mean(N_SUGAR), sd_N_SUGAR = sd(N_SUGAR))

df_sex_SUGAR



# 지방

df_sex_FAT <- hn_all %>% group_by(sex) %>% summarize(mean_N_FAT = mean(N_FAT), sd_N_FAT = sd(N_FAT))

df_sex_FAT









################ 연속형 변수 ANOVA-test 종료 ################





################ 범주형 변수 Chisq-test 시작 ################

# HE_BMI_band

chisq.test(hn_all$HE_BMI_band, hn_all$sex)
CrossTable(hn_all$HE_BMI_band, hn_all$sex, chisq = T)

# edu3G

chisq.test(hn_all$edu3G, hn_all$sex)
CrossTable(hn_all$edu3G, hn_all$sex, chisq = T)

# age_cate

chisq.test(hn_all$age_cate, hn_all$sex)
CrossTable(hn_all$age_cate, hn_all$sex, chisq = T)

# HE_DM

chisq.test(hn_all$HE_DM, hn_all$sex)
CrossTable(hn_all$HE_DM, hn_all$sex, chisq = T)

# ainc_band

chisq.test(hn_all$ainc_band, hn_all$sex)
CrossTable(hn_all$ainc_band, hn_all$sex, chisq = T)

# sm_presnt

chisq.test(hn_all$sm_presnt, hn_all$sex)
CrossTable(hn_all$sm_presnt, hn_all$sex, chisq = T)

# dr_month

chisq.test(hn_all$dr_month, hn_all$sex)
CrossTable(hn_all$dr_month, hn_all$sex, chisq = T)

# pa_aerobic

chisq.test(hn_all$pa_aerobic, hn_all$sex)
CrossTable(hn_all$pa_aerobic, hn_all$sex, chisq = T)

# ainc_band

chisq.test(hn_all$ainc_band, hn_all$sex)
CrossTable(hn_all$ainc_band, hn_all$sex, chisq = T)

# BP6_10 - 자살

chisq.test(hn_all$BP6_10, hn_all$sex)
CrossTable(hn_all$BP6_10, hn_all$sex, chisq = T)

# BP5 - 우울

chisq.test(hn_all$BP5, hn_all$sex)
CrossTable(hn_all$BP5, hn_all$sex, chisq = T)

# mh_stress - 스트레스

chisq.test(hn_all$mh_stress, hn_all$sex)
CrossTable(hn_all$mh_stress, hn_all$sex, chisq = T)

################ 범주형 변수 Chisq-test 종료 #######################




###################### 모집단 특성을 보기 위한 성별 그룹별 ANOVA, Chisq-Test 종료 ######################


########################################################################################################

#### Table 4 - 남녀 직업별 오즈비와 신뢰구간



