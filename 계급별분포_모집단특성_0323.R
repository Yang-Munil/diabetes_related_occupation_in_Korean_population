library(haven)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(gmodels)
library(agricolae)

# install.packages("agricolae")
# install.packages("gmodels")

################ 변수별 당뇨 비율보기

##### 변수별 당뇨 비율을 보기 위해서는 더미변수형태가 아닌 구간으로 나뉘어져 연속형 변수 형태여야 가능.

### 당뇨비율을 보기 위한 데이터 불러오기


# 데이터 불러오기

hn18_all <- read_sas("C:/rproject/startr/hn18_all.sas7bdat", NULL)
hn17_all <- read_sas("C:/rproject/startr/hn17_all.sas7bdat", NULL)
hn16_all <- read_sas("C:/rproject/startr/hn16_all.sas7bdat", NULL)

# 필요한 변수 선택

hn18_all_sub <- hn18_all %>% select(year, sex, age, occp, HE_BMI, HE_DM, BS3_1, edu, marri_1, ainc, incm, EC_wht_5, HE_glu)
hn17_all_sub <- hn17_all %>% select(year, sex, age, occp, HE_BMI, HE_DM, BS3_1, edu, marri_1, ainc, incm, EC_wht_5, HE_glu)
hn16_all_sub <- hn16_all %>% select(year, sex, age, occp, HE_BMI, HE_DM, BS3_1, edu, marri_1, ainc, incm, EC_wht_5, HE_glu)

# 전체 데이터셋

hn_all <- rbind(hn18_all_sub, hn17_all_sub, hn16_all_sub)

# 전체 데이터셋에서 19미만 제거

hn_all <- hn_all[!(hn_all$age < 19),]

# 종속변수로 쓰일 HE_BMI, HE_DM 은 결측치일 경우 drop처리

## HE_BMI, HE_DM, incm, ainc, occp, edu 결측치있는 행 제거

hn_all <- hn_all[complete.cases(hn_all[, c("HE_BMI", "HE_DM", "incm", "ainc", "occp", "edu")]), ]

# 결측치 x

colSums(is.na(hn_all))

# BS3_1 / (1,2),(3),(8),(9) / never, past, current, missing

hn_all$BS3_1[hn_all$BS3_1 == 2] <- 1

# EC_wht_5 ((1,2,3)은 교대근무x, 따라서 1로 변환. (4,5,6,7)은 교대근무 o, 따라서 2로 변환, 나머지 그대로 진행. (1,2,8,88,99) 로 더미변수로 만들지 않고 연속형 변수로 진행

hn_all$EC_wht_5[hn_all$EC_wht_5 == 2] <- 1
hn_all$EC_wht_5[hn_all$EC_wht_5 == 3] <- 1
hn_all$EC_wht_5[hn_all$EC_wht_5 == 4] <- 2
hn_all$EC_wht_5[hn_all$EC_wht_5 == 5] <- 2
hn_all$EC_wht_5[hn_all$EC_wht_5 == 6] <- 2
hn_all$EC_wht_5[hn_all$EC_wht_5 == 7] <- 2

# age 를 구간별로 나눈후 age_band 라는 새로운 변수 생성후 그대로 진행

hn_all$age_band = 1
hn_all$age_band[hn_all$age>=30]=2
hn_all$age_band[hn_all$age>=40]=3
hn_all$age_band[hn_all$age>=50]=4
hn_all$age_band[hn_all$age>=60]=5
hn_all$age_band[hn_all$age>=70]=6

# HE_BMI 를 구간별로 나눈후 HE_BMI_band 라는 새로운 변수 생성후 그대로 진행

hn_all$HE_BMI_band = 1
hn_all$HE_BMI_band[hn_all$HE_BMI>=25]=2

# ainc 를 구간별로 나눈후 ainc_band 라는 새로운 변수 생성후 그대로 진행

hn_all$ainc_band[hn_all$ainc<200]=1
hn_all$ainc_band[hn_all$ainc>=200 & hn_all$ainc<400]=2
hn_all$ainc_band[hn_all$ainc>=400 & hn_all$ainc<600]=3
hn_all$ainc_band[hn_all$ainc>=600]=4

# HE_DM 1,2는 당뇨x(0), 3은 당뇨(1)

# HE_DM이 2면 0, 3이면 1로 바꾸는 코드

hn_all$HE_DM[hn_all$HE_DM == 1] <- 1
hn_all$HE_DM[hn_all$HE_DM == 2] <- 1
hn_all$HE_DM[hn_all$HE_DM == 3] <- 2

# 나머지 파생변수(현재흡연율, ... , 유산소신체활동율) 데이터셋 불러온 후 합치기

# 파생변수 데이터 불러오기

hn_all_der <- read.csv("hn_all_der.csv", sep = ",")
hn_all_aero <- read.csv("hn_all_aero.csv", sep = ",")

# 컬럼 X 제거(갑자기 생김)

hn_all_der <- subset(hn_all_der, select = -c(X))
hn_all_aero <- subset(hn_all_aero, select = -c(X))

# 전처리된 데이터 합치기

hn_all <- cbind(hn_all, hn_all_der, hn_all_aero)

colSums(is.na(hn_all))


#######################변수별 당뇨 구성비를 보기 위한 전처리 데이터 생성 완료#########################


# 종속변수(HE_DM) 비율.

table(hn_all$HE_DM)

cat("total :", margin.table(table(hn_all$HE_DM)))

prop.table(table(hn_all$HE_DM))



################ 변수별 당뇨 비율보기

##### 변수별 당뇨 비율을 보기 위해서는 더미변수형태가 아닌 구간으로 나뉘어져 연속형 변수 형태여야 가능.



# sex

df_sex_int <- hn_all %>% group_by(sex, HE_DM) %>% summarise(total = n())

head(df_sex_int)

df_sex_per <- df_sex_int %>% group_by(sex) %>% mutate(percent = total / sum(total)) %>% mutate(total_sum = sum(total))

df_sex_per

# occp

df_occp_int <- hn_all %>% group_by(occp, HE_DM) %>% summarise(total = n())

head(df_occp_int)

df_occp_per <- df_occp_int %>% group_by(occp) %>% mutate(percent = total / sum(total))

df_occp_per

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

# age_band (1,2,3,4,5,6)

df_age_band_int <- hn_all %>% group_by(age_band, HE_DM) %>% summarise(total = n())

head(df_age_band_int)

df_age_band_per <- df_age_band_int %>% group_by(age_band) %>% mutate(percent = total / sum(total))

df_age_band_per

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















###################### 모집단 특성을 보기 위한 직업그룹별 당뇨유무 ANOVA, Chisq-Test 시작 ######################


################ 연속형 변수 ANOVA-test 시작 ################

### 직업군별 age 비교.

## 정규성, 등분산성 테스트.

# Kolmogorov-Smirnov test, 콜모고로프-스미노프 검정
# 자료의 평균/표준편차와 히스토그램을 표준정규분포와 비교하여 적합도를 검정
# 정규성 만족 p-value < 2.2e-16

ks.test(hn_all$age, "pnorm", mean=mean(hn_all$age), sd=sd(hn_all$age))

# 등분산성 확인 : 바틀렛 검정(Bartlett’s test)
# 등분산성 불만족 p-value < 2.2e-16 (귀무가설(등분산성 이다.)이 기각되었기 때문. )

bartlett.test(age~occp, hn_all)

# 따라서 등분산성을 가정하고 진행하는 ANOVA test와 등분산성을 가정하지 않고 진행하는 Welch ANOVA 함께 진행.

# 그 결과 직업군별(occp) 나이에 대한 평균이 같다고 할 수 없다. (두 개의 테스트 모두 귀무가설 기각)

anova(lm(age~occp, hn_all)) # 2.2e-16 ***
oneway.test(age~occp,hn_all) # p-value < 2.2e-16

# 분산분석 결과, 두 테스트 모두 '직업군별 나이에 대한 평균이 같다고 할 수 없다는 충분한 통계적 근거가 있다.' 라는 결과가 나옴.

# 사후검정(post hoc analysis)를 통해 어떤 직업군간에 차이가 있는 것인지 알아봄.

# 가능한 모든 경우의 수에 대한 짝을 구성한 후 각 검정을 수행하는 것. (= 다중비교(multiple comparison))


# 본페로니(bonferroni) 다중비교 

model_age <- aov(age~occp, hn_all)
comparison_age <- LSD.test(model_age,"occp",p.adj = "bonferroni", group = T)
comparison_age

# 순서대로 
# 변동을 range로 나타낸 그래프
# 변동을 Standard deviation으로 나타낸 그래프
# 변동을 IQR로 나타낸 그래프

plot(comparison_age)
plot(comparison_age, variation = "SD")
plot(comparison_age, variation = "IQR")




### 직업군별 HE_BMI(체질량지수) 비교.

## 정규성, 등분산성 테스트.

# Kolmogorov-Smirnov test, 콜모고로프-스미노프 검정
# 자료의 평균/표준편차와 히스토그램을 표준정규분포와 비교하여 적합도를 검정
# 정규성 만족 p-value < 2.2e-16

ks.test(hn_all$HE_BMI, "pnorm", mean=mean(hn_all$HE_BMI), sd=sd(hn_all$HE_BMI))

# 등분산성 확인 : 바틀렛 검정(Bartlett’s test)
# 등분산성 불만족 p-value = 1.704e-05 (귀무가설(등분산성 이다.)이 기각되었기 때문. )

bartlett.test(HE_BMI~occp, hn_all)

# 따라서 등분산성을 가정하고 진행하는 ANOVA test와 등분산성을 가정하지 않고 진행하는 Welch ANOVA 함께 진행.

# 그 결과 직업군별(occp) 나이에 대한 평균이 같다고 할 수 없다. (두 개의 테스트 모두 귀무가설 기각)

anova(lm(HE_BMI~occp, hn_all)) # 0.1147
oneway.test(HE_BMI~occp,hn_all) # p-value < 2.2e-16

# 분산분석 결과, Welch ANOVA에서 '직업군별 BMI에 대한 평균이 같다고 할 수 없다는 충분한 통계적 근거가 있다.' 라는 결과가 나옴.

# 사후검정(post hoc analysis)를 통해 어떤 직업군간에 차이가 있는 것인지 알아봄.

# 가능한 모든 경우의 수에 대한 짝을 구성한 후 각 검정을 수행하는 것. (= 다중비교(multiple comparison))

# 본페로니(bonferroni) 다중비교

model_HE_BMI <- aov(HE_BMI~occp, hn_all)
comparison_HE_BMI <- LSD.test(model_HE_BMI,"occp",p.adj = "bonferroni", group = T)
comparison_HE_BMI

# 순서대로 
# 변동을 range로 나타낸 그래프
# 변동을 Standard deviation으로 나타낸 그래프
# 변동을 IQR로 나타낸 그래프

plot(comparison_HE_BMI)
plot(comparison_HE_BMI, variation = "SD")
plot(comparison_HE_BMI, variation = "IQR")




### 직업군별 ainc(월평균 가구총소득) 비교.

## 정규성, 등분산성 테스트.

# Kolmogorov-Smirnov test, 콜모고로프-스미노프 검정
# 자료의 평균/표준편차와 히스토그램을 표준정규분포와 비교하여 적합도를 검정
# 정규성 만족 p-value < 2.2e-16

ks.test(hn_all$ainc, "pnorm", mean=mean(hn_all$ainc), sd=sd(hn_all$ainc))

# 등분산성 확인 : 바틀렛 검정(Bartlett’s test)
# 등분산성 불만족 p-value < 2.2e-16 (귀무가설(등분산성 이다.)이 기각되었기 때문. )

bartlett.test(ainc~occp, hn_all)

# 따라서 등분산성을 가정하고 진행하는 ANOVA test와 등분산성을 가정하지 않고 진행하는 Welch ANOVA 함께 진행.

# 그 결과 직업군별(occp) ainc에 대한 평균이 같다고 할 수 없다. (두 개의 테스트 모두 귀무가설 기각)

anova(lm(ainc~occp, hn_all)) # 2.2e-16 ***
oneway.test(ainc~occp,hn_all) # p-value < 2.2e-16

# 분산분석 결과, 두 테스트 모두 '직업군별 ainc에 대한 평균이 같다고 할 수 없다는 충분한 통계적 근거가 있다.' 라는 결과가 나옴.

# 사후검정(post hoc analysis)를 통해 어떤 직업군간에 차이가 있는 것인지 알아봄.

# 가능한 모든 경우의 수에 대한 짝을 구성한 후 각 검정을 수행하는 것. (= 다중비교(multiple comparison))

# 본페로니(bonferroni) 다중비교

model_ainc <- aov(ainc~occp, hn_all)
comparison_ainc <- LSD.test(model_ainc,"occp",p.adj = "bonferroni", group = T)
comparison_ainc

# 순서대로 
# 변동을 range로 나타낸 그래프
# 변동을 Standard deviation으로 나타낸 그래프
# 변동을 IQR로 나타낸 그래프

plot(comparison_ainc)
plot(comparison_ainc, variation = "SD")
plot(comparison_ainc, variation = "IQR")





### 직업군별 HE_glu(공복혈당) 비교.

## 정규성, 등분산성 테스트.

# Kolmogorov-Smirnov test, 콜모고로프-스미노프 검정
# 자료의 평균/표준편차와 히스토그램을 표준정규분포와 비교하여 적합도를 검정
# 정규성 만족 p-value < 2.2e-16

ks.test(hn_all$HE_glu, "pnorm", mean=mean(hn_all$HE_glu), sd=sd(hn_all$HE_glu))

# 등분산성 확인 : 바틀렛 검정(Bartlett’s test)
# 등분산성 불만족 p-value < 2.2e-16 (귀무가설(등분산성 이다.)이 기각되었기 때문. )

bartlett.test(HE_glu~occp, hn_all)

# 따라서 등분산성을 가정하고 진행하는 ANOVA test와 등분산성을 가정하지 않고 진행하는 Welch ANOVA 함께 진행.

# 그 결과 직업군별(occp) HE_glu에 대한 평균이 같다고 할 수 없다. (두 개의 테스트 모두 귀무가설 기각)

anova(lm(HE_glu~occp, hn_all)) # 2.2e-16 ***
oneway.test(HE_glu~occp,hn_all) # p-value < 2.2e-16

# 분산분석 결과, 두 테스트 모두 '직업군별 HE_glu에 대한 평균이 같다고 할 수 없다는 충분한 통계적 근거가 있다.' 라는 결과가 나옴.

# 사후검정(post hoc analysis)를 통해 어떤 직업군간에 차이가 있는 것인지 알아봄.

# 가능한 모든 경우의 수에 대한 짝을 구성한 후 각 검정을 수행하는 것. (= 다중비교(multiple comparison))

# 본페로니(bonferroni) 다중비교

model_HE_glu <- aov(HE_glu~occp, hn_all)
comparison_HE_glu <- LSD.test(model_HE_glu,"occp",p.adj = "bonferroni", group = T)
comparison_HE_glu

# 순서대로 
# 변동을 range로 나타낸 그래프
# 변동을 Standard deviation으로 나타낸 그래프
# 변동을 IQR로 나타낸 그래프

plot(comparison_HE_glu)
plot(comparison_HE_glu, variation = "SD")
plot(comparison_HE_glu, variation = "IQR")


################ 연속형 변수 ANOVA-test 종료 ################





################ 범주형 변수 Chisq-test 시작 ################

# sex

chisq.test(hn_all$sex, hn_all$HE_DM)
CrossTable(hn_all$sex, hn_all$HE_DM, chisq = T)

# BS3_1

chisq.test(hn_all$BS3_1, hn_all$HE_DM)
CrossTable(hn_all$BS3_1, hn_all$HE_DM, chisq = T)

fisher.test(hn_all$BS3_1, hn_all$HE_DM,simulate.p.value = TRUE)

# edu

chisq.test(hn_all$edu, hn_all$HE_DM)
CrossTable(hn_all$edu, hn_all$HE_DM, chisq = T)

# marri_1

chisq.test(hn_all$marri_1, hn_all$HE_DM)
CrossTable(hn_all$marri_1, hn_all$HE_DM, chisq = T)

# incm

chisq.test(hn_all$incm, hn_all$HE_DM)
CrossTable(hn_all$incm, hn_all$HE_DM, chisq = T)

# EC_wht_5

chisq.test(hn_all$EC_wht_5, hn_all$HE_DM)
CrossTable(hn_all$EC_wht_5, hn_all$HE_DM, chisq = T)

fisher.test(hn_all$EC_wht_5, hn_all$HE_DM,simulate.p.value = TRUE)

# age_band

chisq.test(hn_all$age_band, hn_all$HE_DM)
CrossTable(hn_all$age_band, hn_all$HE_DM, chisq = T)

# HE_BMI_band

chisq.test(hn_all$HE_BMI_band, hn_all$HE_DM)
CrossTable(hn_all$HE_BMI_band, hn_all$HE_DM, chisq = T)

# ainc_band

chisq.test(hn_all$ainc_band, hn_all$HE_DM)
CrossTable(hn_all$ainc_band, hn_all$HE_DM, chisq = T)

# sm_presnt

chisq.test(hn_all$sm_presnt, hn_all$HE_DM)
CrossTable(hn_all$sm_presnt, hn_all$HE_DM, chisq = T)

# dr_month

chisq.test(hn_all$dr_month, hn_all$HE_DM)
CrossTable(hn_all$dr_month, hn_all$HE_DM, chisq = T)

# dr_high

chisq.test(hn_all$dr_high, hn_all$HE_DM)
CrossTable(hn_all$dr_high, hn_all$HE_DM, chisq = T)

fisher.test(hn_all$dr_high, hn_all$HE_DM,simulate.p.value = TRUE)

# pa_walk

chisq.test(hn_all$pa_walk, hn_all$HE_DM)
CrossTable(hn_all$pa_walk, hn_all$HE_DM, chisq = T)

# pa_aerobic

chisq.test(hn_all$pa_aerobic, hn_all$HE_DM)
CrossTable(hn_all$pa_aerobic, hn_all$HE_DM, chisq = T)


################ 범주형 변수 Chisq-test 종료 #######################


###################### 모집단 특성을 보기 위한 직업그룹별 당뇨유무 ANOVA, Chisq-Test 종료 ######################

