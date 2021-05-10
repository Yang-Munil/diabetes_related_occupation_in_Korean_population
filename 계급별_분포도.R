library(haven)
library(tidyverse)
library(dplyr)

################ 변수별 당뇨 비율보기

##### 변수별 당뇨 비율을 보기 위해서는 더미변수형태가 아닌 구간으로 나뉘어져 연속형 변수 형태여야 가능.

### 당뇨비율을 보기 위한 데이터 불러오기


# 데이터 불러오기

hn18_all <- read_sas("C:/rproject/startr/hn18_all.sas7bdat", NULL)
hn17_all <- read_sas("C:/rproject/startr/hn17_all.sas7bdat", NULL)
hn16_all <- read_sas("C:/rproject/startr/hn16_all.sas7bdat", NULL)

# 필요한 변수 선택

hn18_all_sub <- hn18_all %>% select(year, sex, age, occp, HE_BMI, HE_DM, BS3_1, edu, marri_1, ainc, incm, EC_wht_5)
hn17_all_sub <- hn17_all %>% select(year, sex, age, occp, HE_BMI, HE_DM, BS3_1, edu, marri_1, ainc, incm, EC_wht_5)
hn16_all_sub <- hn16_all %>% select(year, sex, age, occp, HE_BMI, HE_DM, BS3_1, edu, marri_1, ainc, incm, EC_wht_5)

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

hn_all$HE_DM[hn_all$HE_DM == 1] <- 0
hn_all$HE_DM[hn_all$HE_DM == 2] <- 0
hn_all$HE_DM[hn_all$HE_DM == 3] <- 1

# 나머지 파생변수(현재흡연율, ... , 유산소신체활동율) 데이터셋 불러온 후 합치기

# 파생변수 데이터 불러오기

hn_all_der <- read.csv("hn_all_der.csv", sep = ",")
hn_all_aero <- read.csv("hn_all_aero.csv", sep = ",")

# 컬럼 X 제거(갑자기 생김)

hn_all_der <- subset(hn_all_der, select = -c(X))
hn_all_aero <- subset(hn_all_aero, select = -c(X))

# 전처리된 데이터 합치기

hn_all <- cbind(hn_all, hn_all_der, hn_all_aero)
hn_all

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

df_sex_per <- df_sex_int %>% group_by(sex) %>% mutate(percent = total / sum(total))

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
