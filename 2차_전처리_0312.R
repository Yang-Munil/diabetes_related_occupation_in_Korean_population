library(haven)
library(tidyverse)

# 데이터 불러오기
hn18_all <- read_sas("C:/rproject/startr/hn18_all.sas7bdat", NULL)
hn17_all <- read_sas("C:/rproject/startr/hn17_all.sas7bdat", NULL)
hn16_all <- read_sas("C:/rproject/startr/hn16_all.sas7bdat", NULL)

# 필요한 변수 선택
# 현재흡연율 - (파생변수)
# 음주(월간음주율, 고위험음주율) - (파생변수)
# 신체활동(걷기 실천율, 유산소 신체활동 실천율) - (파생변수)
# educ 대신 edu(4개) 쓰는거 어떤지

hn18_all_sub <- hn18_all %>% select(year, sex, age, occp, HE_BMI, HE_DM, BS3_1, educ, marri_1, ainc, incm, EC_wht_5)
hn17_all_sub <- hn17_all %>% select(year, sex, age, occp, HE_BMI, HE_DM, BS3_1, educ, marri_1, ainc, incm, EC_wht_5)
hn16_all_sub <- hn16_all %>% select(year, sex, age, occp, HE_BMI, HE_DM, BS3_1, educ, marri_1, ainc, incm, EC_wht_5)

# 전체 데이터셋

hn_all <- rbind(hn18_all_sub, hn17_all_sub, hn16_all_sub)

# 전체 데이터셋에서 19미만 제거

hn_all <- hn_all[!(hn_all$age < 19),]



# 결측치 확인

colSums(is.na(hn_all))

########### 결측치 대체 ############

# 종속변수로 쓰일 HE_BMI, HE_DM 은 결측치일 경우 drop처리

## HE_BMI, HE_DM, incm, ainc, occp 결측치있는 행 제거

hn_all <- hn_all[complete.cases(hn_all[, c("HE_BMI", "HE_DM", "incm", "ainc", "occp")]), ]

# 결측치 x

colSums(is.na(hn_all))


## 더미변수로 만들기

# year

hn_all <- transform(hn_all, year_2016 = ifelse(year == 2016, 1, 0), year_2017 = ifelse(year == 2017, 1, 0), year_2018 = ifelse(year == 2018, 1, 0))

# sex

hn_all <- transform(hn_all, sex_1 = ifelse(sex == 1, 1, 0), sex_2 = ifelse(sex == 2, 1, 0))

# occp

hn_all <- transform(hn_all, occp_1 = ifelse(occp == 1, 1, 0), occp_2 = ifelse(occp == 2, 1, 0), occp_3 = ifelse(occp == 3, 1, 0), occp_4 = ifelse(occp == 4, 1, 0), occp_5 = ifelse(occp == 5, 1, 0), occp_6 = ifelse(occp == 6, 1, 0), occp_7 = ifelse(occp == 7, 1, 0))

# BS3_1

hn_all <- transform(hn_all, BS3_1_1 = ifelse(BS3_1 == 1, 1, 0), BS3_1_2 = ifelse(BS3_1 == 2, 1, 0), BS3_1_3 = ifelse(BS3_1 == 3, 1, 0), BS3_1_8 = ifelse(BS3_1 == 8, 1, 0), BS3_1_9 = ifelse(BS3_1 == 9, 1, 0))

# educ (1은 3명, 88 비해당은 결측치 제거하면서 삭제됨.)

hn_all <- transform(hn_all, educ_1 = ifelse(educ == 1, 1, 0), educ_2 = ifelse(educ == 2, 1, 0), educ_3 = ifelse(educ == 3, 1, 0), educ_4 = ifelse(educ == 4, 1, 0), educ_5 = ifelse(educ == 5, 1, 0), educ_6 = ifelse(educ == 6, 1, 0), educ_7 = ifelse(educ == 7, 1, 0), educ_8 = ifelse(educ == 8, 1, 0), educ_99 = ifelse(educ == 99, 1, 0))

# marri_1 (9 무응답, 결측치 제거하면서 삭제됨.)

hn_all <- transform(hn_all, marri_1_1 = ifelse(marri_1 == 1, 1, 0), marri_1_2 = ifelse(marri_1 == 2, 1, 0))

# incm

hn_all <- transform(hn_all, incm_1 = ifelse(incm == 1, 1, 0), incm_2 = ifelse(incm == 2, 1, 0), incm_3 = ifelse(incm == 3, 1, 0), incm_4 = ifelse(incm == 4, 1, 0))

# EC_wht_5 ((1,2,3)은 교대근무x, 따라서 1로 변환. (4,5,6,7)은 교대근무 o, 따라서 2로 변환, 나머지 그대로 진행. (1,2,8,88,99) 로 더미변수 생성.

hn_all$EC_wht_5[hn_all$EC_wht_5 == 2] <- 1
hn_all$EC_wht_5[hn_all$EC_wht_5 == 3] <- 1
hn_all$EC_wht_5[hn_all$EC_wht_5 == 4] <- 2
hn_all$EC_wht_5[hn_all$EC_wht_5 == 5] <- 2
hn_all$EC_wht_5[hn_all$EC_wht_5 == 6] <- 2
hn_all$EC_wht_5[hn_all$EC_wht_5 == 7] <- 2


hn_all <- transform(hn_all, EC_wht_5_1 = ifelse(EC_wht_5 == 1, 1, 0), EC_wht_5_2 = ifelse(EC_wht_5 == 2, 1, 0), EC_wht_5_8 = ifelse(EC_wht_5 == 8, 1, 0), EC_wht_5_88 = ifelse(EC_wht_5 == 88, 1, 0), EC_wht_5_99 = ifelse(EC_wht_5 == 99, 1, 0))

## 구간 나눈 후 더미변수 만들기

# age 를 구간별로 나눈후 age_band 라는 새로운 변수 생성

hn_all$age_band = 1
hn_all$age_band[hn_all$age>=30]=2
hn_all$age_band[hn_all$age>=40]=3
hn_all$age_band[hn_all$age>=50]=4
hn_all$age_band[hn_all$age>=60]=5
hn_all$age_band[hn_all$age>=70]=6
hn_all$age_band[hn_all$age>=80]=7

# 구간으로 나눈 age_band 더미변수로 만들기

hn_all <- transform(hn_all, age_band_1 = ifelse(age_band == 1, 1, 0), age_band_2 = ifelse(age_band == 2, 1, 0), age_band_3 = ifelse(age_band == 3, 1, 0), age_band_4 = ifelse(age_band == 4, 1, 0), age_band_5 = ifelse(age_band == 5, 1, 0), age_band_6 = ifelse(age_band == 6, 1, 0), age_band_7 = ifelse(age_band == 7, 1, 0))

# HE_BMI 를 구간별로 나눈후 HE_BMI_band 라는 새로운 변수 생성

hn_all$HE_BMI_band = 1
hn_all$HE_BMI_band[hn_all$HE_BMI>=25]=2

# 구간으로 나눈 HE_BMI_band 더미변수로 만들기

hn_all <- transform(hn_all, HE_BMI_band_1 = ifelse(HE_BMI_band == 1, 1, 0), HE_BMI_band_2 = ifelse(HE_BMI_band == 2, 1, 0))

# ainc 를 구간별로 나눈후 ainc_band 라는 새로운 변수 생성

hn_all$ainc_band[hn_all$ainc<200]=1
hn_all$ainc_band[hn_all$ainc>=200 & hn_all$ainc<400]=2
hn_all$ainc_band[hn_all$ainc>=400 & hn_all$ainc<600]=3
hn_all$ainc_band[hn_all$ainc>=600]=4

# 구간으로 나눈 ainc_band 더미변수로 만들기

hn_all <- transform(hn_all, ainc_1 = ifelse(ainc_band == 1, 1, 0), ainc_2 = ifelse(ainc_band == 2, 1, 0), ainc_3 = ifelse(ainc_band == 3, 1, 0), ainc_4 = ifelse(ainc_band == 4, 1, 0))

## 더미변수를 제외한 연속형 변수, 더미변수에 쓰인 구간변수(_band 변수) 삭제


hn_all <- subset(hn_all, select = -c(year, sex, age, occp, BS3_1, educ, marri_1, ainc, incm, EC_wht_5, HE_BMI, age_band, ainc_band, HE_BMI_band))


# HE_DM 1,2는 당뇨x(1), 3은 당뇨(2)

# HE_DM이 2면 1, 3이면 2로 바꾸는 코드

hn_all$HE_DM[hn_all$HE_DM == 2] <- 1
hn_all$HE_DM[hn_all$HE_DM == 3] <- 2


str(hn_all)
hn_all

# 결측치 확인

colSums(is.na(hn_all))

# 만들어진 데이터 csv 파일로 추출

write.csv(hn_all, "hn_all.csv")

