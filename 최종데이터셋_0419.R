
##### 변수별 전처리 / 파생변수 / 유산소 한 번에 처리 #####

library(haven)
library(dplyr)
library(DT)





# 데이터 불러오기

hn18_all <- read_sas("C:/rproject/startr/hn18_all.sas7bdat", NULL)



hn17_all <- read_sas("C:/rproject/startr/hn17_all.sas7bdat", NULL)




hn16_all <- read_sas("C:/rproject/startr/hn16_all.sas7bdat", NULL)




# 필요한 변수 선택

hn18_all_sub <- hn18_all %>% select(year, sex, age, occp, HE_BMI, HE_DM, BS3_1, edu, marri_1, ainc, incm, EC_wht_5, BS1_1, BS3_1, BD1_11, BD1, BD2_31, BD2_32, BD2_1, BE3_31, BE3_32, BE3_33, BE3_71, BE3_72, BE3_73, BE3_74, BE3_81, BE3_82, BE3_83, BE3_84, BE3_75, BE3_76, BE3_77, BE3_78, BE3_85, BE3_86, BE3_87, BE3_88, BE3_91, BE3_92, BE3_93, BE3_94, mh_stress, BP6_10, BP5, N_EN, N_SUGAR, sm_presnt, dr_month, pa_aerobic, DE1_dg, N_FAT)

hn17_all_sub <- hn17_all %>% select(year, sex, age, occp, HE_BMI, HE_DM, BS3_1, edu, marri_1, ainc, incm, EC_wht_5, BS1_1, BS3_1, BD1_11, BD1, BD2_31, BD2_32, BD2_1, BE3_31, BE3_32, BE3_33, BE3_71, BE3_72, BE3_73, BE3_74, BE3_81, BE3_82, BE3_83, BE3_84, BE3_75, BE3_76, BE3_77, BE3_78, BE3_85, BE3_86, BE3_87, BE3_88, BE3_91, BE3_92, BE3_93, BE3_94, mh_stress, BP6_10, BP5, N_EN, N_SUGAR, sm_presnt, dr_month, pa_aerobic, DE1_dg, N_FAT)

hn16_all_sub <- hn16_all %>% select(year, sex, age, occp, HE_BMI, HE_DM, BS3_1, edu, marri_1, ainc, incm, EC_wht_5, BS1_1, BS3_1, BD1_11, BD1, BD2_31, BD2_32, BD2_1, BE3_31, BE3_32, BE3_33, BE3_71, BE3_72, BE3_73, BE3_74, BE3_81, BE3_82, BE3_83, BE3_84, BE3_75, BE3_76, BE3_77, BE3_78, BE3_85, BE3_86, BE3_87, BE3_88, BE3_91, BE3_92, BE3_93, BE3_94, mh_stress, BP6_10, BP5, N_EN, N_SUGAR, sm_presnt, dr_month, pa_aerobic, DE1_dg, N_FAT)

# 전체 데이터셋

hn_all <- rbind(hn18_all_sub, hn17_all_sub, hn16_all_sub)

### 19세 미만 데이터 제거, 결측치 제거

# 결측치 확인

colSums(is.na(hn_all))

########### 결측치 대체 ############

# 전체 데이터셋에서 19미만 제거

# 종속변수로 쓰일 HE_BMI, HE_DM 은 결측치일 경우 drop처리

## HE_BMI, HE_DM, incm, occp, edu 결측치있는 행 제거

hn_all <- hn_all[!(hn_all$age < 19),] # 24269 > 19389 (4880 제거)

hn_all <- hn_all[complete.cases(hn_all[, c("HE_BMI")]), ] # 19389 > 18489 (900 제거)

hn_all <- hn_all[complete.cases(hn_all[, c("HE_DM")]), ] # 18489 > 17285 (1204 제거)

hn_all <- hn_all[complete.cases(hn_all[, c("incm")]), ] # 17285 > 17230 (55 제거)

hn_all <- hn_all[complete.cases(hn_all[, c("occp")]), ] # 17230 > 16469 (761 제거)

hn_all <- hn_all[complete.cases(hn_all[, c("edu")]), ] # 16469 > 16460 (9 제거)

hn_all <- hn_all[!(hn_all$BS3_1 == 9), ] # 16460 > 16429 (31 제거)

hn_all <- hn_all[!(hn_all$EC_wht_5 == 8), ] # 16429 > 16415 (14 제거)

hn_all <- hn_all[!(hn_all$EC_wht_5 == 99), ] # 16415 > 16406 (9 제거)

hn_all <- hn_all[complete.cases(hn_all[, c("mh_stress")]), ] # 16469 > 16399 (7 제거)

hn_all <- hn_all[complete.cases(hn_all[, c("pa_aerobic")]), ] # 16399 > 16357 (42 제거)

hn_all <- hn_all[complete.cases(hn_all[, c("N_EN", "N_SUGAR")]), ] # 16357 >  14313 (2044 제거)


# 결측치 x

colSums(is.na(hn_all))


sum(is.na(hn_all))



######## 파생변수 ########

# 당뇨병 인지율 변수 

# 8번은 항목에 없음. (설문항목)

hn_all <- transform(hn_all, DM_recog = ifelse(HE_DM == 3, 1, 
                                                      ifelse(DE1_dg == 1, 1, 0)))


hn_all$DM_recog




###############################################

# EC_wht_5 ((1,2,3)은 교대근무x, 따라서 1로 변환. (4,5,6,7)은 교대근무 o, 따라서 2로 변환, 나머지 그대로 진행. (1,2,8,88,99) 로 더미변수 생성.

hn_all$EC_wht_5[hn_all$EC_wht_5 == 2] <- 1
hn_all$EC_wht_5[hn_all$EC_wht_5 == 3] <- 1
hn_all$EC_wht_5[hn_all$EC_wht_5 == 4] <- 2
hn_all$EC_wht_5[hn_all$EC_wht_5 == 5] <- 2
hn_all$EC_wht_5[hn_all$EC_wht_5 == 6] <- 2
hn_all$EC_wht_5[hn_all$EC_wht_5 == 7] <- 2

## 구간 나눈 후 더미변수 만들기

# HE_BMI 를 구간별로 나눈후 HE_BMI_band 라는 새로운 변수 생성(1 : 정상, 2 : 비만)

hn_all$HE_BMI_band = 1
hn_all$HE_BMI_band[hn_all$HE_BMI>=25]=2

# 구간으로 나눈 HE_BMI_band 더미변수로 만들기


# HE_BMI 를 구간별로 나눈후 HE_BMI_cate 라는 새로운 변수 생성(1 : 저체중, 2 : 정상, 3 : 과체중)

hn_all$HE_BMI_cate = 1
hn_all$HE_BMI_cate[hn_all$HE_BMI>=18.5]=2
hn_all$HE_BMI_cate[hn_all$HE_BMI>=25]=3

# ainc 를 구간별로 나눈후 ainc_band 라는 새로운 변수 생성

hn_all$ainc_band[hn_all$ainc<250] = 1
hn_all$ainc_band[hn_all$ainc>=250 & hn_all$ainc<500]=2
hn_all$ainc_band[hn_all$ainc>=500 & hn_all$ainc<=1500]=3

# HE_DM 1,2는 당뇨x(0), 3은 당뇨(1)

# HE_DM이 2면 0, 3이면 1로 바꾸는 코드

hn_all$HE_DM[hn_all$HE_DM == 1] <- 0
hn_all$HE_DM[hn_all$HE_DM == 2] <- 0
hn_all$HE_DM[hn_all$HE_DM == 3] <- 1

# 연령

hn_all$age_cate[hn_all$age>=19 & hn_all$age<=39]=1
hn_all$age_cate[hn_all$age>=40 & hn_all$age<=49]=2
hn_all$age_cate[hn_all$age>=50 & hn_all$age<=59]=3
hn_all$age_cate[hn_all$age>=60 & hn_all$age<=69]=4
hn_all$age_cate[hn_all$age>=70]=5

# edu 초중/고/대 3개로 나누기

hn_all$edu3G[hn_all$edu == 1] <- 1
hn_all$edu3G[hn_all$edu == 2] <- 1
hn_all$edu3G[hn_all$edu == 3] <- 2
hn_all$edu3G[hn_all$edu == 4] <- 3





######## 파생변수 ########

# 스트레스+자살+우울 3종세트 변수

hn_all <- transform(hn_all, ex_stress = ifelse(mh_stress == 1, 1, 
                                             ifelse(BP6_10 == 1, 1,
                                                    ifelse(BP5 == 1, 1, 0))))


# 고위험음주율(dr_high, 변수설명이 없음.)

# 고위험음주율을 구하기 위한 지표변수(dr_high_ind_1(남), dr_high_ind_2(여)) 생성.

# 남자

hn_all <- transform(hn_all, dr_high_ind_1 = ifelse(sex == 1, 1,
                                                   ifelse(BD2_31 == 1, 1,
                                                          ifelse(BD2_31 == 2, 1,
                                                                 ifelse(BD2_31 == 3, 1,
                                                                        ifelse(BD2_31 == 4, 1, 
                                                                               ifelse(BD2_31 == 5, 1,
                                                                                      ifelse(BD2_31 == 8, 1, 0))))))))

hn_all <- transform(hn_all, dr_high_ind_1 = ifelse(BD2_31 == 9, 0, 1))

# 여자

hn_all <- transform(hn_all, dr_high_ind_2 = ifelse(sex == 2, 1,
                                                   ifelse(BD2_32 == 1, 1,
                                                          ifelse(BD2_32 == 2, 1,
                                                                 ifelse(BD2_32 == 3, 1,
                                                                        ifelse(BD2_32 == 4, 1, 
                                                                               ifelse(BD2_32 == 5, 1,
                                                                                      ifelse(BD2_32 == 8, 1, 0))))))))

hn_all <- transform(hn_all, dr_high_ind_2 = ifelse(BD2_32 == 9, 0, 1))

# 지표변수(hr_high_ind_1,2)를 이용하여 hr_digh 만들기.

hn_all <- transform(hn_all, dr_high = ifelse(dr_high_ind_1 == 1, 1,
                                             ifelse(BD1_11 == 5, 1,
                                                    ifelse(BD1_11 == 6, 1,
                                                           ifelse(BD2_1 == 3, 1,
                                                                  ifelse(BD2_1 == 4, 1, 0))))))


hn_all <- transform(hn_all, dr_high = ifelse(dr_high_ind_2 == 1, 1,
                                             ifelse(BD1_11 == 5, 1,
                                                    ifelse(BD1_11 == 6, 1,
                                                           ifelse(BD2_1 == 3, 1,
                                                                  ifelse(BD2_1 == 4, 1, 
                                                                         ifelse(BD2_1 == 5, 1, 0)))))))


hn_all <- transform(hn_all, dr_high = ifelse(dr_high_ind_1 == 0, 0, 1))



## 걷기 실천율 변수 만들기 위한 pa_hb30_1

hn_all$pa_hb30_1 <- hn_all$BE3_32*60 + hn_all$BE3_33*1

# 걷기 실천율(pa_walk) (변수설명없음.)

hn_all <- transform(hn_all, pa_walk = ifelse(BE3_31 == 6, 1, 
                                             ifelse(BE3_31 == 7, 1,
                                                    ifelse(BE3_31 == 8, 1,
                                                           ifelse(pa_hb30_1 >=30, 1, 0)))))







## 더미변수를 제외한 연속형 변수, 더미변수에 쓰인 구간변수(_band 변수) 삭제

## 파생변수를 제외한 나머지 변수 삭제(파생변수를 위한 파생변수 pa_hb30_1 등 포함)

## 유산소 변수를 제외한 나머지 변수 삭제(파생변수를 위한 파생변수 vig_t2 등 포함)

hn_all <- subset(hn_all, select = -c(BS1_1, BD1_11, BD1, BD2_31, BD2_32, BD2_1, BE3_31, BE3_32, BE3_33, pa_hb30_1, dr_high_ind_1, dr_high_ind_2, BE3_71, BE3_72, BE3_73, BE3_74, BE3_81, BE3_82, BE3_83, BE3_84, BE3_75, BE3_76, BE3_77, BE3_78, BE3_85, BE3_86, BE3_87, BE3_88, BE3_91, BE3_92, BE3_93, BE3_94))


# 만들어진 데이터 csv 파일로 추출

hn_all_der <- hn_all %>% select(dr_high, pa_walk)

write.csv(hn_all_der, "hn_all_der.csv")

write.csv(hn_all, "hn_all.csv")

head(hn_all)
