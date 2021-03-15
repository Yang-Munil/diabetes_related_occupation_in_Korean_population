library(haven)
library(tidyverse)

##### 유산소신체활동실천율 변수 생성 ####


### 변수생성시 유의사항 : 비해당, 무응답 인원은 배제하고 만들어야하기 때문에 조건문 코드작성시 주의.


################파생변수 생성을 위한 새로운 전체 데이터 셋#####################

# 데이터 불러오기

hn18_all <- read_sas("C:/rproject/startr/hn18_all.sas7bdat", NULL)
hn17_all <- read_sas("C:/rproject/startr/hn17_all.sas7bdat", NULL)
hn16_all <- read_sas("C:/rproject/startr/hn16_all.sas7bdat", NULL)


# 유산소 파생변수를 만들기 위한 변수만 선택

hn18_all_sub <- hn18_all %>% select(sex, BE3_71, BE3_72, BE3_73, BE3_74, BE3_81, BE3_82, BE3_83, BE3_84, BE3_75, BE3_76, BE3_77, BE3_78, BE3_85, BE3_86, BE3_87, BE3_88, BE3_91, BE3_92, BE3_93, BE3_94, age, occp, HE_BMI, HE_DM, incm, ainc, edu)

hn17_all_sub <- hn17_all %>% select(sex, BE3_71, BE3_72, BE3_73, BE3_74, BE3_81, BE3_82, BE3_83, BE3_84, BE3_75, BE3_76, BE3_77, BE3_78, BE3_85, BE3_86, BE3_87, BE3_88, BE3_91, BE3_92, BE3_93, BE3_94, age, occp, HE_BMI, HE_DM, incm, ainc, edu)

hn16_all_sub <- hn16_all %>% select(sex, BE3_71, BE3_72, BE3_73, BE3_74, BE3_81, BE3_82, BE3_83, BE3_84, BE3_75, BE3_76, BE3_77, BE3_78, BE3_85, BE3_86, BE3_87, BE3_88, BE3_91, BE3_92, BE3_93, BE3_94, age, occp, HE_BMI, HE_DM, incm, ainc, edu)

## 파생변수 만들기 위한 전체 데이터셋

hn_all <- rbind(hn18_all_sub, hn17_all_sub, hn16_all_sub)
hn_all <- hn_all[complete.cases(hn_all[, c("HE_BMI", "HE_DM", "incm", "ainc", "occp", "edu")]), ]

hn_all <- hn_all[!(hn_all$age < 19),]

colSums(is.na(hn_all))

## 일 관련 고강도 신체활동 시간

# pa_hb28_2, vig_t1 변수 만들기 위한 지표변수(indicator variable) 생성(pa_vig_ind)

hn_all <- transform(hn_all, pa_vig_ind = ifelse(BE3_71 == 1, 1,
                                             ifelse(BE3_72 == 1, 1,
                                                    ifelse(BE3_72 == 2, 1,
                                                           ifelse(BE3_72 == 3, 1,
                                                                  ifelse(BE3_72 == 4, 1, 
                                                                         ifelse(BE3_72 == 5, 1,
                                                                                ifelse(BE3_72 == 6, 1,
                                                                                       ifelse(BE3_72 == 7, 1, 0)))))))))

hn_all <- transform(hn_all, pa_vig_ind = ifelse(BE3_73 == 88, 0,
                                                ifelse(BE3_73 == 99, 0,
                                                       ifelse(BE3_74 == 88, 0,
                                                              ifelse(BE3_74 == 99, 0, 1)))))
                    
hn_all <- transform(hn_all, pa_vig_ind = ifelse(BE3_71 == 9, 0,
                                                ifelse(BE3_72 == 9, 0,
                                                       ifelse(BE3_72 == 8, 0, 1))))

# 생성된 지표변수(pa_vig_ind)를 이용하여 vig_t1 생성


hn_all$pa_hb28_2 <- hn_all$BE3_73*60 + hn_all$BE3_74*1
hn_all$pa_hb28_2 <- hn_all$pa_vig_ind*(hn_all$BE3_73*60 + hn_all$BE3_74*1)
hn_all$vig_t1 <- hn_all$BE3_72*hn_all$pa_hb28_2


## 일 관련 증강도 신체활동 시간

# pa_hb29_2, mod_t1 변수 만들기 위한 지표변수(indicator variable) 생성(pa_mod_ind)

hn_all <- transform(hn_all, pa_mod_ind = ifelse(BE3_81 == 1, 1,
                                                ifelse(BE3_82 == 1, 1,
                                                       ifelse(BE3_82 == 2, 1,
                                                              ifelse(BE3_82 == 3, 1,
                                                                     ifelse(BE3_82 == 4, 1, 
                                                                            ifelse(BE3_82 == 5, 1,
                                                                                   ifelse(BE3_82 == 6, 1,
                                                                                          ifelse(BE3_82 == 7, 1, 0)))))))))

hn_all <- transform(hn_all, pa_mod_ind = ifelse(BE3_83 == 88, 0,
                                                ifelse(BE3_83 == 99, 0,
                                                       ifelse(BE3_84 == 88, 0,
                                                              ifelse(BE3_84 == 99, 0, 1)))))


hn_all <- transform(hn_all, pa_mod_ind = ifelse(BE3_81 == 9, 0,
                                                ifelse(BE3_82 == 9, 0,
                                                       ifelse(BE3_82 == 8, 0, 1))))


# 생성된 지표변수(pa_mod_ind)를 이용하여 mod_t1 생성

hn_all$pa_hb29_2 <- hn_all$pa_mod_ind*(hn_all$BE3_83*60 + hn_all$BE3_84*1)
hn_all$mod_t1 <- hn_all$BE3_82*hn_all$pa_hb29_2

## 여가 관련 고강도 신체활동 시간

# pa_hb28_3, vig_t2 변수 만들기 위한 지표변수(indicator variable) 생성(pa_vig2_ind)

hn_all <- transform(hn_all, pa_vig2_ind = ifelse(BE3_75 == 1, 1,
                                                ifelse(BE3_76 == 1, 1,
                                                       ifelse(BE3_76 == 2, 1,
                                                              ifelse(BE3_76 == 3, 1,
                                                                     ifelse(BE3_76 == 4, 1, 
                                                                            ifelse(BE3_76 == 5, 1,
                                                                                   ifelse(BE3_76 == 6, 1,
                                                                                          ifelse(BE3_76 == 7, 1, 0)))))))))

hn_all <- transform(hn_all, pa_vig2_ind = ifelse(BE3_77 == 88, 0,
                                                 ifelse(BE3_77 == 99, 0,
                                                        ifelse(BE3_78 == 88, 0,
                                                               ifelse(BE3_78 == 99, 0, 1)))))

hn_all <- transform(hn_all, pa_vig2_ind = ifelse(BE3_75 == 9, 0,
                                                ifelse(BE3_76 == 9, 0,
                                                       ifelse(BE3_76 == 8, 0, 1))))

# 생성된 지표변수(pa_vig2_ind)를 이용하여 vig_t2 생성

hn_all$pa_hb28_3 <- hn_all$pa_vig2_ind*(hn_all$BE3_77*60 + hn_all$BE3_78*1)
hn_all$vig_t2 <- hn_all$BE3_76*hn_all$pa_hb28_3

## 여가 관련 증강도 신체활동 시간

# pa_hb29_3, mod_t2 변수 만들기 위한 지표변수(indicator variable) 생성(pa_mod2_ind)

hn_all <- transform(hn_all, pa_mod2_ind = ifelse(BE3_85 == 1, 1,
                                                 ifelse(BE3_86 == 1, 1,
                                                        ifelse(BE3_86 == 2, 1,
                                                               ifelse(BE3_86 == 3, 1,
                                                                      ifelse(BE3_86 == 4, 1, 
                                                                             ifelse(BE3_86 == 5, 1,
                                                                                    ifelse(BE3_86 == 6, 1,
                                                                                           ifelse(BE3_86 == 7, 1, 0)))))))))

hn_all <- transform(hn_all, pa_vig2_ind = ifelse(BE3_87 == 88, 0,
                                                 ifelse(BE3_87 == 99, 0,
                                                        ifelse(BE3_88 == 88, 0,
                                                               ifelse(BE3_88 == 99, 0, 1)))))

hn_all <- transform(hn_all, pa_vig2_ind = ifelse(BE3_85 == 9, 0,
                                                 ifelse(BE3_86 == 9, 0,
                                                        ifelse(BE3_86 == 8, 0, 1))))

# 생성된 지표변수(pa_mod2_ind)를 이용하여 mod_t2 생성

hn_all$pa_hb29_3 <- hn_all$pa_mod2_ind*(hn_all$BE3_87*60 + hn_all$BE3_88*1)
hn_all$mod_t2 <- hn_all$BE3_86*hn_all$pa_hb29_3

## 장소이동 관련 신체활동 시간

# pa_hb30_2, walk_t2 변수 만들기 위한 지표변수(indicator variable) 생성(pa_walk2_ind)

hn_all <- transform(hn_all, pa_walk2_ind = ifelse(BE3_91 == 1, 1,
                                                 ifelse(BE3_92 == 1, 1,
                                                        ifelse(BE3_92 == 2, 1,
                                                               ifelse(BE3_92 == 3, 1,
                                                                      ifelse(BE3_92 == 4, 1, 
                                                                             ifelse(BE3_92 == 5, 1,
                                                                                    ifelse(BE3_92 == 6, 1,
                                                                                           ifelse(BE3_92 == 7, 1, 0)))))))))

hn_all <- transform(hn_all, pa_walk2_ind = ifelse(BE3_93 == 88, 0,
                                                 ifelse(BE3_93 == 99, 0,
                                                        ifelse(BE3_94 == 88, 0,
                                                               ifelse(BE3_94 == 99, 0, 1)))))

hn_all <- transform(hn_all, pa_walk2_ind = ifelse(BE3_91 == 9, 0,
                                                 ifelse(BE3_92 == 9, 0,
                                                        ifelse(BE3_92 == 8, 0, 1))))

# 생성된 지표변수(pa_mod2_ind)를 이용하여 walk_t2 생성

hn_all$pa_hb30_2 <- hn_all$pa_walk2_ind*(hn_all$BE3_93*60 + hn_all$BE3_94*1)
hn_all$walk_t2 <- hn_all$BE3_92*hn_all$pa_hb30_2

### 유산소신체활동실천율 계산 위한 파생변수

# vig_t / vig_tt2 / sum_mw / hour_vm

hn_all$vig_t = hn_all$vig_t1 + hn_all$vig_t2

hn_all$vig_tt2 = (hn_all$vig_t1)*2

hn_all$sum_mw = hn_all$mod_t1 + hn_all$walk_t2 + hn_all$mod_t2

hn_all$hour_vm <- hn_all$vig_tt2 + hn_all$sum_mw


## 유산소신체활동실천율

# pa_aerobic

# pa_aerobic = (sum_mw>=150 or vig_t>=75 or hour_vm>=150) << 3가지 조건 동시에 만족할 필요 없음. 각각 만족해도 됨. 따라서 ifelse 조건문 3개로 변수생성. 


hn_all <- transform(hn_all, pa_aerobic = ifelse(sum_mw >= 150, 1, 0))

hn_all <- transform(hn_all, pa_aerobic = ifelse(vig_t >= 75, 1, 0))

hn_all <- transform(hn_all, pa_aerobic = ifelse(hour_vm >= 150, 1, 0))

# 유산소신체활동실천율 컬럼만 남기기

hn_all_aero <- hn_all %>% select(pa_aerobic)
hn_all_aero

# 만들어진 파생변수 데이터 csv 파일로 추출

write.csv(hn_all_aero, "hn_all_aero.csv")
