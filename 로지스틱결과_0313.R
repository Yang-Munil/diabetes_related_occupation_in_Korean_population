## 1차 전처리 데이터 + 파생변수 데이터 합치기 ##

set.seed(1000)

hn_all <- read.csv("hn_all.csv", header = TRUE, sep = ",")
hn_all_der <- read.csv("hn_all_der.csv", header = TRUE, sep = ",")

# 컬럼 X 제거(갑자기 생김), 다중 로지스틱 회귀분석을 위해 year더미변수 제거

hn_all <- subset(hn_all, select = -c(X, year_2016, year_2017, year_2018))
hn_all_der <- subset(hn_all_der, select = -c(X))

# 1차전처리 + 파생변수 데이터 합치기

hn_all <- cbind(hn_all, hn_all_der)

str(hn_all)

colSums(is.na(hn_all))


# 다중 로지스틱 회귀분석

multi_logic = glm(formula = HE_DM ~ sex_1 + sex_2 + occp_1 + occp_2 + occp_3 + occp_4 + occp_5 + occp_6 + occp_7 + BS3_1_1 + BS3_1_2 + BS3_1_3 + BS3_1_8 + BS3_1_9 + educ_1 + educ_2 + educ_3 + educ_4 + educ_5 + educ_6 + educ_7 + educ_8 + educ_99 + marri_1_2 + marri_1_2 + incm_1 + incm_2 + incm_3 + incm_4 + EC_wht_5_1 + EC_wht_5_2 + EC_wht_5_8 + EC_wht_5_88 + EC_wht_5_99 + age_band_1 + age_band_2 + age_band_3 + age_band_4 + age_band_5 + age_band_6 + HE_BMI_band_1 + HE_BMI_band_2 + ainc_1 + ainc_2 + ainc_3 + ainc_4 + sm_presnt + dr_month + dr_high + pa_walk, data = hn_all, family = binomial)

summary(multi_logic)

