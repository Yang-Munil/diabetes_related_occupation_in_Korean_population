## 종속변수, 독립변수 범주/계급 구성 분포 확인 및 처리

hn_all <- read.csv("hn_all.csv", sep = ",")
hn_all_der <- read.csv("hn_all_der.csv", sep = ",")
hn_all_aero <- read.csv("hn_all_aero.csv", sep = ",")

# 컬럼 X 제거(갑자기 생김), 계급별 분포보기 위해 year더미변수 제거

hn_all <- subset(hn_all, select = -c(X, year_2016, year_2017, year_2018))
hn_all_der <- subset(hn_all_der, select = -c(X))
hn_all_aero <- subset(hn_all_aero, select = -c(X))

# 전처리된 데이터 합치기

hn_all_final <- cbind(hn_all, hn_all_der, hn_all_aero)
hn_all_final

# 종속변수(HE_DM) 비율.

table(hn_all_final$HE_DM)

cat("total :", margin.table(table(hn_all_final$HE_DM)))

prop.table(table(hn_all_final$HE_DM))

