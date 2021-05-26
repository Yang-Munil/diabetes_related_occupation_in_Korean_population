library(scorecardModelUtils)
library(ggplot2)
library(Rprofet)

df3 <- read.csv("C:/rproject/df3_columns.csv")



binned <- BinProfet(df3, id= "TPR_NO_ENC", target= "TARGET", num.bins = 5) ## Binning variables



WOE_dat <- WOEProfet(binned, "TPR_NO_ENC", "TARGET", 3:281)




Score_dat <- ScorecardProfet(WOE_dat, target="TARGET",
                             id= "TPR_NO_ENC", PDO = 50, BaseOdds = 10, BasePts = 1000, reverse = TRUE)


df3_scorecard <- Score_dat$Scorecard

Score_dat$Scorecard

# 변수 WOE 시각화

names(binned)

WOEplotter(binned, target= "TARGET", var= "A07_SNP_AMT_DUE_1Y_Bins") # 합치기

WOEplotter(binned, target= "TARGET", var= "A07_SNP_CNT_DUE_FIX_3Y_Bins")

WOEplotter(binned, target= "TARGET", var= "A07_SNP_IN_ACCT_VR_1Y_Bins")

WOEplotter(binned, target= "TARGET", var= "A09_SEIZ_CNT_SGG_Bins")

WOEplotter(binned, target= "TARGET", var= "A02_CNP_AMT_3Y_Bins")

WOEplotter(binned, target= "TARGET", var= "A02_CNP_CNT_Bins")

WOEplotter(binned, target= "TARGET", var= "C1L120003_Bins")

WOEplotter(binned, target= "TARGET", var= "C1L120014_Bins")

WOEplotter(binned, target= "TARGET", var= "C1M123W06_Bins")

WOEplotter(binned, target= "TARGET", var= "C1N2B3003_Bins")

WOEplotter(binned, target= "TARGET", var= "C1Z000798_Bins") # 합치기

WOEplotter(binned, target= "TARGET", var= "C1Z000951_Bins")

WOEplotter(binned, target= "TARGET", var= "C1Z001221_Bins")

WOEplotter(binned, target= "TARGET", var= "C1Z001222_Bins")

WOEplotter(binned, target= "TARGET", var= "C20220000_Bins")

WOEplotter(binned, target= "TARGET", var= "C23220000_Bins")

WOEplotter(binned, target= "TARGET", var= "D20110000_Bins")

WOEplotter(binned, target= "TARGET", var= "D20131000_Bins") # 합치기

WOEplotter(binned, target= "TARGET", var= "D2Z000061_Bins")

WOEplotter(binned, target= "TARGET", var= "KGRAD_Bins") # 합치기

WOEplotter(binned, target= "TARGET", var= "U5Z20K010_Bins")

WOEplotter(binned, target= "TARGET", var= "U5Z21K010_Bins")

WOEplotter(binned, target= "TARGET", var= "A09_SEIZ_BOOL_3Y_Bins")

WOEplotter(binned, target= "TARGET", var= "A09_SEIZ_CNT_SGG_3Y_Bins")

WOEplotter(binned, target= "TARGET", var= "A02_CNP_AMT_3YB_Bins") # 수정

WOEplotter(binned, target= "TARGET", var= "A02_CNP_SNP_CNT_MM_3Y_3YB_Bins")

# 스코어기준점 추출

write.csv(df3_scorecard, "df3_scorecard_final.csv")

## Less points means more likely to default


df3_score <- Score_dat$Results




# 결손그룹 체납자, 수납자

df3_score$TARGET[df3_score$TARGET == 0] <- '체납자'
df3_score$TARGET[df3_score$TARGET == 1] <- '수납자'


df3_score$TARGET <- as.factor(df3_score$TARGET)

ggplot(df3_score, aes(x=Score, fill = TARGET, color = TARGET)) +
  geom_histogram(aes(y=..density..), alpha=0.5, position = "dodge", binwidth = 15) +
  theme(legend.position = "top") +
  theme_minimal() +
  theme(legend.position = "top") +
  ggtitle("결손그룹 체납자, 수납자 스코어 분포(파일럿)") +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black")) +
  labs(x="스코어", y="인원수") +
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black"))

# 12월 파일럿 결과

df3 <- read.csv("C:/rproject/df3_12월.csv")

df3$TARGET <- as.factor(df3$TARGET)

ggplot(df3, aes(x=mySum, fill = TARGET, color = TARGET)) +
  geom_histogram(aes(y=..density..), alpha=0.5, position = "dodge", binwidth = 15) +
  theme(legend.position = "top") +
  theme_minimal() +
  theme(legend.position = "top") +
  ggtitle("12월 결손그룹 체납자, 수납자 스코어 분포(파일럿)") +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black")) +
  labs(x="스코어", y="인원수") +
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black"))


# 01월 파일럿 결과

df3 <- read.csv("C:/rproject/df3_01월.csv")

df3$TARGET <- as.factor(df3$TARGET)

ggplot(df3, aes(x=mySum, fill = TARGET, color = TARGET)) +
  geom_histogram(aes(y=..density..), alpha=0.5, position = "dodge", binwidth = 5) +
  theme(legend.position = "top") +
  theme_minimal() +
  theme(legend.position = "top") +
  ggtitle("01월 결손그룹 체납자, 수납자 스코어 분포(파일럿)") +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black")) +
  labs(x="스코어", y="인원수") +
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black"))
