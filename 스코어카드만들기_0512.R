
library(ggplot2)
library(Rprofet)


df2 <- read.csv("C:/rproject/df2_columns_select_final.csv")


# 장기


binned <- BinProfet(df2, id= "TPR_NO_ENC", target= "TARGET", num.bins = 5) ## Binning variables


WOE_dat <- WOEProfet(binned, "TPR_NO_ENC", "TARGET", 3:24)



Score_dat <- ScorecardProfet(WOE_dat, target="TARGET",
                             id= "TPR_NO_ENC", PDO = 50, BaseOdds = 10, BasePts = 1000, reverse = TRUE)

df2_scorecard <- Score_dat$Scorecard

df2_score <- Score_dat$Results

# 스코어기준점 추출

write.csv(df2_scorecard, "df2_scorecard_final.csv")

# 장기그룹 파일럿 변수 시각화

names(binned)

WOEplotter(binned, target= "TARGET", var= "A07_SNP_AMT_DUE_3Y_Bins")

WOEplotter(binned, target= "TARGET", var= "A07_SNP_CNT_DUE_FIX_3Y_Bins")

WOEplotter(binned, target= "TARGET", var= "A07_SNP_IN_ACCT_VR_1Y_Bins")

WOEplotter(binned, target= "TARGET", var= "A09_SEIZ_BOOL_Bins")

WOEplotter(binned, target= "TARGET", var= "A09_SEIZ_CNT_SGG_Bins")

WOEplotter(binned, target= "TARGET", var= "A09_SEIZ_GDS_CAR_Bins")

WOEplotter(binned, target= "TARGET", var= "A02_CNP_CNT_Bins")

WOEplotter(binned, target= "TARGET", var= "C1L120003_Bins")

WOEplotter(binned, target= "TARGET", var= "C1L120014_Bins")

WOEplotter(binned, target= "TARGET", var= "C1N2BF003_Bins")

WOEplotter(binned, target= "TARGET", var= "C1Z000943_Bins")

WOEplotter(binned, target= "TARGET", var= "C1Z001221_Bins")

WOEplotter(binned, target= "TARGET", var= "C1Z001222_Bins")

WOEplotter(binned, target= "TARGET", var= "D20110000_Bins")

WOEplotter(binned, target= "TARGET", var= "D20131000_Bins")

WOEplotter(binned, target= "TARGET", var= "D20210000_Bins")

WOEplotter(binned, target= "TARGET", var= "D2Z000065_Bins")

WOEplotter(binned, target= "TARGET", var= "KGRAD_Bins")

WOEplotter(binned, target= "TARGET", var= "S900010Z0_Bins")

WOEplotter(binned, target= "TARGET", var= "A09_SEIZ_CNT_SGG_3Y_Bins")

WOEplotter(binned, target= "TARGET", var= "A09_SEIZ_GDS_CAR_3Y_Bins")

WOEplotter(binned, target= "TARGET", var= "A02_CNP_CNT_3YB_Bins")

# 장기그룹 체납자, 수납자

df2_score$TARGET <- as.factor(df2_score$TARGET)

ggplot(df2_score, aes(x=Score, fill = TARGET, color = TARGET)) +
  geom_histogram(aes(y=..density..), alpha=0.5, position = "dodge", binwidth = 15) +
  theme(legend.position = "top") +
  theme_minimal() +
  theme(legend.position = "top") +
  ggtitle("장기그룹 체납자, 수납자 스코어 분포(파일럿)") +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black")) +
  labs(x="스코어", y="인원수") +
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black"))










# 단기


binned <- BinProfet(df1, id= "TPR_NO_ENC", target= "TARGET", num.bins = 5) ## Binning variables


WOE_dat <- WOEProfet(binned, "TPR_NO_ENC", "TARGET", 3:40)


head(WOE_dat$BinWOE)

head(WOE_dat$WOE)

WOE_dat$IV

Score_dat <- ScorecardProfet(WOE_dat, target="TARGET",
                             id= "TPR_NO_ENC", PDO = 50, BaseOdds = 10, BasePts = 1000, reverse = TRUE)

df1_scorecard <- Score_dat$Scorecard

df1_score <- Score_dat$Results

# 단기그룹 체납자, 수납자

df1_score$TARGET <- as.factor(df1_score$TARGET)

ggplot(df1_score, aes(x=Score, fill = TARGET, color = TARGET)) +
  geom_histogram(aes(y=..density..), alpha=0.5, position = "dodge", binwidth = 15) +
  theme(legend.position = "top") +
  theme_minimal() +
  theme(legend.position = "top") +
  ggtitle("단기그룹 체납자, 수납자 스코어 분포") +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black")) +
  labs(x="스코어", y="인원수") +
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black"))




# 스코어기준점 추출

write.csv(df1_scorecard, "df1_scorecard.csv")




