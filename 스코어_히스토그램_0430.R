library(ggplot2)


df <- read.csv("C:/rproject/기준시점_8_9_10_11_타겟값_데이터_중복제거_last_조정전후스코어_타겟값.csv")

round(prop.table(table(df$TARGET))*100, 2)


df1 <- df[df$seg == 1,]
df2 <- df[df$seg == 2,]
df3 <- df[df$seg == 3,]

round(prop.table(table(df1$TARGET))*100, 2)
round(prop.table(table(df2$TARGET))*100, 2)
round(prop.table(table(df3$TARGET))*100, 2)


df3$mySum_adj <- df3$mySum_no + 557

df$seg[df$seg == 1] <- '단기'
df$seg[df$seg == 2] <- '장기'
df$seg[df$seg == 3] <- '결손'


df$seg <- as.factor(df$seg)


### 단기, 장기, 결손 강제조정 후 스코어 분포

# 단기

ggplot(df1, aes(x=mySum)) + 
  geom_histogram(color = 'black', fill="gray", alpha=0.7, binwidth = 10) + 
  ggtitle("04월 단기그룹 강제조정 후 분포") +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black")) +
  labs(x="스코어", y="인원수") +
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black")) +
  ggsave(file="C:/rproject/그룹별_월별_스코어분포/04월 단기그룹 강제조정 후 분포.jpg")

# 장기

ggplot(df2, aes(x=mySum)) + 
  geom_histogram(color = 'black', fill="gray", alpha=0.7, binwidth = 10) + 
  ggtitle("04월 장기그룹 강제조정 후 분포") +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black")) +
  labs(x="스코어", y="인원수") +
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black")) +
  ggsave(file="C:/rproject/그룹별_월별_스코어분포/04월 장기그룹 강제조정 후 분포.jpg")

# 결손 

ggplot(df3, aes(x=mySum)) + 
  geom_histogram(color = 'black', fill="gray", alpha=0.7, binwidth = 10) + 
  ggtitle("04월 결손그룹 강제조정 후 분포") +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black")) +
  labs(x="스코어", y="인원수") +
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black")) +
  ggsave(file="C:/rproject/그룹별_월별_스코어분포/04월 결손그룹 강제조정 후 분포.jpg")




# 단기그룹 강제조정 전

ggplot(df1, aes(x=mySum_no)) + 
  geom_histogram(color = 'black', fill="gray", alpha=0.7, binwidth = 10) + 
  ggtitle("8월 단기그룹 조정 전 분포") +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black")) +
  labs(x="스코어", y="인원수") +
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black"))

# 장기그룹 강제조정 전

ggplot(df2, aes(x=mySum_no)) + 
  geom_histogram(color = 'black', fill="gray", alpha=0.7, binwidth = 10) + 
  ggtitle("8월 장기그룹 조정 전 분포") +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black")) +
  labs(x="스코어", y="인원수") +
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black"))


# 결손그룹 강제조정 전

ggplot(df3, aes(x=mySum_adj)) + 
  geom_histogram(color = 'black', fill="gray", alpha=0.7, binwidth = 10) + 
  geom_vline(aes(xintercept = 0), color = "red", linetype = "dashed", size = 1, ) + 
  geom_vline(aes(xintercept = 780), color = "blue", linetype = "dashed", size = 1) + 
  ggtitle("8월 결손그룹 0점, 780점 강제조정 전 분포") +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black")) +
  labs(x="스코어", y="인원수") +
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black"))

# 결손그룹 강제조정 후

ggplot(df3, aes(x=mySum)) + 
  geom_histogram(color = 'black', fill="gray", alpha=0.7, binwidth = 10) + 
  geom_vline(aes(xintercept = 0), color = "red", linetype = "dashed", size = 1) + 
  geom_vline(aes(xintercept = 780), color = "blue", linetype = "dashed", size = 1) + 
  ggtitle("8월 결손그룹 0점, 780점 강제조정 후 분포") +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black")) +
  labs(x="스코어",y="인원수") +
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black"))

# 하나의 히스토그램에 모든 그룹 표현하기

# 강제조정 전

ggplot(df, aes(x=mySum_no, fill = seg, color = seg)) +
  geom_histogram(aes(y=..density..), alpha=0.5, position = "dodge", binwidth = 50) +
  theme(legend.position = "top") +
  theme_minimal() +
  theme(legend.position = "top") +
  ggtitle("8월 전체 그룹 강제조정 전 스코어 분포") +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black")) +
  labs(x="스코어", y="인원수") +
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black"))

# 강제조정 후

ggplot(df, aes(x=mySum, fill = seg, color = seg)) +
  geom_histogram(aes(y=..density..), alpha=0.5, position = "dodge", binwidth = 50) +
  theme(legend.position = "top") +
  theme_minimal() +
  geom_vline(aes(xintercept = 965), color = "red", linetype = "dashed", size = 1) + 
  geom_vline(aes(xintercept = 877), color = "blue", linetype = "dashed", size = 1) + 
  geom_vline(aes(xintercept = 693), color = "purple", linetype = "dashed", size = 1) + 
  geom_vline(aes(xintercept = 530), color = "black", linetype = "dashed", size = 1) +
  theme(legend.position = "top") +
  ggtitle("8월 전체 그룹 강제조정 후 스코어 분포") +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black")) +
  labs(x="스코어", y="인원수") +
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black"))

