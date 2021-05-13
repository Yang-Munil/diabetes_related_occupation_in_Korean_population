library(devtools)
library(knitr)
library(DT)
library(sqldf)
library(ggplot2)
library(corrplot)
library(ROCR)
library(rpart)
library(nnet)
library(NeuralNetTools)
library(e1071)
library(randomForest)
library(woe)
library(haven)
library(tidyverse)
library(dplyr)


housing <- read_sas("C:/rproject/startr/housing.sas7bdat", NULL)
hmeq <- read_sas("C:/rproject/startr/hmeq.sas7bdat", NULL)
buytest <- read_sas("C:/rproject/startr/buytest.sas7bdat", NULL)

# buytest

colSums(is.na(buytest))

buytest <- buytest[complete.cases(buytest[, c("AGE")]), ] # 결측치 AGE 제거

head(buytest)

# Target값은 RESPOND

buytest$SEX[buytest$SEX == "M"] <- 1
buytest$SEX[buytest$SEX == "F"] <- 0

buytest$LOC[buytest$LOC == "A"] <- 1
buytest$LOC[buytest$LOC == "B"] <- 2
buytest$LOC[buytest$LOC == "C"] <- 3
buytest$LOC[buytest$LOC == "D"] <- 4
buytest$LOC[buytest$LOC == "E"] <- 5
buytest$LOC[buytest$LOC == "F"] <- 6
buytest$LOC[buytest$LOC == "G"] <- 7
buytest$LOC[buytest$LOC == "H"] <- 8

buytest$ORGSRC[buytest$ORGSRC == "C"] <- 1
buytest$ORGSRC[buytest$ORGSRC == "D"] <- 2
buytest$ORGSRC[buytest$ORGSRC == "I"] <- 3
buytest$ORGSRC[buytest$ORGSRC == ""] <- 4
buytest$ORGSRC[buytest$ORGSRC == "O"] <- 4
buytest$ORGSRC[buytest$ORGSRC == "R"] <- 5
buytest$ORGSRC[buytest$ORGSRC == "U"] <- 6


buytest <- subset(buytest, select = -c(ID))

colSums(is.na(buytest))


## RESPOND 비율

table(buytest$RESPOND)

cat("total :", margin.table(table(buytest$RESPOND)))

prop.table(table(buytest$RESPOND))





## 1) Information Value 값 확인

buytest <- data.frame(buytest)

buy_iv <- iv.mult(buytest, "RESPOND", TRUE)

head(buy_iv)

## 2) Information value 값 plot

iv.plot.summary(buy_iv)

## 3) 각 변수 WOE 생성

buy_woe <- iv.replace.woe(buytest, iv = iv.mult(buytest, "RESPOND"))

buy_woe <- data.frame(buy_woe)

head(buy_woe)





#### 4.1. 모형1 - 로지스틱 회귀모형

### Stepwise 변수선택 및 모형 산출

## 1) 모델식 ##

logit_model <- glm(
  RESPOND ~ AGE_woe+
    INCOME_woe+
    SEX_woe+
    MARRIED_woe+
    FICO_woe+
    OWNHOME_woe+
    LOC_woe+
    CLIMATE_woe+
    BUY6_woe+
    BUY12_woe+
    BUY18_woe+
    VALUE24_woe+
    ORGSRC_woe+
    DISCBUY_woe+
    RETURN24_woe+
    COA6_woe+
    C1_woe+
    C2_woe+
    C3_woe+
    C4_woe+
    C5_woe+
    C6_woe+
    C7_woe+
    PURCHTOT_woe,
  data = buy_woe, family = binomial())

## 2) Stepwise ##

model_1 <- step(logit_model)
summary(model_1)

## 3) 최종 모델식 ##

model1 <- glm(
  RESPOND ~ AGE_woe+
    FICO_woe+
    OWNHOME_woe+
    LOC_woe+
    BUY12_woe+
    BUY18_woe+
    ORGSRC_woe,
  data = buy_woe, family = binomial())

summary(model1)

## 4) 상관계수 확인 및 그래프 도출 ##

buy_corr <- sqldf("select AGE_woe, FICO_woe, OWNHOME_woe, LOC_woe, BUY12_woe, BUY18_woe, ORGSRC_woe from buy_woe")
X <- cor(buy_corr)
cor(buy_corr)
corrplot(X)

## 5) ROC curve, AUROC, KS, Gini 계수 등 주요 값 확인 ##
# ROC Curve #
# score test data

buy_woe$model1_score <- predict(model1, type='response', buy_woe)
model1_pred <- prediction(buy_woe$model1_score, buy_woe$RESPOND)
model1_RESPOND <- performance(model1_pred, "tpr", "fpr")

# ROC - Model performance plot
plot(model1_RESPOND, lwd=2, colorize=TRUE, main="ROC Model 1 : Logistic Regression Performance")
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);
lines(x=c(1, 0), y=c(0, 1), col="green", lwd=1, lty=4)

# AUC, KS, Gini 계수 #

model1_KS <- 
  round(max(attr(model1_RESPOND,'y.values')[[1]]-attr(model1_RESPOND,'x.values')[[1]])*100,2)

model1_AUROC <- 
  round(performance(model1_pred, measure = "auc")@y.values[[1]]*100, 2)

model1_Gini <- (2*model1_AUROC - 100)

cat("AUROC: ",model1_AUROC,"\tKS: ", model1_KS, "\tGini:", model1_Gini, "\n")


#### 4.2. 모형2 - 기계학습 모형 : Recursive Partitioning Tree (반복분할 트리)

### 1) Model 2 : Recursive Partitioning ###

model2 <- rpart(RESPOND ~ AGE_woe+
                  FICO_woe+
                  OWNHOME_woe+
                  LOC_woe+
                  BUY12_woe+
                  BUY18_woe+
                  ORGSRC_woe,
                data = buy_woe)
printcp(model2)
plot(model2, main="Tree:Recursive Partitioning");text(model2);

## 2) ROC curve, AUROC, KS, Gini 계수 등 주요 값 확인 ##
# ROC Curve #
# score test data

buy_woe$model2_score <- predict(model2, buy_woe)
model2_pred <- prediction(buy_woe$model2_score, buy_woe$RESPOND)
model2_RESPOND <- performance(model2_pred, "tpr", "fpr")

# ROC - Model performance plot
plot(model2_RESPOND, lwd=2, colorize=TRUE, main="ROC Model 2 : Traditional Recursive Partitioning")
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);
lines(x=c(1, 0), y=c(0, 1), col="green", lwd=1, lty=4)

# AUC, KS, Gini 계수 #

model2_KS <- 
  round(max(attr(model2_RESPOND,'y.values')[[1]]-attr(model2_RESPOND,'x.values')[[1]])*100,2)

model2_AUROC <- 
  round(performance(model2_pred, measure = "auc")@y.values[[1]]*100, 2)

model2_Gini <- (2*model2_AUROC - 100)

cat("AUROC: ",model2_AUROC,"\tKS: ", model2_KS, "\tGini:", model2_Gini, "\n")


# 4.3 기계학습 모형 : Neural Network (신경망)

## 1) Model 3 : Neural Network ##

# Normalize

train <- sqldf("select RESPOND, AGE_woe, FICO_woe, OWNHOME_woe, LOC_woe, BUY12_woe, BUY18_woe, ORGSRC_woe from buy_woe")
test <- sqldf("select RESPOND, AGE_woe, FICO_woe, OWNHOME_woe, LOC_woe, BUY12_woe, BUY18_woe, ORGSRC_woe from buy_woe")

train$RESPOND <- as.factor(train$RESPOND)
test$RESPOND <- as.factor(test$RESPOND)

normalize <- function(x){
  return((x - min(x) / (max(x) - min(x))))
}

train_norm <- as.data.frame(lapply(train[,2:7], normalize))
test_norm <- as.data.frame(lapply(test[,2:7], normalize))
train_norm$RESPOND <- as.factor(ifelse(train$RESPOND == 1, 1, 0))
test_norm$RESPOND <- as.factor(ifelse(test$RESPOND == 1, 1, 0))

# 2) build the neural network (NN) formula
a <- colnames(train[,2:7])
mformula <- as.formula(paste('RESPOND ~ ', paste(a, collapse = '+')))
set.seed(1000)
train_nn <- train_norm
test_nn <- test_norm

# 3) Modeling
model3 <- nnet(RESPOND~., data=train_nn, size=20, maxit=10000, decay=.001, linout=F, trace=F)
table(test_nn$RESPOND, predict(model3, newdata = test_nn, type="class"))

# 모형 plot 확인
plotnet(model3)

## 4) ROC 값 확인 ##

# score test data

model3_pred <- prediction(predict(model3, newdata = test_nn, type="raw"), test_nn$RESPOND)
model3_RESPOND <- performance(model3_pred, "tpr", "fpr")

# 5) ROC - Model performance plot
plot(model3_RESPOND, lwd=2, colorize=TRUE, main="ROC model3:ROC - Neural Network")
abline(a=0,b=1)

# 6) AUC, KS, Gini 계수 #

model3_KS <- 
  round(max(attr(model3_RESPOND,'y.values')[[1]]-attr(model3_RESPOND,'x.values')[[1]])*100,2)

model3_AUROC <- 
  round(performance(model3_pred, measure = "auc")@y.values[[1]]*100, 2)

model3_Gini <- (2*model3_AUROC - 100)

cat("AUROC: ",model3_AUROC,"\tKS: ", model3_KS, "\tGini:", model3_Gini, "\n")


# 4.4 기계학습 모형 : Random Forest (랜덤 포레스트)
## Model 4 : Random Forest ##

# 1) Modeling

model4 <- randomForest(RESPOND~., data=train)

# Model Fitting

model4_fitForest <- predict(model4, newdata = test, type="prob")[,2]

## 2) ROC 값 확인 ##

model4_pred <- prediction(model4_fitForest,test$RESPOND)
model4_RESPOND <- performance(model4_pred, "tpr", "fpr")

# ROC - Model performance plot
plot(model4_RESPOND, colorize=TRUE, lwd=2, main = "ROC model4: Random Forest", col = "blue")
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);
lines(x=c(1, 0), y=c(0, 1), col="green", lwd=1, lty=4)

# 3) AUC, KS, Gini 계수 #

model4_KS <- 
  round(max(attr(model4_RESPOND,'y.values')[[1]]-attr(model4_RESPOND,'x.values')[[1]])*100,2)

model4_AUROC <- 
  round(performance(model4_pred, measure = "auc")@y.values[[1]]*100, 2)

model4_Gini <- (2*model4_AUROC - 100)

cat("AUROC: ",model3_AUROC,"\tKS: ", model3_KS, "\tGini:", model3_Gini, "\n")


# 5. 각 모형의 성능비교

### 1) 모델 성능비교 ###

plot(model1_RESPOND, col='blue', lty=1, main='ROCs: Model Performance Comparision') # logistic regression
plot(model2_RESPOND, col='gold', lty=2, add=TRUE) # tree
plot(model3_RESPOND, col='orange', lty=3, add=TRUE) # neuralnet
plot(model4_RESPOND, col='green', lty=4, add=TRUE) # random forest
legend(0.6,0.5,
       c('m1:Logistic Regression','m2:Recursive Partitioning','m3:Neural Network','m4:Random Forest'),
       col=c('blue','gold','orange','green','dark gray','dark green','black','red','brown'),
       lwd=3);
lines(c(0,1),c(0,1),col="gray",lty=4) # random line


## 2) 성능 테이블 ##

models <- c('m1:Logistic Regression','m2:Recursive Partitioning','m3:Neural Network','m4:Random Forest')

# AUCs

models_AUC <- c(model1_AUROC, model2_AUROC, model3_AUROC, model4_AUROC)

# KS

models_KS <- c(model1_KS, model2_KS, model3_KS, model4_KS)

# Gini

models_Gini <- c(model1_Gini, model2_Gini, model3_Gini, model4_Gini)

# Combine AUC and KS

model_performance_metric <- as.data.frame(cbind(models, models_AUC, models_KS, models_Gini))

# Colnames

colnames(model_performance_metric) <- c("Model", "AUC", "KS", "Gini")

# 3) Display Performance Reports
kable(model_performance_metric, caption = "Comparision of Model Performances")
DT::datatable(model_performance_metric)
