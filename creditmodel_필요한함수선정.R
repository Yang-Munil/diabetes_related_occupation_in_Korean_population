library(creditmodel)
library(ggplot2)




# equal sample size breaks

equ_breaks = cut_equal(dat = UCICreditCard[, "PAY_AMT2"], g = 10)

# select best bins

bins_control = list(bins_num = 10, bins_pct = 0.02, b_chi = 0.02,
                    b_odds = 0.1, b_psi = 0.05, b_or = 0.15, mono = 0.3, odds_psi = 0.1, kc = 1)

select_best_breaks(dat = UCICreditCard, x = "PAY_AMT2", breaks = equ_breaks,
                   target = "default.payment.next.month", occur_time = "apply_date",
                   sp_values = NULL, bins_control = bins_control)



# fast_high_cor_filter In a highly correlated variable group, select the variable with the highest IV. 

# high_cor_filter In a highly correlated variable group, select the variable with the highest IV.


# calculate iv for each variable.

iv_list = feature_selector(dat_train = UCICreditCard[1:1000,], dat_test = NULL,
                           target = "default.payment.next.month",
                           occur_time = "apply_date",
                           filter = c("IV"), cv_folds = 1, iv_cp = 0.01,
                           ex_cols = "ID$|date$|default.payment.next.month$",
                           save_data = FALSE, vars_name = FALSE)

high_cor_list = fast_high_cor_filter(dat = UCICreditCard[1:1000,],
                                     com_list = iv_list, save_data = FALSE,
                                     ex_cols = "ID$|date$|default.payment.next.month$",
                                     p = 0.9, cor_class = FALSE ,var_name = FALSE)



# get_breaks is for generating optimal binning for numerical and nominal variables. The get_breaks_all is a simpler wrapper for get_breaks.


#controls

"## tree_control the list of tree parameters.

• p the minimum percent of observations in any terminal <leaf> node. 0 < p<
  1; 0.01 to 0.1 usually work.
  
• cp complexity parameter. the larger, the more conservative the algorithm
will be. 0 < cp< 1 ; 0.0001 to 0.0000001 usually work.

• xval number of cross-validations.Default: 5

• max_depth maximum depth of a tree. Default: 10

## bins_control the list of parameters.

• bins_num The maximum number of bins. 5 to 10 usually work. Default:
  10
  
• bins_pct The minimum percent of observations in any bins. 0 < bins_pct
< 1 , 0.01 to 0.1 usually work. Default: 0.02

• b_chi The minimum threshold of chi-square merge. 0 < b_chi< 1; 0.01 to
0.1 usually work. Default: 0.02

• b_odds The minimum threshold of odds merge. 0 < b_odds < 1; 0.05 to 0.2
usually work. Default: 0.1

• b_psi The maximum threshold of PSI in any bins. 0 < b_psi < 1 ; 0 to 0.1
usually work. Default: 0.05

• b_or The maximum threshold of G/B index in any bins. 0 < b_or < 1 ; 0.05
to 0.3 usually work. Default: 0.15"

tree_control = list(p = 0.02, cp = 0.000001, xval = 5, maxdepth = 10)

bins_control = list(bins_num = 10, bins_pct = 0.02, b_chi = 0.02, b_odds = 0.1,
                    b_psi = 0.05, b_or = 15, mono = 0.2, odds_psi = 0.1, kc = 5)

# get categrory variable breaks

b = get_breaks(dat = UCICreditCard[1:1000,], x = "MARRIAGE",
               target = "default.payment.next.month",
               occur_time = "apply_date",
               sp_values = list(-1, "missing"),
               tree_control = tree_control, bins_control = bins_control)


# get numeric variable breaks

b2 = get_breaks(dat = UCICreditCard[1:1000,], x = "PAY_2",
                target = "default.payment.next.month",
                occur_time = "apply_date",
                sp_values = list(-1, "missing"),
                tree_control = tree_control, bins_control = bins_control)


# get breaks of all predictive variables

b3 = get_breaks_all(dat = UCICreditCard[1:10000,], target = "default.payment.next.month",
                    x_list = c("MARRIAGE","PAY_2"),
                    occur_time = "apply_date", ex_cols = "ID",
                    sp_values = list(-1, "missing"),
                    tree_control = tree_control, bins_control = bins_control,
                    save_data = FALSE)
b3




# get_bins_table is used to generates summary information of varaibles. get_bins_table_all can generates bins table for all specified independent variables. (중요)


breaks_list = get_breaks_all(dat = UCICreditCard, x_list = names(UCICreditCard)[3:4],
                             target = "default.payment.next.month", equal_bins =TRUE,best = FALSE, g=5, ex_cols = "ID|apply_date", save_data = FALSE) 



get_bins_table <- get_bins_table_all(dat = UCICreditCard, breaks_list = breaks_list, target = "default.payment.next.month")

head(get_bins_table)

get_bins_table <- as.data.frame(get_bins_table)


################# 본격적인 스코어카드 생성 ################

#woe transforming

train_woe = woe_trans_all(dat = dat_train, 
                          target = "target", 
                          breaks_list = breaks_list, 
                          woe_name = FALSE)

test_woe = woe_trans_all(dat = dat_test,
                         target = "target",
                         breaks_list = breaks_list,
                         note = FALSE)

Formula = as.formula(paste("target", paste(x_list, collapse = ' + '), sep = ' ~ '))

set.seed(46)

lr_model = glm(Formula, data = train_woe[, c("target", x_list)], family = binomial(logit))

#get LR coefficient

dt_imp_LR = get_logistic_coef(lg_model = lr_model, save_data = FALSE)

bins_table = get_bins_table_all(dat = dat_train, target = "target",
                                x_list = x_list,dat_test = dat_test,
                                breaks_list = breaks_list, note = FALSE)

#score card

LR_score_card = get_score_card(lg_model = lr_model, bins_table, target = "target")

#scoring

train_pred = dat_train[, c("ID", "apply_date", "target")]

test_pred = dat_test[, c("ID", "apply_date", "target")]

train_pred$pred_LR = score_transfer(model = lr_model,
                                    tbl_woe = train_woe,
                                    save_data = TRUE)[, "score"]

test_pred$pred_LR = score_transfer(model = lr_model,
                                   tbl_woe = test_woe, save_data = FALSE)[, "score"]


train_pred$target <- as.factor(train_pred$target)

ggplot(train_pred, aes(x=pred_LR, fill = target, color = target)) +
  geom_histogram(aes(y=..density..), alpha=0.5, position = "dodge", binwidth = 20) +
  theme(legend.position = "top") +
  theme_minimal() +
  theme(legend.position = "top") +
  ggtitle("스코어 분포") +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black")) +
  labs(x="스코어", y="인원수") +
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black"))

test_pred$target <- as.factor(test_pred$target)

ggplot(test_pred, aes(x=pred_LR, fill = target, color = target)) +
  geom_histogram(aes(y=..density..), alpha=0.5, position = "dodge", binwidth = 20) +
  theme(legend.position = "top") +
  theme_minimal() +
  theme(legend.position = "top") +
  ggtitle("스코어 분포") +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black")) +
  labs(x="스코어", y="인원수") +
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black"))









############## 모델 매월 평가할 때 쓰일 패키지 함수 ###############


# model result plots model_result_plot is a wrapper of following: 

# 1. perf_table is for generating a model performance table. 
# 2. ks_plot is for K-S. 
# 3. roc_plot is for ROC. 
# 4. lift_plot is for Lift Chart. 
# 5. score_distribution_plot is for ploting the score distribution.


sub = cv_split(UCICreditCard, k = 30)[[1]]
dat = UCICreditCard[sub,]
dat = re_name(dat, "default.payment.next.month", "target")
x_list = c("PAY_0", "LIMIT_BAL", "PAY_AMT5", "PAY_3", "PAY_2")
dat = data_cleansing(dat, target = "target", obs_id = "ID",x_list = x_list,
                     occur_time = "apply_date", miss_values = list("", -1))
dat = process_nas(dat,default_miss = TRUE)
train_test = train_test_split(dat, split_type = "OOT", prop = 0.7,
                              occur_time = "apply_date")
dat_train = train_test$train
dat_test = train_test$test
Formula = as.formula(paste("target", paste(x_list, collapse = ' + '), sep = ' ~ '))
set.seed(46)

lr_model = glm(Formula, data = dat_train[, c("target", x_list)], family = binomial(logit))
dat_train$pred_LR = round(predict(lr_model, dat_train[, x_list], type = "response"), 5)
dat_test$pred_LR = round(predict(lr_model, dat_test[, x_list], type = "response"), 5)

dat_train$Score <- p_to_score(p = dat_train$pred_LR, PDO = 20, base = 1000, ratio = 1)
dat_test$Score <- p_to_score(p = dat_test$pred_LR, PDO = 20, base = 1000, ratio = 1)

# model evaluation

perf_table <- perf_table(train_pred = dat_train, test_pred = dat_test, target = "target", score = "pred_LR")

head(perf_table)

ks_plot(train_pred = dat_train, test_pred = dat_test, target = "target", score = "Score")

roc_plot(train_pred = dat_train, test_pred = dat_test, target = "target", score = "Score")

lift_plot(train_pred = dat_train, test_pred = dat_test, target = "target", score = "Score")

score_distribution_plot(train_pred = dat_train, test_pred = dat_test,
                        target = "target", score = "Score")

model_result_plot(train_pred = dat_train, test_pred = dat_test,
                  target = "target", score = "Score")

library(InformationValue)

data('ActualsAndScores')
optimalCutoff(actuals=ActualsAndScores$Actuals,
              predictedScores=ActualsAndScores$PredictedScores, optimiseFor="Both", returnDiagnostics=TRUE)
ks
data('ActualsAndScores')
plotROC(actuals=ActualsAndScores$Actuals, predictedScores=ActualsAndScores$PredictedScores)


somersD(actuals=ActualsAndScores$Actuals, predictedScores=ActualsAndScores$PredictedScores)
