library(creditmodel)
library(ggplot2)

data <- creditmodel::UCICreditCard
head(data)

# Cramer’s V matrix between categorical variables.

char_x_list = get_names(dat = UCICreditCard,
                        types = c('factor', 'character'),
                        ex_cols = "ID$|date$|default.payment.next.month$", get_ex = FALSE)
char_cor(dat = UCICreditCard[char_x_list])



#variables that are converted to numbers containing strings

dat_sub = lendingclub[c('dti_joint','emp_length')]
str(dat_sub)

dat_sub = char_to_num(dat_sub)
str(dat_sub)

# checking_data cheking dat before processing.

dat = checking_data(dat = UCICreditCard, target = "default.payment.next.month")
dat

# cohort_analysis & cohort_table cohort_analysis is for cohort(vintage) analysis

cohort_analysis(
  dat,
  obs_id = "ID",
  occur_time = NULL,
  MOB = NULL,
  group = NULL,
  due_day = NULL,
  period = "monthly",
  status = NULL,
  amount = NULL,
  by_out = "cnt",
  start_date = NULL,
  end_date = NULL,
  dead_status = 30,
  all_due = TRUE
)

cohort_table(
  dat,
  obs_id = "ID",
  occur_time = NULL,
  MOB = NULL,
  group = NULL,
  due_day = NULL,
  period = "monthly",
  status = NULL,
  amount = NULL,
  by_out = "cnt",
  start_date = NULL,
  end_date = NULL,
  dead_status = 30,
  all_due = TRUE
)

# cor_heat_plot is for ploting correlation matrix

# 데이터 분할

train_test = train_test_split(UCICreditCard,
                              split_type = "Random", prop = 0.8, save_data = FALSE)
dat_train = train_test$train
dat_test = train_test$test

# 히트맵

cor_mat = cor(dat_train[,8:12], use = "complete.obs")
cor_heat_plot(cor_mat)


# 변수별 cross_table 

cross_table(dat = UCICreditCard, cross_x = "SEX",cross_y = "AGE",
            target = "default.payment.next.month", cross_type = "bad_pct",value = "LIMIT_BAL")

cross_table(dat = UCICreditCard, cross_x = "SEX",cross_y = "AGE",
            target = "default.payment.next.month", cross_type = "total_pct",value = "LIMIT_BAL")

cross_table(dat = UCICreditCard, cross_x = "SEX",cross_y = "AGE",
            target = "default.payment.next.month", cross_type = "total_mean",value = "LIMIT_BAL")

cross_table(dat = UCICreditCard, cross_x = "SEX",cross_y = "AGE",
            target = "default.payment.next.month", cross_type = "total_median",value = "LIMIT_BAL")

cross_table(dat = UCICreditCard, cross_x = c("SEX", "MARRIAGE"), cross_y = "AGE",
            target = "default.payment.next.month", cross_type = "bad_pct",value = "LIMIT_BAL")


# segmentation

clust = customer_segmentation(dat = lendingclub[1:10000,20:30],
                              x_list = NULL, ex_cols = "id$|loan_status",
                              cluster_control = list(meth = "FCM", kc = 3), save_data = FALSE,
                              tree_control = list(minbucket = round(nrow(lendingclub) / 10)),
                              file_name = NULL, dir_path = tempdir())

head(clust)

head(lendingclub)

# Generating Initial Equal Size Sample Bins
# equal sample size breaks

equ_breaks = cut_equal(dat = UCICreditCard[, "PAY_AMT2"], g = 10)


# this function creates stratified folds for cross validation.

sub = cv_split(UCICreditCard, k = 30)[[1]]
dat = UCICreditCard[sub,]


# The data_cleansing function is a simpler wrapper for data cleaning functions, such as delete variables that values are all NAs; 

# checking dat and target format. delete low variance variables replace null or NULL or blank with NA; 

# encode variables which NAs & miss value rate is more than 95 encode variables which unique value rate is more than 95 merge categories of character variables that is more than 10; 

#transfer time variables to dateformation; remove duplicated observations; process outliers; process NAs.

#data cleaning

dat_cl = data_cleansing(dat = UCICreditCard[1:2000,],
                        target = "default.payment.next.month",
                        x_list = NULL,
                        obs_id = "ID",
                        occur_time = "apply_date",
                        ex_cols = c("PAY_6|BILL_"),
                        outlier_proc = TRUE,
                        missing_proc = TRUE,
                        low_var = TRUE,
                        save_data = FALSE)

str(dat)
str(dat_cl)


#’The data_exploration includes both univariate and bivariate analysis and ranges from univariate statistics and frequency distributions, to correlations, cross-tabulation and characteristic analysis.

data_exploration(
  dat,
  save_data = FALSE,
  file_name = NULL,
  dir_path = tempdir(),
  note = FALSE
)

# date_cut is a small function to get date point.
# pct the percent of cutting. Default: 0.7.

date_cut(dat_time = lendingclub$issue_d, pct = 0.8)

#"2018-08-01"


# digits_num is for caculating optimal digits number for numeric variables.

## Not run:

digits_num(lendingclub[,"dti"])

# 7
## End(Not run)


# entropy_weight is for calculating Entropy Weight.

entropy_weight(dat = ewm_data,
               pos_vars = c(6,8,9,10),
               neg_vars = c(7,11))



datss = entry_rate_na(dat = lendingclub[1:1000, ], nr = 0.98)

# eval_auc ,eval_ks ,eval_lift,eval_tnr is for getting best params of xgboost.

# Arguments

# preds A list of predict probability or score.
# dtrain Matrix of x predictors.

eval_auc(preds, dtrain)

eval_ks(preds, dtrain)

eval_tnr(preds, dtrain)

eval_lift(preds, dtrain)



# fast_high_cor_filter In a highly correlated variable group, select the variable with the highest IV. 

# high_cor_filter In a highly correlated variable group, select the variable with the highest IV.


# calculate iv for each variable.

# feature_selector This function uses four different methods (IV, PSI, correlation, xgboost) in order to select important features.The correlation algorithm must be used with IV.

# iv_cp The minimum threshold of IV. 0 < iv_i ; 0.01 to 0.1 usually work. Default: 0.02

# psi_cp The maximum threshold of PSI. 0 <= psi_i <=1; 0.05 to 0.2 usually work. Default: 0.1

# xgb_cp Threshold of XGB feature’s Gain. 0 <= xgb_cp <=1. Default is 1/number of independent variables.

# cor_cp Threshold of correlation between features. 0 <= cor_cp <=1; 0.7 to 0.98 usually work. Default is 0.98.

feature_selector(dat_train = UCICreditCard[1:1000,c(2,8:12,26)],
                 dat_test = NULL, target = "default.payment.next.month",
                 occur_time = "apply_date", filter = c("IV", "PSI"),
                 cv_folds = 1, iv_cp = 0.01, psi_cp = 0.1, xgb_cp = 0, cor_cp = 0.98,
                 vars_name = FALSE, note = TRUE)

iv_list = feature_selector(dat_train = UCICreditCard[1:1000,c(2,8:12,26)],
                           dat_test = NULL, target = "default.payment.next.month",
                           occur_time = "apply_date", filter = c("IV", "PSI"),
                           cv_folds = 1, iv_cp = 0.01, psi_cp = 0.1, xgb_cp = 0, cor_cp = 0.98,
                           vars_name = FALSE, note = TRUE)

high_cor_list = fast_high_cor_filter(dat = UCICreditCard[1:1000,],
                     com_list = iv_list, save_data = FALSE,
                     ex_cols = "ID$|date$|default.payment.next.month$",
                     p = 0.9, cor_class = FALSE ,var_name = FALSE)

#


# gbm_filter is for selecting important features using GBM.


# Arguments

# n.trees Integer specifying the total number of trees to fit. This is equivalent to the number of iterations and the number of basis functions in the additive expansion. Default is 100.

# interaction.depth

# Integer specifying the maximum depth of each tree(i.e., the highest level of variable interactions allowed). A value of 1 implies an additive model, a value of 2 implies a model with up to 2 - way interactions, etc. Default is 1.

# shrinkage a shrinkage parameter applied to each tree in the expansion. Also known as the learning rate or step - size reduction; 0.001 to 0.1 usually work, but a smaller learning rate typically requires more trees. Default is 0.1 .

GBM.params = gbm_params(n.trees = 100, interaction.depth = 2, shrinkage = 0.1,
                        bag.fraction = 1, train.fraction = 1,
                        n.minobsinnode = 30,
                        cv.folds = 2)
## Not run:
features = gbm_filter(dat = UCICreditCard[1:1000, c(8:12, 26)],
                      target = "default.payment.next.month",
                      occur_time = "apply_date",
                      GBM.params = GBM.params
                      , vars_name = FALSE)
## End(Not run)



# get_auc_ks_lambda get_auc_ks_lambda is for get best lambda required in lasso_filter. This function required in lasso_filter


get_auc_ks_lambda(
  lasso_model,
  x_test,
  y_test,
  save_data = FALSE,
  plot_show = TRUE,
  file_name = NULL,
  dir_path = tempdir()
)


# get_bins_table is used to generates summary information of varaibles. get_bins_table_all can generates bins table for all specified independent variables. (중요)


breaks_list = get_breaks_all(dat = UCICreditCard, x_list = names(UCICreditCard)[3:4],
                             target = "default.payment.next.month", equal_bins =TRUE,best = FALSE, g=5, ex_cols = "ID|apply_date", save_data = FALSE) 



get_bins_table <- get_bins_table_all(dat = UCICreditCard, breaks_list = breaks_list, target = "default.payment.next.month")

head(get_bins_table)

get_bins_table <- as.data.frame(get_bins_table)



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


# get_correlation_group is funtion for obtaining highly correlated variable groups. select_cor_group is funtion for selecting highly correlated variable group. select_cor_list is funtion for selecting highly correlated variable list.

## Not run:

cor_mat = cor(UCICreditCard[8:20],
              use = "complete.obs", method = "spearman")

get_correlation_group(cor_mat, p = 0.6 )

## End(Not run)


# get_ctree_rules This function is used to desision tree rules and percentage of 1 under each rule.


train_test = train_test_split(UCICreditCard, split_type = "Random", prop = 0.8, save_data = FALSE)
dat_train = train_test$train
dat_test = train_test$test
dat_train$default.payment.next.month = as.numeric(dat_train$default.payment.next.month)

get_ctree_rules(tree_fit = NULL, train_dat = dat_train[, 8:26],
                target ="default.payment.next.month", test_dat = dat_test)


# Calculate Information Value (IV) get_iv is used to calculate Information Value (IV) of an independent variable. 
# get_iv_all can loop through IV for all specified independent variables.

get_iv_all(dat = UCICreditCard,
           x_list = names(UCICreditCard)[3:10],
           equal_bins = TRUE, best = FALSE,
           target = "default.payment.next.month",
           ex_cols = "ID|apply_date")

get_iv(UCICreditCard, x = "PAY_3",
       equal_bins = TRUE, best = FALSE,
       target = "default.payment.next.month")

# get_logistic_coef is for geting logistic coefficients.

# dataset spliting

sub = cv_split(UCICreditCard, k = 30)[[1]]

dat = UCICreditCard[sub,]

#rename the target variable

dat = re_name(dat, "default.payment.next.month", "target")

dat = data_cleansing(dat, target = "target", obs_id = "ID",
                     occur_time = "apply_date", miss_values = list("", -1))

#train_ test spliting

train_test = train_test_split(dat, split_type = "OOT", prop = 0.7,
                              occur_time = "apply_date")

dat_train = train_test$train

dat_test = train_test$test

#get breaks of all predictive variables

x_list = c("PAY_0", "LIMIT_BAL", "PAY_AMT5", "EDUCATION", "PAY_3", "PAY_2")

breaks_list = get_breaks_all(dat = dat_train, target = "target",
                             x_list = x_list, occur_time = "apply_date", ex_cols = "ID",
                             save_data = FALSE, note = FALSE)

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



# get_names is for getting names of particular classes of variables

x_list = get_names(dat = UCICreditCard, types = c('factor', 'character'),
                   ex_cols = c("default.payment.next.month","ID$|_date$"), get_ex = FALSE)

x_list = get_names(dat = UCICreditCard, types = c('numeric', 'character', "integer"),
                   ex_cols = c("default.payment.next.month", "ID$|SEX "), get_ex = FALSE)


# Plot Independent Variables Distribution

train_test = train_test_split(UCICreditCard[1:1000,], split_type = "Random",
                              prop = 0.8, save_data = FALSE)
dat_train = train_test$train
dat_test = train_test$test
get_plots(dat_train[, c(8, 26)], dat_test = dat_test[, c(8, 26)],
          target = "default.payment.next.month")


# Calculate Population Stability Index (PSI) get_psi is used to calculate Population Stability Index (PSI) of an independent variable. get_psi_all can loop through PSI for all specified independent variables.


# dat_test is null

get_psi(dat = UCICreditCard, x = "PAY_3", occur_time = "apply_date")

# dat_test is not all
# train_test split

train_test = train_test_split(dat = UCICreditCard, prop = 0.7, split_type = "OOT",
                              occur_time = "apply_date", start_date = NULL, cut_date = NULL,
                              save_data = FALSE, note = FALSE)
dat_ex = train_test$train
dat_ac = train_test$test

# generate psi table

get_psi(dat = dat_ex, dat_test = dat_ac, x = "PAY_3",
        occur_time = "apply_date", bins_no = TRUE)


# get_iv_psi is used to calculate Information Value (IV) and Population Stability Index (PSI) of an independent variable. get_iv_psi_all can loop through IV & PSI for all specified independent variables.

iv_list = get_psi_iv_all(dat = UCICreditCard[1:1000, ],
                         x_list = names(UCICreditCard)[3:5], equal_bins = TRUE,
                         target = "default.payment.next.month", ex_cols = "ID|apply_date")

get_psi_iv(UCICreditCard, x = "PAY_3",
           target = "default.payment.next.month",bins_total = TRUE)


# You can use the psi_plot to plot PSI of your data. get_psi_plots can loop through plots for all specified independent variables.

train_test = train_test_split(UCICreditCard[1:1000,], split_type = "Random",
                              prop = 0.8, save_data = FALSE)
dat_train = train_test$train
dat_test = train_test$test

get_psi_plots(dat_train[, c(8, 9)], dat_test = dat_test[, c(8, 9)])

# get_score_card is for generating a stardard scorecard

get_score_card(
  lg_model,
  target,
  bins_table,
  a = 600,
  b = 50,
  file_name = NULL,
  dir_path = tempdir(),
  save_data = FALSE
)

#tree breaks
tree_control = list(p = 0.02, cp = 0.000001, xval = 5, maxdepth = 10)
tree_breaks = get_tree_breaks(dat = UCICreditCard, x = "MARRIAGE",
                              target = "default.payment.next.month", tree_control = tree_control)

# get_x_list is for getting intersect names of x_list, train and test.


x_list = get_x_list(x_list = NULL,dat_train = UCICreditCard,
                    ex_cols = c("default.payment.next.month","ID$|_date$"))


# high_cor_selector is function for comparing the two highly correlated variables, select a variable with the largest IV value.

high_cor_selector(
  cor_mat,
  p = 0.95,
  x_list = NULL,
  com_list = NULL,
  retain = TRUE
)

# ks_table is for generating a model performance table. ks_table_plot is for ploting the table generated by ks_table ks_psi_plot is for K-S & PSI distrbution ploting.


sub = cv_split(UCICreditCard, k = 30)[[1]]
dat = UCICreditCard[sub,]
dat = re_name(dat, "default.payment.next.month", "target")
dat = data_cleansing(dat, target = "target", obs_id = "ID",
                     occur_time = "apply_date", miss_values = list("", -1))
train_test = train_test_split(dat, split_type = "OOT", prop = 0.7,
                              occur_time = "apply_date")
dat_train = train_test$train
dat_test = train_test$test

train_woe = woe_trans_all(dat = dat_train, 
                          target = "target", 
                          breaks_list = breaks_list, 
                          woe_name = FALSE)

test_woe = woe_trans_all(dat = dat_test,
                         target = "target",
                         breaks_list = breaks_list,
                         note = FALSE)


x_list = c("PAY_0", "LIMIT_BAL", "PAY_AMT5", "PAY_3", "PAY_2")

Formula = as.formula(paste("target", paste(x_list, collapse = ' + '), sep = ' ~ '))

set.seed(46)

lr_model = glm(Formula, data = dat_train[, c("target", x_list)], family = binomial(logit))

dat_train$pred_LR = round(predict(lr_model, dat_train[, x_list], type = "response"), 5)

dat_test$pred_LR = round(predict(lr_model, dat_test[, x_list], type = "response"), 5)

dat_train$pred_LR = score_transfer(model = lr_model,
                                    tbl_woe = train_woe,
                                    save_data = TRUE)[, "score"]

dat_test$pred_LR = score_transfer(model = lr_model,
                                   tbl_woe = test_woe, save_data = FALSE)[, "score"]

# model evaluation

ks_psi_plot(train_pred = dat_train, test_pred = dat_test,
            score = "pred_LR", target = "target",
            plot_show = TRUE)

tb_pred = ks_table_plot(train_pred = dat_train, test_pred = dat_test,
                        score = "pred_LR", target = "target",
                        g = 10, g_width = 13, plot_show = FALSE)

key_index = model_key_index(tb_pred)

# ks_value is for get K-S value for a prob or score.

ks_value(dat_train$target, dat_train$pred_LR)
ks_value(dat_test$target, dat_test$pred_LR)


# lift_value is for getting max lift value for a prob or score.

lift_value(dat_train$target, dat_train$pred_LR)
lift_value(dat_test$target, dat_test$pred_LR)


# Logarithmic transformation

dat = log_trans(dat = UCICreditCard, target = "default.payment.next.month",
                x_list =NULL,cor_dif = 0.01,ex_cols = "ID", note = TRUE)

# Loop Function. #’ loop_function is an iterator to loop through

dat = UCICreditCard[24:26]
num_x_list = get_names(dat = dat, types = c('numeric', 'integer', 'double'),
                       ex_cols = NULL, get_ex = FALSE)
dat[ ,num_x_list] = loop_function(func = outliers_kmeans_lof, x_list = num_x_list,
                                  args = list(dat = dat),
                                  bind = "cbind", as_list = FALSE,
                                  parallel = FALSE)


# low_variance_filter is for removing variables with repeated values up to a certain percentage.

dat = low_variance_filter(lendingclub[1:1000, ], lvp = 0.9)


# Variance-Inflation Factors

sub = cv_split(UCICreditCard, k = 30)[[1]]

x_list = c("PAY_0", "LIMIT_BAL", "PAY_AMT5", "PAY_3", "PAY_2")

dat = re_name(UCICreditCard[sub,], "default.payment.next.month", "target")

dat = dat[,c("target",x_list)]

dat = data_cleansing(dat, miss_values = list("", -1))

train_test = train_test_split(dat, prop = 0.7)

dat_train = train_test$train

dat_test = train_test$test

Formula = as.formula(paste("target", paste(x_list, collapse = ' + '), sep = ' ~ '))

set.seed(46)

lr_model = glm(Formula, data = dat_train[, c("target", x_list)], family = binomial(logit))

lr_vif(lr_model)

get_logistic_coef(lr_model)

class(dat)

mod = lr_model

lr_vif(lr_model)


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

# model evaluation

perf_table <- perf_table(train_pred = dat_train, test_pred = dat_test, target = "target", score = "pred_LR")

head(perf_table)

ks_plot(train_pred = dat_train, test_pred = dat_test, target = "target", score = "pred_LR")

roc_plot(train_pred = dat_train, test_pred = dat_test, target = "target", score = "pred_LR")

lift_plot(train_pred = dat_train, test_pred = dat_test, target = "target", score = "pred_LR")

score_distribution_plot(train_pred = dat_train, test_pred = dat_test,
target = "target", score = "pred_LR")

model_result_plot(train_pred = dat_train, test_pred = dat_test,
target = "target", score = "pred_LR")

# Plot multiple ggplot-objects as a grid-arranged single plot.

library(ggplot2)
sub = cv_split(UCICreditCard, k = 30)[[1]]
dat = UCICreditCard[sub,]
dat = re_name(dat, "default.payment.next.month", "target")
dat = data_cleansing(dat, target = "target", obs_id = "ID",
                     occur_time = "apply_date", miss_values = list("", -1))
dat = process_nas(dat)
train_test = train_test_split(dat, split_type = "OOT", prop = 0.7,
                              occur_time = "apply_date")
dat_train = train_test$train
dat_test = train_test$test
x_list = c("PAY_0", "LIMIT_BAL", "PAY_AMT5", "PAY_3", "PAY_2")
Formula = as.formula(paste("target", paste(x_list, collapse = ' + '), sep = ' ~ '))
set.seed(46)
lr_model = glm(Formula, data = dat_train[, c("target", x_list)], family = binomial(logit))
dat_train$pred_LR = round(predict(lr_model, dat_train[, x_list], type = "response"), 5)
dat_test$pred_LR = round(predict(lr_model, dat_test[, x_list], type = "response"), 5)

# model evaluation

p1 = ks_plot(train_pred = dat_train, test_pred = dat_test, target = "target", score = "pred_LR")

p2 = roc_plot(train_pred = dat_train, test_pred = dat_test, target = "target", score = "pred_LR")

p3 = lift_plot(train_pred = dat_train, test_pred = dat_test, target = "target", score = "pred_LR")

p4 = score_distribution_plot(train_pred = dat_train, test_pred = dat_test,
                             target = "target", score = "pred_LR")

p_plots= multi_grid(p1,p2,p3,p4)

plot(p_plots)

# outliers_detection Outliers Detection outliers_detection is for outliers detecting using Kmeans and Local Outlier Factor (lof)

outliers_detection(dat, x_list, kc = 3, kn = 5)

# partial_dependence_plot is for generating a partial dependence plot. get_partial_dependence_plots is for ploting partial dependence of all vairables in x_list.

sub = cv_split(UCICreditCard, k = 30)[[1]]
dat = UCICreditCard[sub,]
dat = re_name(dat, "default.payment.next.month", "target")
dat = data_cleansing(dat, target = "target", obs_id = "ID",
                     occur_time = "apply_date", miss_values = list("", -1))
train_test = train_test_split(dat, split_type = "OOT", prop = 0.7,
                              occur_time = "apply_date")
dat_train = train_test$train
dat_test = train_test$test
x_list = c("PAY_0", "LIMIT_BAL", "PAY_AMT5", "PAY_3", "PAY_2")
Formula = as.formula(paste("target", paste(x_list, collapse = ' + '), sep = ' ~ '))
set.seed(46)
lr_model = glm(Formula, data = dat_train[, c("target", x_list)], family = binomial(logit))

#plot partial dependency of one variable

partial_dependence_plot(model = lr_model, x ="LIMIT_BAL", x_train = dat_train)

#plot partial dependency of all variables

pd_list = get_partial_dependence_plots(model = lr_model, x_list = x_list[1:2],
                                       x_train = dat_train, save_data = FALSE,plot_show = TRUE)


plot_bar(dat = lendingclub, x = "grade")
plot_bar(dat = lendingclub, x = "grade", y= "dti",
         g_x = 5,g_y = 3,
         position = 'dodge', dodge_width = 0.9,
         type = "each_pct_x",
         reverse = FALSE,
         cut_bin = 'equal_depth',
         fill_colors = c(love_color(type = "line"),
                         love_color(type = "line")))

plot_box(lendingclub, x = "grade", y = "installment", g = 7)

plot_density(dat = lendingclub, x = "annual_inc",y = "emp_length", m =0, hist = -1)
plot_density(dat = lendingclub, x = "annual_inc", m = 2,
             colors_y = love_color(type = "line")[c(1,3)])


plot_distribution_x(dat = lendingclub, x = "max_bal_bc", g = 10,
                    cut_bin = 'equal_width')
plot_distribution(dat = lendingclub, x_list = c("max_bal_bc", "installment"),
                  g = 10,dir_path = tempdir(),
                  cut_bin = 'equal_width')


plot_line(
  dat,
  x_list,
  y = NULL,
  x_breaks = NULL,
  y_breaks = NULL,
  cut_bin = "equal_width",
  g_x = 10,
  g_y = 5,
  target = NULL,
  value = NULL,
  type = "total_pct",
  reverse = FALSE,
pl_theme = plot_theme(legend.position = "right", title_size = 9, legend_size = 7,
                      axis_title_size = 8),
fill_colors = c(love_color(type = "deep"), love_color(type = "light"),
                love_color(type = "shallow"))
)

# plot_oot_perf plot_oot_perf is for ploting performance of cross time samples in the future

sub = cv_split(UCICreditCard, k = 30)[[1]]
dat = UCICreditCard[sub,]
dat = re_name(dat, "default.payment.next.month", "target")
x_list = c("PAY_0", "LIMIT_BAL", "PAY_AMT5", "PAY_3", "PAY_2")
dat = data_cleansing(dat, target = "target", obs_id = "ID",x_list = x_list,
                     occur_time = "apply_date", miss_values = list("", -1))
dat = process_nas(dat)
train_test = train_test_split(dat, split_type = "OOT", prop = 0.7,
                              occur_time = "apply_date")
dat_train = train_test$train
dat_test = train_test$test
Formula = as.formula(paste("target", paste(x_list, collapse = ' + '), sep = ' ~ '))
set.seed(46)
lr_model = glm(Formula, data = dat_train[, c("target", x_list)], family = binomial(logit))
dat_train$pred_LR = round(predict(lr_model, dat_train[, x_list], type = "response"), 5)
dat_test$pred_LR = round(predict(lr_model, dat_test[, x_list], type = "response"), 5)

plot_oot_perf(dat_test = dat_test, occur_time = "apply_date", target = "target", x = "pred_LR")

plot_relative_freq_histogram(dat = lendingclub, x = "grade", y = "dti", g_x = 7,g_y = 3,
                             cut_bin = 'equal_width')


iv_list = get_psi_iv_all(dat = UCICreditCard[1:1000, ],
                         x_list = names(UCICreditCard)[3:5], equal_bins = TRUE,
                         target = "default.payment.next.month", ex_cols = "ID|apply_date")

iv_dt =get_psi_iv(UCICreditCard, x = "PAY_3",
                  target = "default.payment.next.month", bins_total = TRUE)

plot_table(iv_dt)

psi_iv_filter(dat= UCICreditCard[1:1000,c(2,4,8:9,26)],
              target = "default.payment.next.month",
              occur_time = "apply_date",
              parallel = FALSE)

rank_dict = ranking_percent_dict(dat = UCICreditCard[1:1000,],
                                 x_list = c("LIMIT_BAL","BILL_AMT2","PAY_AMT3"), ex_cols = NULL )
UCICreditCard_new = ranking_percent_proc(dat = UCICreditCard[1:1000,],
                                         x_list = c("LIMIT_BAL", "BILL_AMT2", "PAY_AMT3"), rank_dict = rank_dict, parallel = FALSE)


# equal sample size breaks

equ_breaks = cut_equal(dat = UCICreditCard[, "PAY_AMT2"], g = 10)

# select best bins

bins_control = list(bins_num = 10, bins_pct = 0.02, b_chi = 0.02,
                    b_odds = 0.1, b_psi = 0.05, b_or = 0.15, mono = 0.3, odds_psi = 0.1, kc = 1)

select_best_breaks(dat = UCICreditCard, x = "PAY_AMT2", breaks = equ_breaks,
                   target = "default.payment.next.month", occur_time = "apply_date",
                   sp_values = NULL, bins_control = bins_control)






