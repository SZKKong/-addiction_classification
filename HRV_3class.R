################################################################################
#                               load data
################################################################################

library(readxl)
data1 <- read_excel(file.choose(),col_names=TRUE) 


# data2 <- read_excel(file.choose(),col_names=TRUE,skip=1) 
# head(data2) ;dim(data2)


################################################################################
#                          pre_processing_No.1
#
#                       replace  NA as mean of mode
################################################################################

######## Demographic data : n/a , Missing data : 해당사항 없음.

# log_LF_frequency, log_HF_frequency, log_LF_HF_ratio_frequency, log_Power_VLF제외_frequency, log_Power_VLF포함_frequency
# log-transformation (SDNN_time, NN50_time, pNN50_time, RMSSD_time, mean_HR_time, sd_HR_time)


data1 <- data1[,-c(5,6,7,8,9,15,16,17)]
data1[,10:15] <- log(data1[,10:15])
names(data1)[10:15] <- c("log_SDNN_time","log_NN50_time", "log_pNN50_time","log_RMSSD_time","log_mean_HR_time","log_sd_HR_time")


summary(data1)


################################################################################
#                  3-class classification (normal vs AUD vs IGD)

#                   classification of 3 classes by HRV data
################################################################################

data_3= data1[,-1] # ID 삭제 

data_3[data_3$group == "HC","group"] = 0
data_3[data_3$group == "AUD","group"] = 1
data_3[data_3$group == "IGD","group"] = 2
data_3$group = as.factor(data_3$group)

set.seed(2022)
split1 =  sample(c(rep(0, 0.7 * nrow(data_3)+1), rep(1, 0.3 * nrow(data_3))))
train1 <- data_3[split1 == 0, ]  
test1 <- data_3[split1== 1, ]  

dim(train1) ; dim(test1)


################################################################################
#                       1.  Logistic Regression : 0.413  
################################################################################


library(caret)
library(nnet)

mr_fit = multinom(factor(group)~.,data = train1)
pred_mr = predict(mr_fit,test1)
confusionMatrix(table(test1$group, pred_mr))
# pred_mr
# 0 1 2
# 0 7 7 3
# 1 3 6 6
# 2 3 5 6




################################################################################
#                model1. decision tree : 0.6087
################################################################################

library(caret)
library(rpart)
set.seed(2022)

fitControl <- trainControl(method="adaptive_cv", repeats=3)
metric <- "Accuracy"

rpart_fit = train(factor(group) ~.,data = train1, method = "rpart", 
                  trControl = fitControl, tuneLength = 10, metric=metric)

pred_dt <- predict(rpart_fit, newdata = test1)
confusionMatrix(table(test1$group, pred_dt))
# pred_dt
# 0  1  2
# 0 14  1  2
# 1  6  7  2
# 2  6  1  7


################################################################################
#                  model2. random forest :  0.4565 
################################################################################

library(caret)
library(randomForest)
set.seed(2022)

fitControl <- trainControl(method="adaptive_cv", repeats=3)
metric <- "Accuracy"

# rfgrid <- expand.grid(.mtry=c(1:15))

rf_fit <- train(factor(group) ~., data=train1, method="rf",tuneLength = 10,
                trControl=fitControl,  metric=metric)

pred_rf <- predict(rf_fit, newdata = test1)
confusionMatrix(table(test1$group, pred_rf))

#pred_rf
#0  1  2
#0 10  2  5
#1  5  5  5
#2  2  6  6

################################################################################
#                model3_1. svm_linear : 0.4565 
#                            Warning??
################################################################################

library(e1071)
library(tidyverse)
set.seed(2022)
fitControl = trainControl(method = "adaptive_cv", repeats = 3)
metric = "Accuracy"

# linear classifier
svm_linerGrid = expand.grid(C = seq(0.5, 2, length = 20))
svm_linear <- train(factor(group) ~ ., data = train1, method = "svmLinear",
                    na.action = na.omit,
                    trControl = fitControl,tuneGrid = svm_linerGrid , metric = metric)

pred_svmliner <- predict(svm_linear, newdata = test1)
confusionMatrix(table(test1$group, pred_svmliner))

# pred_svmliner
#   0  1  2
#0 10  4  3
#1  3  5  7
#2  6  2  6

################################################################################
#                   model3_2. svm_kernel : 0.3696   
#                           Warning??
################################################################################

library(e1071)
library(tidyverse)
set.seed(2022)
fitControl = trainControl(method = "adaptive_cv", repeats = 10)
metric = "Accuracy"

# linear classifier
svm_kernel <- train(factor(group) ~ ., data = train1, method = "svmRadial",
                     tuneLength = 10,
                    trControl = fitControl,  metric = metric)

pred_svmkernel <- predict(svm_kernel, newdata = test1)
confusionMatrix(table(test1$group, pred_svmkernel))


#pred_svmkernel
#   0  1  2
#0 17  0  0
#1 15  0  0
#2 14  0  0

################################################################################
#                     model4. gbm : 0.3696    
################################################################################

library(caret)

set.seed(2022)
fitControl = trainControl(method = "adaptive_cv", repeats = 10)
metric = "Accuracy"

#gbmGrid = expand.grid(interaction.depth = c(1, 3, 5, 7, 9), n.trees = (1:30)*50, shrinkage = c(0.05, 0.1), n.minobsinnode = 20)
gbm_fit = train(factor(group) ~ ., data = train1, method = "gbm", trControl = fitControl,tuneLength = 10,  metric = metric)

pred_gb <- predict(gbm_fit, newdata = test1)
confusionMatrix(table(test1$group, pred_gb))


#   pred_gb
#  0 1 2
#0 6 6 5
#1 7 5 3
#2 2 6 6


#######################################################
#                model6. xgb : 0.5 
#######################################################

set.seed(2022)
library(Matrix)
library(xgboost)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)


train.data  <- as.matrix(train1[,-1])
test.data   <- as.matrix(test1[,-1])
train.label <- train1$group
test.label  <- test1$group

set.seed(2022)
params <- list(booster = "gbtree", objective = "multi:softmax", num_class = 3, eval_metric = "mlogloss")
xgb <- xgboost(params = params,data = train.data, label = train.label, nrounds = 2 )
test_pred = predict(xgb, newdata=test.data, type="response")
confusionMatrix(table(test1$group, test_pred))

#test_pred
#  0 1 2
#0 8 4 5
#1 7 7 1
#2 2 4 8

################################################################################
#                model7. lgbm  : 0.5  
################################################################################

set.seed(2022)
library(Matrix)
library(lightgbm)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)


set.seed(2022)
train_matrix = lgb.Dataset(data = train.data , label = train.label)

train.label

# model parameters
params = list(max_bin = 5, learning_rate = 0.001, objective = "multiclass", num_class = 3, eval_metric = "mlogloss")
lgbm <- lightgbm(params = params, train_matrix, valid, nrounds = 1000)
test_pred = predict(lgbm, test.data,reshape = TRUE)
pred <- data.frame(test_pred) %>% mutate(max_prob = max.col(., ties.method = "last"), label = test.label )
confusionMatrix(factor(pred$max_prob-1), factor(pred$label))


#      Reference
#Prediction   0  1  2
#          0 13  5  8
#          1  1  5  1
#          2  3  5  5



