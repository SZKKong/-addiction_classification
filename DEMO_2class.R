################################################################################
#                               load data
################################################################################

library(readxl)
data1 <- read_excel(file.choose(),col_names=TRUE) 
head(data1) ;dim(data1) ; summary(data1)

# data2 <- read_excel(file.choose(),col_names=TRUE,skip=1) 
# head(data2) ;dim(data2)


################################################################################
#                          pre_processing_No.1
#
#                       replace  NA as mean of mode
################################################################################

######## Demographic data : n/a , Missing data : 해당사항 없음.

library(lubridate)
library(DescTools)
colnames(data1)
data1 = data1[-c(1,2,4)] # remove useless col

summary(data1)
data1$Sex = as.factor(data1$Sex)

data1$`5.marriage`[data1$`5.marriage`== '999' | data1$`5.marriage`== 'MISS'] = NA
data1$`5.marriage` = as.factor(data1$`5.marriage`)
data1$`5.marriage`[is.na(data1$`5.marriage`)] = 1


data1$`8-1.drinking_F`[data1$`8-1.drinking_F`== 'MISS' | data1$`8-1.drinking_F`== 'n/a'] = 0
data1$`8-1.drinking_F`[data1$`8-1.drinking_F`== '999'] = NA

data1$`8.drinking`[data1$`8.drinking`== 4] = 1 
data1$`8.drinking`[data1$`8.drinking`== 999] = 1 

print(data1[is.na(data1$`8-1.drinking_F`),],n=30)  ##### fill 0 / 4

data1$`8-1.drinking_F`[data1$`8.drinking`== 1 & is.na(data1$`8-1.drinking_F`)] = 4
data1$`8-1.drinking_F`[is.na(data1$`8-1.drinking_F`)] = 0

data1$`8-1.drinking_F` = as.numeric(data1$`8-1.drinking_F`)
data1$`8.drinking` = as.factor(data1$`8.drinking`)

data1$`9.smoking여부` = as.factor(data1$`9.smoking여부`)

data1$`9-1. smoking_하루흡연량(개비)`[ data1$`9-1. smoking_하루흡연량(개비)`== 'MISS'] = NA 
data1$`9-1. smoking_하루흡연량(개비)` = as.numeric(data1$`9-1. smoking_하루흡연량(개비)`)
data1$`9-1. smoking_하루흡연량(개비)`[is.na(data1$`9-1. smoking_하루흡연량(개비)`)] = 0

# Week_game

data1$`10-1. 주중게임시간` = as.numeric(data1$`10-1. 주중게임시간`,na.rm=TRUE)
data1$`10-1. 주중게임시간`[data1$`10-1. 주중게임시간`== '999'] = 0
data1$`10-1. 주중게임시간`[is.na(data1$`10-1. 주중게임시간`)] = 0


data1$`10-2. 주말게임시간` = as.numeric(data1$`10-2. 주말게임시간`,na.rm=TRUE)
data1$`10-2. 주말게임시간`[is.na(data1$`10-2. 주말게임시간`)] = 0
data1$`10-2. 주말게임시간`[data1$`10-2. 주말게임시간`> 24] = NA
data1$`10-2. 주말게임시간`[is.na(data1$`10-2. 주말게임시간`)] = median(data1$`10-2. 주말게임시간`,na.rm=TRUE)

# Week_internet

data1$`11-1. 주중인터넷사용시간`[data1$`11-1. 주중인터넷사용시간`== '999'] = NA
data1$`11-1. 주중인터넷사용시간`[is.na(data1$`11-1. 주중인터넷사용시간`)] = 0

data1$`11-2. 주말인터넷사용시간` = as.numeric(data1$`11-2. 주말인터넷사용시간`,na.rm=TRUE)
data1$`11-2. 주말인터넷사용시간`[data1$`11-2. 주말인터넷사용시간`== '999'] = NA
data1$`11-2. 주말인터넷사용시간`[is.na(data1$`11-2. 주말인터넷사용시간`)] = 0

# Week_smartphone

data1$`12-1. 주중스마트폰사용시간` = as.numeric(data1$`12-1. 주중스마트폰사용시간`,na.rm=TRUE)
data1$`12-1. 주중스마트폰사용시간`[data1$`12-1. 주중스마트폰사용시간`== '999'] = NA
data1$`12-1. 주중스마트폰사용시간`[is.na(data1$`12-1. 주중스마트폰사용시간`)] = 0

data1$`12-2. 주말스마트폰사용시간` = as.numeric(data1$`12-2. 주말스마트폰사용시간`,na.rm=TRUE)
data1$`12-2. 주말스마트폰사용시간`[data1$`12-2. 주말스마트폰사용시간`== '999'] = NA
data1$`12-2. 주말스마트폰사용시간`[is.na(data1$`12-2. 주말스마트폰사용시간`)] = 0
data1$`12-2. 주말스마트폰사용시간`[data1$`12-2. 주말스마트폰사용시간`> 24] = NA
data1$`12-2. 주말스마트폰사용시간`[is.na(data1$`12-2. 주말스마트폰사용시간`)] = median(data1$`12-2. 주말스마트폰사용시간`,na.rm=TRUE)

data1$Fagerstrom[(data1$Fagerstrom== '999')|(data1$Fagerstrom== 'n/a')] = 0
data1$Fagerstrom = as.numeric(data1$Fagerstrom)

data1$`AUDIT-K`[(data1$`AUDIT-K`== '999')|(data1$`AUDIT-K`== 'n/a')] = NA
data1$`AUDIT-K` = as.numeric(data1$`AUDIT-K`)
data1$`AUDIT-K`[is.na(data1$`AUDIT-K`)] = median(data1$`AUDIT-K`,na.rm=TRUE)

data1$BDI[data1$BDI == "MISS"] = NA
data1$BDI = as.numeric(data1$BDI)
data1$BDI[is.na(data1$BDI)] = median(data1$BDI,na.rm=TRUE)

data1$SSP_length[data1$SSP_length == "n/a"] = NA
data1$SSP_length = as.numeric(data1$SSP_length)

data1$SSP_totalerror[data1$SSP_totalerror == "n/a"] = NA
data1$SSP_totalerror = as.numeric(data1$SSP_totalerror)

# fill NA with median(x)

data1 = data.frame(data1)
for(i in 58:ncol(data1)) {
  data1[ , i][is.na(data1[ , i])] = median(data1[ , i], na.rm=TRUE)
}

summary(data1)


################################################################################
#                  binomial classification (normal vs disorders)
################################################################################

data_binom = data1
data_binom[data_binom$CC == "AUD" | data_binom$CC == "IGD","CC"] = "DIS"
data_binom[data_binom$CC == "HC","CC"] = 0 
data_binom[data_binom$CC == "DIS","CC"] = 1
data_binom$CC = as.numeric(data_binom$CC)

set.seed(2022)
split1 =  sample(c(rep(0, 0.7 * nrow(data_binom)), rep(1, 0.3 * nrow(data_binom))))
train1 <- data_binom[split1 == 0, ]  
test1 <- data_binom[split1== 1, ]  

dim(train1) ; dim(test1)


################################################################################
#                       1.  Logistic Regression
################################################################################


library(caret)

lr_fit = glm(as.factor(CC)~.,data = train1,family = 'binomial', maxit = 100)
prob_lr = predict(lr_fit,test1,type="response")
pred_lr = rep(0, dim(test1)[1])
pred_lr[prob_lr > .5] = 1
confusionMatrix(table(test1$CC, pred_lr))


################################################################################
#                model1. decision tree : 0.7458
################################################################################

library(caret)
library(rpart)
set.seed(2022)

fitControl <- trainControl(method="adaptive_cv", repeats=3)
metric <- "Accuracy"

rpart_fit = train(factor(CC) ~.,data = train1, method = "rpart", 
                  trControl = fitControl, tuneLength = 10, metric=metric)

pred_dt <- predict(rpart_fit, newdata = test1)
confusionMatrix(table(test1$CC, pred_dt))


################################################################################
#                  model2. random forest : 0.8644
################################################################################

library(caret)
library(randomForest)
set.seed(2022)

fitControl <- trainControl(method="adaptive_cv", repeats=3)
metric <- "Accuracy"

rfgrid <- expand.grid(.mtry=c(1:15))

rf_fit <- train(factor(CC) ~., data=train1, method="rf",
                trControl=fitControl, tuneGrid=rfgrid, metric=metric)

pred_rf <- predict(rf_fit, newdata = test1)
  confusionMatrix(table(test1$CC, pred_rf))


################################################################################
#                model3_1. svm_linear : 0.8305
#                            Warning??
################################################################################
  
library(e1071)
library(tidyverse)
set.seed(2022)
fitControl = trainControl(method = "adaptive_cv", repeats = 10)
metric = "Accuracy"
  
# linear classifier
  
svm_linerGrid = expand.grid(C = seq(0.5, 2, length = 20))
svm_linear <- train(factor(CC) ~ ., data = train1, method = "svmLinear",
                      preProcess = c("center","scale"),
                      trControl = fitControl,tuneGrid = svm_linerGrid , metric = metric)
  
pred_svmliner <- predict(svm_linear, newdata = test1)
confusionMatrix(table(test1$CC, pred_svmliner))
  

################################################################################
#                   model3_2. svm_kernel :  0.8983
#                           Warning??
################################################################################

library(e1071)
library(tidyverse)
set.seed(2022)
fitControl = trainControl(method = "adaptive_cv", repeats = 10)
metric = "Accuracy"

# linear classifier

svm_kernel <- train(factor(CC) ~ ., data = train1, method = "svmRadial",
                    preProcess = c("center","scale"), tuneLength = 10,
                    trControl = fitControl,  metric = metric)

pred_svmliner <- predict(svm_kernel, newdata = test1)
confusionMatrix(table(test1$CC, pred_svmliner))




################################################################################
#                     model4. gbm : 0.8814
################################################################################

library(caret)

set.seed(2022)
fitControl = trainControl(method = "adaptive_cv", repeats = 10)
metric = "Accuracy"

gbmGrid = expand.grid(interaction.depth = c(1, 3, 5, 7, 9), n.trees = (1:30)*50, shrinkage = c(0.05, 0.1), n.minobsinnode = 20)

gbm_fit = train(factor(CC) ~ ., data = train1, method = "gbm", trControl = fitControl, tuneGrid = gbmGrid, metric = metric)

pred_gb <- predict(gbm_fit, newdata = test1)
confusionMatrix(table(test1$CC, pred_gb))



################################################################################
#                model7. lgbm  : 0.864
################################################################################

set.seed(2022)
library(Matrix)
library(lightgbm)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)


train.data  <- as.matrix(train1[,-1])
test.data   <- as.matrix(test1[,-1])

train.label <- train1$CC
test.label  <- test1$CC

set.seed(2022)
train_matrix = lgb.Dataset(data = train.data , label = train.label)

# model parameters
params = list(max_bin = 5,
              learning_rate = 0.001,
              objective = "binary",
              metric = 'binary_logloss')

#model training
lgbm <- lightgbm(params = params, train_matrix, valid, nrounds = 1000)

test_prob = predict(lgbm, test.data)
test_pred = rep(0, dim(test1)[1])
test_pred[test_prob > .5] = 1

table(test_pred,test1$CC)
mean(test_pred == test1$CC)





################################################################################
################################################################################
################################################################################

################################################################################
#                  binomial classification (AUD vs IGD)
################################################################################

data_diso = data1[data1$CC == "AUD" | data1$CC == "IGD",] 

data_diso[data_diso$CC == "IGD","CC"] = 0
data_diso[data_diso$CC == "AUD","CC"] = 1 
data_diso$CC = as.numeric(data_diso$CC)

set.seed(2022)
split1 =  sample(c(rep(0, 0.7 * nrow(data_diso)), rep(1, 0.3 * nrow(data_diso))))
train1 <- data_diso[split1 == 0, ]  
test1 <- data_diso[split1== 1, ]  

dim(train1) ; dim(test1)


################################################################################
#                       1.  Logistic Regression
################################################################################


library(caret)

lr_fit = glm(as.factor(CC)~.,data = train1,family = 'binomial', maxit = 100)
prob_lr = predict(lr_fit,test1,type="response")
pred_lr = rep(0, dim(test1)[1])
pred_lr[prob_lr > .5] = 1
confusionMatrix(table(test1$CC, pred_lr))


################################################################################
#                model1. decision tree : 0.8919
################################################################################

library(caret)
library(rpart)
set.seed(2022)

fitControl <- trainControl(method="adaptive_cv", repeats=3)
metric <- "Accuracy"

rpart_fit = train(factor(CC) ~.,data = train1, method = "rpart", 
                  trControl = fitControl, tuneLength = 10, metric=metric)

pred_dt <- predict(rpart_fit, newdata = test1)
confusionMatrix(table(test1$CC, pred_dt))


################################################################################
#                  model2. random forest : 0.9459
################################################################################

library(caret)
library(randomForest)
set.seed(2022)

fitControl <- trainControl(method="adaptive_cv", repeats=3)
metric <- "Accuracy"

rfgrid <- expand.grid(.mtry=c(1:15))

rf_fit <- train(factor(CC) ~., data=train1, method="rf",
                trControl=fitControl, tuneGrid=rfgrid, metric=metric)

pred_rf <- predict(rf_fit, newdata = test1)
confusionMatrix(table(test1$CC, pred_rf))


################################################################################
#                model3_1. svm_linear : 0.8919
#                            Warning??
################################################################################

library(e1071)
library(tidyverse)
set.seed(2022)
fitControl = trainControl(method = "adaptive_cv", repeats = 10)
metric = "Accuracy"

# linear classifier

svm_linerGrid = expand.grid(C = seq(0.5, 2, length = 20))
svm_linear <- train(factor(CC) ~ ., data = train1, method = "svmLinear",
                    preProcess = c("center","scale"),
                    trControl = fitControl,tuneGrid = svm_linerGrid , metric = metric)

pred_svmliner <- predict(svm_linear, newdata = test1)
confusionMatrix(table(test1$CC, pred_svmliner))


################################################################################
#                   model3_2. svm_kernel :  0.9189
#                           Warning??
################################################################################

library(e1071)
library(tidyverse)
set.seed(2022)
fitControl = trainControl(method = "adaptive_cv", repeats = 10)
metric = "Accuracy"

# linear classifier

svm_kernel <- train(factor(CC) ~ ., data = train1, method = "svmRadial",
                    preProcess = c("center","scale"), tuneLength = 10,
                    trControl = fitControl,  metric = metric)

pred_svmliner <- predict(svm_kernel, newdata = test1)
confusionMatrix(table(test1$CC, pred_svmliner))




################################################################################
#                     model4. gbm : 0.7027
################################################################################

library(caret)

set.seed(2022)
fitControl = trainControl(method = "adaptive_cv", repeats = 10)
metric = "Accuracy"

gbmGrid = expand.grid(interaction.depth = c(1, 3, 5, 7, 9), n.trees = (1:30)*50, shrinkage = c(0.05, 0.1), n.minobsinnode = 20)

gbm_fit = train(factor(CC) ~ ., data = train1, method = "gbm", trControl = fitControl, tuneGrid = gbmGrid, metric = metric)

pred_gb <- predict(gbm_fit, newdata = test1)
confusionMatrix(table(test1$CC, pred_gb))



################################################################################
#                model7. lgbm  : 0.8378
################################################################################

set.seed(2022)
library(Matrix)
library(lightgbm)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)


train.data  <- as.matrix(train1[,-1])
test.data   <- as.matrix(test1[,-1])

train.label <- train1$CC
test.label  <- test1$CC

set.seed(2022)
train_matrix = lgb.Dataset(data = train.data , label = train.label)

# model parameters
params = list(max_bin = 5,
              learning_rate = 0.001,
              objective = "binary",
              metric = 'binary_logloss')

#model training
lgbm <- lightgbm(params = params, train_matrix, valid, nrounds = 1000)

test_prob = predict(lgbm, test.data)
test_pred = rep(0, dim(test1)[1])
test_pred[test_prob > .5] = 1

table(test_pred,test1$CC)
mean(test_pred == test1$CC)


