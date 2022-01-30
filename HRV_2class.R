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

data1 <- data1[,c(2:4, 10:14, 18:23)]
data1[,c(9:14)] <- log(data1[,c(9:14)])
View(data1)

################################################################################
#                  binomial classification (normal vs disorders)

#                   Since the behavior patterns are similar
#               alcoholism and game addiction using only data1.
################################################################################

data_binom = data1
data_binom[data_binom$group == "AUD" | data_binom$group == "IGD","group"] = "DIS"
View(data_binom)
data_binom[data_binom$group == "HC", "group"] <- "0"
data_binom[data_binom$group == "DIS","group"] <- "1"
data_binom$group = as.numeric(data_binom$group)

set.seed(2022)
split1 =  sample(c(rep(0, 0.7 * nrow(data_binom)+1), rep(1, 0.3 * nrow(data_binom))))
train1 <- data_binom[split1 == 0, ]  
test1 <- data_binom[split1 == 1, ]  

dim(train1) ; dim(test1)

################################################################################
#               model1.  Logistic Regression : 0.7174 
#                                         0  1
#                                      0  9  8
#                                      1  5 24
################################################################################


library(caret)

lr_fit = glm(as.factor(group)~.,data = train1, family = 'binomial', maxit = 100)
prob_lr = predict(lr_fit, test1, type="response")
pred_lr = rep(0, dim(test1)[1])
pred_lr[prob_lr > .5] = 1
confusionMatrix(table(test1$group, pred_lr))

################################################################################
#                model1. decision tree : 0.6304
#                                         0  1
#                                      0  2  15
#                                      1  2  27
################################################################################

library(caret)
library(rpart)

set.seed(2022)
fitControl <- trainControl(method="adaptive_cv", repeats=3)
metric <- "Accuracy"

rpart_fit = train(factor(group) ~.,data = train1, method = "rpart", 
                  tuneLength = 10, metric=metric)

pred_dt <- predict(rpart_fit, newdata = test1)
confusionMatrix(table(test1$group, pred_dt))


################################################################################
#                  model2. random forest : 0.6087
#                  model2. random forest(tuning) : 0.6522
#                                         0  1
#                                      0  5  12
#                                      1  4  25
################################################################################

library(caret)
library(randomForest)
set.seed(2022)

fitControl <- trainControl(method="adaptive_cv", repeats=3)
metric <- "Accuracy"

#rfgrid <- expand.grid(.mtry=c(1:15))

rf_fit <- train(factor(group) ~., data=train1, method="rf",
                trControl=fitControl, metric=metric) #tuneGrid=rfgrid)

pred_rf <- predict(rf_fit, newdata = test1)
confusionMatrix(table(test1$group, pred_rf))


################################################################################
#                model3_1. svm_linear : 0.6087 
#                                         0  1
#                                      0  1  16
#                                      1  2  27
################################################################################

library(e1071)
library(tidyverse)
set.seed(2022)
fitControl = trainControl(method = "adaptive_cv", repeats = 10)
metric = "Accuracy"

# linear classifier

svm_linerGrid = expand.grid(C = seq(0.5, 2, length = 20))
svm_linear <- train(factor(group) ~ ., data = train1, method = "svmLinear",
                    preProcess = c("center","scale"),
                    trControl = fitControl,tuneGrid = svm_linerGrid , metric = metric)

pred_svmliner <- predict(svm_linear, newdata = test1)
confusionMatrix(table(test1$group, pred_svmliner))


################################################################################
#                   model3_2. svm_kernel :  0.6739  
#                                         0  1
#                                      0  4  13
#                                      1  2  27
################################################################################

library(e1071)
library(tidyverse)
set.seed(2022)
fitControl = trainControl(method = "adaptive_cv", repeats = 10)
metric = "Accuracy"

# linear classifier

svm_kernel <- train(factor(group) ~ ., data = train1, method = "svmRadial",
                    preProcess = c("center","scale"), tuneLength = 10,
                    trControl = fitControl,  metric = metric)

pred_svmliner <- predict(svm_kernel, newdata = test1)
confusionMatrix(table(test1$group, pred_svmliner))




################################################################################
#                     model4. gbm : 0.5217
#                                         0  1
#                                      0  0  17
#                                      1  5  24
################################################################################

library(caret)

set.seed(2022)
fitControl = trainControl(method = "adaptive_cv", repeats = 3)
metric = "Accuracy"

gbmGrid = expand.grid(interaction.depth = c(1, 3, 5, 7, 9), n.trees = (1:30)*50, shrinkage = c(0.05, 0.1), n.minobsinnode = 20)

gbm_fit = train(factor(group) ~ ., data = train1, method = "gbm", trControl = fitControl, tuneGrid = gbmGrid, metric = metric)

pred_gb <- predict(gbm_fit, newdata = test1)
confusionMatrix(table(test1$group, pred_gb))



################################################################################
#                model7. lgbm  : 0.609
#                                         0  1
#                                      0  0  1
#                                      1 17  28
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

train.label <- train1$group
test.label  <- test1$group

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

table(test_pred,test1$group)
mean(test_pred == test1$group)


################################################################################
################################################################################
################################################################################

################################################################################
#                  binomial classification (alcoholism vs game addiction)
################################################################################

data_disord <- data1[data1$group == "IGD" | data1$group == "AUD",]
View(data_disord)
data_disord[data_disord$group == "IGD", "group"] <- "0"
data_disord[data_disord$group == "AUD", "group"] <- "1"
data_disord$group = as.numeric(data_disord$group)

set.seed(2022)
split2 =  sample(c(rep(0, 0.7 * nrow(data_disord)+1), rep(1, 0.3 * nrow(data_disord))))
train2 <- data_disord[split2 == 0, ]  
test2 <- data_disord[split2 == 1, ]  

dim(train2) ; dim(test2)

################################################################################
#               model1.  Logistic Regression : 0.5172 
#                                         0  1
#                                      0  7  8
#                                      1  6  8
################################################################################


library(caret)

lr_fit2 = glm(as.factor(group)~.,data = train2, family = 'binomial', maxit = 100)
prob_lr2 = predict(lr_fit2, test2, type="response")
pred_lr2 = rep(0, dim(test2)[1])
pred_lr2[prob_lr2 > .5] = 1
confusionMatrix(table(test2$group, pred_lr2))

################################################################################
#                model1. decision tree : 0.5517
#                                         0  1
#                                      0  8  7
#                                      1  6  8
################################################################################

library(caret)
library(rpart)

set.seed(2022)
fitControl <- trainControl(method="adaptive_cv", repeats=3)
metric <- "Accuracy"

rpart_fit2 = train(factor(group) ~.,data = train2, method = "rpart", 
                  tuneLength = 10, metric=metric)

pred_dt2 <- predict(rpart_fit2, newdata = test2)
confusionMatrix(table(test2$group, pred_dt2))

################################################################################
#                  model2. random forest : 0.4828  
#                                         0  1
#                                      0  7  8
#                                      1  7  7
################################################################################

library(caret)
library(randomForest)
set.seed(2022)

fitControl <- trainControl(method="adaptive_cv", repeats=3)
metric <- "Accuracy"

rfgrid <- expand.grid(.mtry=c(1:15))

rf_fit <- train(factor(group) ~., data=train2, method="rf",
                trControl=fitControl, metric=metric) #tuneGrid=rfgrid)

pred_rf <- predict(rf_fit, newdata = test2)
confusionMatrix(table(test2$group, pred_rf))


################################################################################
#                model3_1. svm_linear : 0.5517
################################################################################

library(e1071)
library(tidyverse)
set.seed(2022)
fitControl = trainControl(method = "adaptive_cv", repeats = 10)
metric = "Accuracy"

# linear classifier

svm_linerGrid = expand.grid(C = seq(0.5, 2, length = 20))
svm_linear <- train(factor(group) ~ ., data = train2, method = "svmLinear",
                    preProcess = c("center","scale"),
                    trControl = fitControl,tuneGrid = svm_linerGrid , metric = metric)

pred_svmliner <- predict(svm_linear, newdata = test2)
confusionMatrix(table(test2$group, pred_svmliner))


################################################################################
#                   model3_2. svm_kernel :   0.4138 
################################################################################

library(e1071)
library(tidyverse)
set.seed(2022)
fitControl = trainControl(method = "adaptive_cv", repeats = 10)
metric = "Accuracy"

# linear classifier

svm_kernel <- train(factor(group) ~ ., data = train2, method = "svmRadial",
                    preProcess = c("center","scale"), tuneLength = 10,
                    trControl = fitControl,  metric = metric)

pred_svmliner <- predict(svm_kernel, newdata = test2)
confusionMatrix(table(test2$group, pred_svmliner))



################################################################################
#                     model4. gbm : 0.5217
################################################################################

library(caret)

set.seed(2022)
fitControl = trainControl(method = "adaptive_cv", repeats = 3)
metric = "Accuracy"

#gbmGrid = expand.grid(interaction.depth = c(1, 3, 5, 7, 9), n.trees = (1:30)*50, shrinkage = c(0.05, 0.1), n.minobsinnode = 20)

gbm_fit = train(factor(group) ~ ., data = train2, method = "gbm", trControl = fitControl, metric = metric)

pred_gb <- predict(gbm_fit, newdata = test2)
confusionMatrix(table(test2$group, pred_gb))



################################################################################
#                model7. lgbm  : 0.609
################################################################################

set.seed(2022)
library(Matrix)
library(lightgbm)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)


train.data  <- as.matrix(train2[,-1])
test.data   <- as.matrix(test2[,-1])

train.label <- train2$group
test.label  <- test2$group

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
test_pred = rep(0, dim(test2)[1])
test_pred[test_prob > .5] = 1

table(test_pred,test2$group)
mean(test_pred == test2$group)
