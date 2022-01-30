################################################################################
#                               load data
################################################################################

library(readxl)
data <- read_excel(file.choose(),col_names=TRUE) 
head(data) ;dim(data) 

################################################################################
#                            pre_processing
#                           group으로 나누기
################################################################################

# group 분류

data_group = subset(data, select = -c(ID,group,age,sex))
cut_list = c()
y = data$group
  
for (i in 1:length(colnames(data_group))) {
  char = "..."
 
   if (grepl(char, colnames(data_group)[i], fixed = TRUE)) {
     print(i)
     cut_list = append(cut_list,i)
  }
}



for (i in 1:length(cut_list)){
  
  if (i==1){
  num = cut_list[i]
  group_name = paste("data", i, sep = "")
  assign(group_name,data_group[,1:(num-1)]) 
  
  }
  
  else{
    num1 = cut_list[i-1]
    num2 = cut_list[i]
    group_name = paste("data", i, sep = "")
    assign(group_name,data_group[,(num1+1):(num2-1)]) 
    
  }
  
}

#group1 ~ group13

################################################################################
#                            pre_processing_no.1
#                       dimension reduction (with PCA)
################################################################################

pca_dt1 = prcomp(data1, center = T, scale. = T)
summary(pca_dt1)
pca_data1 = pca_dt1$x[,1:4]

pca_dt2 = prcomp(data2, center = T, scale. = T)
summary(pca_dt2)
pca_data2 = pca_dt2$x[,1:2]

pca_dt3 = prcomp(data3, center = T, scale. = T)
summary(pca_dt3)
pca_data3 = pca_dt3$x[,1:2]

pca_dt4 = prcomp(data4, center = T, scale. = T)
summary(pca_dt4)
pca_data4 = pca_dt4$x[,1:3]

pca_dt5 = prcomp(data5, center = T, scale. = T)
summary(pca_dt5)
pca_data5 = pca_dt5$x[,1:3]

pca_dt6 = prcomp(data6, center = T, scale. = T)
summary(pca_dt6)
pca_data6 = pca_dt6$x[,1:3]

pca_dt7 = prcomp(data7, center = T, scale. = T)
summary(pca_dt7)
pca_data7 = pca_dt7$x[,1:3]

pca_dt8 = prcomp(data8, center = T, scale. = T)
summary(pca_dt8)
pca_data11 = pca_dt8$x[,1:11]

pca_dt9 = prcomp(data9, center = T, scale. = T)
summary(pca_dt9)
pca_data9 = pca_dt9$x[,1:11]

pca_dt10 = prcomp(data10, center = T, scale. = T)
summary(pca_dt10)
pca_data10 = pca_dt10$x[,1:8]

pca_dt11 = prcomp(data11, center = T, scale. = T)
summary(pca_dt11)
pca_data11 = pca_dt11$x[,1:7]

pca_dt12 = prcomp(data12, center = T, scale. = T)
summary(pca_dt12)
pca_data12 = pca_dt12$x[,1:4]

pca_dt13 = prcomp(data13, center = T, scale. = T)
summary(pca_dt13)
pca_data13 = pca_dt13$x[,1:3]


final_data = cbind(pca_data1,pca_data2,pca_data3,pca_data4,pca_data5,
                   pca_data6,pca_data7,pca_data8,pca_data9,pca_data10,
                   pca_data11,pca_data12,pca_data13)


for (i in 1:length(colnames(final_data))) {
  colnames(final_data)[i] =  paste("X", i, sep = "")
}


final_data = cbind(data[2:4],final_data)
final_data = data.frame(final_data)
colnames(final_data)[1] = "CC"
dim(final_data)


for (i in 4:dim(final_data)[2]){
  final_data[,i] = as.numeric(final_data[,i])
}

final_data$sex = as.factor(final_data$sex)




################################################################################
#                  binomial classification (normal vs disorders)
################################################################################

data_binom = final_data
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
#                 1.  Logistic Regression : 0.5435
################################################################################


library(caret)

lr_fit = glm(CC~.,data = train1,family = 'binomial', maxit = 100)
prob_lr = predict(lr_fit,test1,type="response")
pred_lr = rep(0, dim(test1)[1])
pred_lr[prob_lr > .5] = 1
confusionMatrix(table(test1$CC, pred_lr))


################################################################################
#                model1. decision tree : 0.5217
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
#                  model2. random forest : 0.587
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
#                model3_1. svm_linear : 0.6957
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
#                   model3_2. svm_kernel :  0.6522
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
#                     model4. gbm : 0.6087
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
#                model7. lgbm  : 0.6087
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

data_diso = final_data[final_data$CC == "AUD" | final_data$CC == "IGD",] 

data_diso[data_diso$CC == "IGD","CC"] = 0
data_diso[data_diso$CC == "AUD","CC"] = 1 
data_diso$CC = as.numeric(data_diso$CC)

set.seed(2022)
split1 =  sample(c(rep(0, 0.7 * nrow(data_diso)), rep(1, 0.3 * nrow(data_diso))))
train1 <- data_diso[split1 == 0, ]  
test1 <- data_diso[split1== 1, ]  

dim(train1) ; dim(test1)


################################################################################
#                 1.  Logistic Regression : 0.5862
################################################################################


library(caret)

lr_fit = glm(CC~.,data = train1,family = 'binomial', maxit = 100)
prob_lr = predict(lr_fit,test1,type="response")
pred_lr = rep(0, dim(test1)[1])
pred_lr[prob_lr > .5] = 1
confusionMatrix(table(test1$CC, pred_lr))

#Confusion Matrix and Statistics

# pred_lr
#   0  1
#0 10  4
#1  8  7

################################################################################
#                model1. decision tree : 0.6207
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

# Confusion Matrix and Statistics

#pred_dt
#  0  1
#0 11  3
# 1  8  7

################################################################################
#                  model2. random forest : 0.6897
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

#  pred_rf
#  0  1
#0 10  4
#1  5 10

################################################################################
#                model3_1. svm_linear : 0.5517
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


# Confusion Matrix and Statistics

# pred_svmliner
#   0  1
# 0 10  4
# 1  9  6

################################################################################
#                   model3_2. svm_kernel :  0.7586
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


# Confusion Matrix and Statistics
# pred_svmliner
#   0  1
#0 11  3
#1  4 11



################################################################################
#                     model4. gbm : 0.6897
################################################################################

library(caret)

set.seed(2022)
metric = "Accuracy"

gbm_fit = train(factor(CC) ~ ., data = train1, method = "gbm", metric = metric)

pred_gb <- predict(gbm_fit, newdata = test1)
confusionMatrix(table(test1$CC, pred_gb))



# Confusion Matrix and Statistics

# pred_gb
#  0  1
#0 11  3
#1  6  9

################################################################################
#                model7. lgbm  : 0.6551
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






