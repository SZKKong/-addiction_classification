################################################################################
#                               load data
################################################################################

library(readxl)
data <- read_excel(file.choose(),col_names=TRUE) 
head(data) ;dim(data) 

################################################################################
#                                 pre_processing
#                           group으로 나누기 (by frequency)
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
#                           pre_processing
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
pca_data8 = pca_dt8$x[,1:11]

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




# get final data


final_data = cbind(pca_data1,pca_data2,pca_data3,pca_data4,pca_data5,
                   pca_data6,pca_data7,pca_data8,pca_data9,pca_data10,
                   pca_data11,pca_data12,pca_data13)


# rename
for (i in 1:length(colnames(final_data))) {
  colnames(final_data)[i] =  paste("X", i, sep = "")
}

# add (age,sex,group==CC)
final_data = cbind(data[2:4],final_data)
final_data = data.frame(final_data)
colnames(final_data)[1] = "CC"


# convert data as numeric
for (i in 4:dim(final_data)[2]){
  final_data[,i] = as.numeric(final_data[,i])
}


final_data$sex = as.factor(final_data$sex)


################################################################################
#                        3 group classification 
################################################################################

# check age,sex !

final_data[final_data$CC == "HC","CC"] = 0 
final_data[final_data$CC == "AUD","CC"] = 1
final_data[final_data$CC == "IGD","CC"] = 2
final_data$CC = as.factor(final_data$CC)

set.seed(2022)
split1 =  sample(c(rep(0, 0.7 * nrow(final_data)), rep(1, 0.3 * nrow(final_data))))
train1 <- final_data[split1 == 0, ]  
test1 <- final_data[split1== 1, ]  

dim(train1) ; dim(test1)


################################################################################
#                 1.  Logistic Regression : 0.413
################################################################################


library(caret)
library(nnet)

mr_fit = multinom(factor(CC)~.,data = train1)
pred_mr = predict(mr_fit,test1)
confusionMatrix(table(test1$CC, pred_mr))


# Confusion Matrix and Statistics

#pred_mr
#    AUD HC IGD
#AUD   8  2   1
#HC    5  9   8
#IGD   3  8   2

################################################################################
#                model1. decision tree : 0.5435
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

# pred_dt
#   AUD HC IGD
#AUD   4  7   0
#HC    1 21   0
#IGD   2 11   0


################################################################################
#                  model2. random forest : 0.5
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

# Confusion Matrix and Statistics

# pred_rf
#    AUD HC IGD
# AUD   4  3   4
# HC    4 12   6
# IGD   3  3   7


################################################################################
#                model3_1. svm_linear : 0.4783
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
#    AUD HC IGD
# AUD   7  2   2
# HC    7 11   4
# IGD   2  7   4

################################################################################
#                   model3_2. svm_kernel :  0.5217
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
#    AUD HC IGD
# AUD   5  1   5
# HC    6 12   4
# IGD   3  3   7


################################################################################
#                     model4. gbm : 0.5
################################################################################

library(caret)

set.seed(2022)
fitControl = trainControl(method = "adaptive_cv", repeats = 10)
metric = "Accuracy"

gbmGrid = expand.grid(interaction.depth = c(1, 3, 5, 7, 9), n.trees = (1:30)*50, shrinkage = c(0.05, 0.1), n.minobsinnode = 20)

gbm_fit = train(factor(CC) ~ ., data = train1, method = "gbm", trControl = fitControl, tuneGrid = gbmGrid, metric = metric)

pred_gb <- predict(gbm_fit, newdata = test1)
confusionMatrix(table(test1$CC, pred_gb))

# Confusion Matrix and Statistics

# pred_gb
#    AUD HC IGD
# AUD   5  2   4
# HC    5 10   7
# IGD   3  2   8

################################################################################
#                model7. lgbm  : 0.4783
################################################################################

train.data  <- as.matrix(train1[,-1])
test.data   <- as.matrix(test1[,-1])

train.label <- train1$CC
test.label  <- test1$CC

train.label[train.label == "IGD"] = 2
train.label[train.label == "AUD"] = 1
train.label[train.label == "HC"] = 0

test.label[test.label == "IGD"] = 2
test.label[test.label == "AUD"] = 1
test.label[test.label == "HC"] = 0

train.label = as.numeric(train.label)
test.label = as.numeric(test.label)

set.seed(2022)
library(Matrix)
library(lightgbm)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)


set.seed(2022)
train_matrix = lgb.Dataset(data = train.data , label = train.label)


# model parameters
params = list(max_bin = 5, learning_rate = 0.001, objective = "multiclass", num_class = 3, eval_metric = "mlogloss")
lgbm <- lightgbm(params = params, train_matrix, valid, nrounds = 1000)
test_pred = predict(lgbm, test.data,reshape = TRUE)
pred <- data.frame(test_pred) %>% mutate(max_prob = max.col(., ties.method = "last"), label = test.label )
confusionMatrix(factor(pred$max_prob-1), factor(pred$label))



# Confusion Matrix and Statistics

# Reference
# Prediction  
#  0  1  2
# 0 11  3  5
# 1  4  5  2
# 2  7  3  6


