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

data1$`8.drinking`[data1$`8.drinking`== 4 | data1$`8.drinking`== 999] = NA
data1$`8.drinking` = as.factor(data1$`8.drinking`)
data1$`8.drinking`[is.na(data1$`8.drinking`)] = 0

data1$`8-1.drinking_F`[data1$`8-1.drinking_F`== '999' | data1$`8-1.drinking_F`== 'MISS' | data1$`8-1.drinking_F`== 'n/a'] = NA
print(data1[is.na(data1$`8-1.drinking_F`),],n=30) ##### fill 0
data1$`8-1.drinking_F`[is.na(data1$`8-1.drinking_F`)] = 0
data1$`8-1.drinking_F` = as.numeric(data1$`8-1.drinking_F`)

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






# drop the Fagerstrom (a lot of missing)
data1 = subset(data1,select=-c(Fagerstrom))

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

#                   Since the behavior patterns are similar
#               alcoholism and game addiction using only data1.
################################################################################

data_binom = data1
data_binom[data_binom$CC == "AUD" | data_binom$CC == "IGD","CC"] = "ADIC"
data_binom[data_binom$CC == "HC","CC"] = 0 
data_binom[data_binom$CC == "ADIC","CC"] = 1
data_binom$CC = as.numeric(data_binom$CC)

set.seed(20220125)
split1 =  sample(c(rep(0, 0.7 * nrow(data_binom)), rep(1, 0.3 * nrow(data_binom))))
train1 <- data_binom[split1 == 0, ]  
test1 <- data_binom[split1== 1, ]  

dim(train1) ; dim(test1)


################################################################################
#                       1.  Logistic Regression
################################################################################


library(caret)

lr_fit = glm(CC~.,data = train1,family = 'binomial', maxit = 100)
prob_lr = predict(lr_fit,test1,type="response")
pred_lr = rep(0, dim(test1)[1])
pred_lr[prob_lr > .5] = 1
confusionMatrix(table(test1$CC, pred_lr))



################################################################################
#                      3-group classification with data1
################################################################################

# split data set

set.seed(20220125)
split1 =  sample(c(rep(0, 0.7 * nrow(data1)), rep(1, 0.3 * nrow(data1))))
train1 <- data1[split1 == 0, ]  
test1 <- data1[split1== 1, ]  

dim(train1) ; dim(test1)

################################################################################
#                       1.  Logistic Regression
# 
################################################################################

library(caret)

lr_fit = glm(factor(CC)~.,data = dat_tra,family = 'binomial')

prob_lr = predict(lr_fit,dat_tes,type="response")
pred_lr = rep(0, dim(dat_tes)[1])
pred_lr[prob_lr > .5] = 1
confusionMatrix(table(dat_tes$cgis_r_w24, pred_lr))


