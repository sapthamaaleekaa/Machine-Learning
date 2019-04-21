setwd("C:\\Users\\Home\\Desktop\\Saptha\\Machine-Learning\\ltfs")
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_121')
library('eeptools')
library("MASS") 
library(caTools)
library('randomForest')
library("pROC") #For ROC curve

#################### For increasing R's default number of rows to be printed ####################
options(max.print=1000000)

#################### Reading Training and Test Datasets ####################
set.seed(100)
train <- read.csv("train.csv",stringsAsFactors = FALSE)
test <- read.csv("test.csv",stringsAsFactors = FALSE)

#################### EDA ####################
dim(train)
colnames(train)
head(train)
summary(train)
str(train)
table(train$loan_default) 

#################### Data preparation #################### 
train$UniqueID <- NULL
train$DisbursalDate <- NULL
train$PERFORM_CNS.SCORE <- NULL
train$branch_id <- NULL
train$supplier_id <- NULL
train$manufacturer_id <- NULL
train$Employee_code_ID <- NULL
train$State_ID <- NULL
train$Current_pincode_ID <- NULL
train$MobileNo_Avl_Flag <- NULL #Everyone has a mobile number

train$loan_default <- as.factor(train$loan_default)
train$Employment.Type <- as.factor(train$Employment.Type)
train$Aadhar_flag <- as.factor(train$Aadhar_flag)
train$PAN_flag <- as.factor(train$PAN_flag)
train$VoterID_flag <- as.factor(train$VoterID_flag)                  
train$Driving_flag <- as.factor(train$Driving_flag)                     
train$Passport_flag <- as.factor(train$Passport_flag)
train$PERFORM_CNS.SCORE.DESCRIPTION <- as.factor(train$PERFORM_CNS.SCORE.DESCRIPTION)

# Converting D.O.B to age
date <- as.Date(train$Date.of.Birth,  format = "%d-%m-%y")
correctDate <- as.Date(ifelse(date > as.Date(Sys.Date(), format = "%d-%m-%Y"), format(date, "19%y-%m-%d"), format(date)))
train$age <- round(age_calc(correctDate, enddate = as.Date(Sys.Date(), format = "%d-%m-%Y"), units = "years"))
Date.of.Birth <- train$Date.of.Birth
train$Date.of.Birth <- NULL

# Converting AVERAGE.ACCT.AGE,CREDIT.HISTORY.LENGTH to exact number of months

for (i in seq(1,233154,1)){
  train[i,'AVERAGE.ACCT.AGE.MONTHS'] <- as.numeric(regmatches(train[i,'AVERAGE.ACCT.AGE'], gregexpr("[0-9]+", train[i,'AVERAGE.ACCT.AGE']))[[1]][1])*12 + 
    as.numeric(regmatches(train[i,'AVERAGE.ACCT.AGE'], gregexpr("[0-9]+", train[i,'AVERAGE.ACCT.AGE']))[[1]][2])
  train[i,'CREDIT.HISTORY.LENGTH.MONTHS'] <- as.numeric(regmatches(train[i,'CREDIT.HISTORY.LENGTH'], gregexpr("[0-9]+", train[i,'CREDIT.HISTORY.LENGTH']))[[1]][1])*12 + 
    as.numeric(regmatches(train[i,'CREDIT.HISTORY.LENGTH'], gregexpr("[0-9]+", train[i,'CREDIT.HISTORY.LENGTH']))[[1]][2])
}


train$AVERAGE.ACCT.AGE <- NULL
train$CREDIT.HISTORY.LENGTH <- NULL

#Apply the same to test data
test$Employment.Type <- as.factor(test$Employment.Type)
test$Aadhar_flag <- as.factor(test$Aadhar_flag)
test$PAN_flag <- as.factor(test$PAN_flag)
test$VoterID_flag <- as.factor(test$VoterID_flag)                  
test$Driving_flag <- as.factor(test$Driving_flag)                     
test$Passport_flag <- as.factor(test$Passport_flag)
test$PERFORM_CNS.SCORE.DESCRIPTION <- as.factor(test$PERFORM_CNS.SCORE.DESCRIPTION)

date <- as.Date(test$Date.of.Birth,  format = "%d-%m-%y")
correctDate <- as.Date(ifelse(date > as.Date(Sys.Date(), format = "%d-%m-%Y"), format(date, "19%y-%m-%d"), format(date)))
test$age <- round(age_calc(correctDate, enddate = as.Date(Sys.Date(), format = "%d-%m-%Y"), units = "years"))
Date.of.Birth <- test$Date.of.Birth
test$Date.of.Birth <- NULL

for (i in seq(1,112392,1)){
  test[i,'AVERAGE.ACCT.AGE.MONTHS'] <- as.numeric(regmatches(test[i,'AVERAGE.ACCT.AGE'], gregexpr("[0-9]+", test[i,'AVERAGE.ACCT.AGE']))[[1]][1])*12 + 
    as.numeric(regmatches(test[i,'AVERAGE.ACCT.AGE'], gregexpr("[0-9]+", test[i,'AVERAGE.ACCT.AGE']))[[1]][2])
  test[i,'CREDIT.HISTORY.LENGTH.MONTHS'] <- as.numeric(regmatches(test[i,'CREDIT.HISTORY.LENGTH'], gregexpr("[0-9]+", test[i,'CREDIT.HISTORY.LENGTH']))[[1]][1])*12 + 
    as.numeric(regmatches(test[i,'CREDIT.HISTORY.LENGTH'], gregexpr("[0-9]+", test[i,'CREDIT.HISTORY.LENGTH']))[[1]][2])
}

dim(train)
dim(test)

# checking for NA values
na_columns_train = colnames(train)[colSums(is.na(train)) > 0] # There is no NA values in the dataset
na_columns_test = colnames(test)[colSums(is.na(test)) > 0] 
###### Building Basic model ###### 

#################### Splitting train data as train, validation and test data #################### 
set.seed(123)
split <- sample.split(train,SplitRatio = 0.80)
training<- subset(train,split == TRUE)
testing <- subset(train,split == FALSE)

data.rose <- ROSE(loan_default~., data=training, seed=3)$data

loan_default <- data.rose$loan_default
data.rose$loan_default <- NULL

# Tuning Random Forest Parameters
###### Deciding the number of trees ###### 

train_auc <- data.frame(matrix(ncol = 2, nrow = 0))
test_auc <- data.frame(matrix(ncol = 2, nrow = 0))

for (tree in c(5,10,25,50,75,100,150,250)){
  gc()
  set.seed(777)
  classifier = randomForest(x = data.rose,y = loan_default,do.trace = TRUE,ntree = tree)
  data.rose$predicted = predict(classifier, newdata = data.rose, type="prob")
  testing$predicted = predict(classifier, newdata = testing, type="prob")
  
  ####### Train ####### 
  auc <- auc(loan_default, data.rose$predicted[,2])[1]
  new_row <- list(tree,auc)
  train_auc <- rbind(new_row,train_auc)
  
  ####### Test ####### 
  auc <- auc(testing$loan_default, testing$predicted[,2])[1]
  new_row <- list(tree,auc)
  test_auc <- rbind(new_row,test_auc)
  
  testing$predicted<- NULL
  data.rose$predicted<- NULL
}

colnames(train_auc) <- c("Trees","AUC")
colnames(test_auc) <- c("Trees","AUC")

# > train_auc
# Trees       AUC
# 1   250 1.0000000
# 2   150 1.0000000
# 3   100 1.0000000
# 4    75 1.0000000
# 5    50 0.9999999
# 6    25 0.9999955
# 7    10 0.9998542
# 8     5 0.9984131
# > test_auc
# Trees       AUC
# 1   250 0.5804766
# 2   150 0.5730487
# 3   100 0.5590522
# 4    75 0.5591149
# 5    50 0.5547228
# 6    25 0.5425041
# 7    10 0.5451314
# 8     5 0.5475844

Trees = c(250,150,100,75,50,25,10,5)
library(ggplot2)
ggplot() + geom_line(aes(x=Trees,y=train_auc[,2]),color='red') + 
  geom_line(aes(x=Trees,y=test_auc[,2]),color='blue') +  geom_hline(yintercept = 0.5804766) + geom_vline(xintercept = 225) + 
  xlab('Trees')+ylab('AUC')

# Ideal no. of trees : 225

############# No. of parameters to try at each split #############  

train_auc <- data.frame(matrix(ncol = 2, nrow = 0))
test_auc <- data.frame(matrix(ncol = 2, nrow = 0))

for (NoOfParams in seq(1,30,1)){
  gc()
  set.seed(777)
  classifier = randomForest(x = data.rose,y = loan_default,do.trace = TRUE,ntree = 50,mtry = NoOfParams)
  data.rose$predicted = predict(classifier, newdata = data.rose, type="prob")
  testing$predicted = predict(classifier, newdata = testing, type="prob")
  
  ####### Train ####### 
  auc <- auc(loan_default, data.rose$predicted[,2])[1]
  new_row <- list(NoOfParams,auc)
  train_auc <- rbind(new_row,train_auc)
  
  ####### Test ####### 
  auc <- auc(testing$loan_default, testing$predicted[,2])[1]
  new_row <- list(NoOfParams,auc)
  test_auc <- rbind(new_row,test_auc)
  
  testing$predicted<- NULL
  data.rose$predicted<- NULL
}

colnames(train_auc) <- c("No. of Params","AUC")
colnames(test_auc) <- c("No. of Params","AUC")

# > train_auc
# No. of Params       AUC
# 1     30 0.9999997
# 2     29 0.9999999
# 3     28 0.9999996
# 4     27 0.9999999
# 5     26 0.9999999
# 6     25 0.9999999
# 7     24 0.9999998
# 8     23 0.9999998
# 9     22 0.9999998
# 10    21 0.9999998
# 11    20 0.9999996
# 12    19 0.9999998
# 13    18 0.9999999
# 14    17 0.9999999
# 15    16 0.9999998
# 16    15 0.9999999
# 17    14 0.9999998
# 18    13 0.9999999
# 19    12 0.9999999
# 20    11 0.9999998
# 21    10 0.9999998
# 22     9 0.9999999
# 23     8 0.9999999
# 24     7 0.9999999
# 25     6 0.9999999
# 26     5 0.9999999
# 27     4 1.0000000
# 28     3 1.0000000
# 29     2 0.9999999
# 30     1 0.9717169
# > test_auc
# No. of Params       AUC
# 1     30 0.5379196
# 2     29 0.5652249
# 3     28 0.5613604
# 4     27 0.5695039
# 5     26 0.5396298
# 6     25 0.5364347
# 7     24 0.5736049
# 8     23 0.5453245
# 9     22 0.5237175
# 10    21 0.5588683
# 11    20 0.5479778
# 12    19 0.5753162
# 13    18 0.5433190
# 14    17 0.5590636
# 15    16 0.5653078
# 16    15 0.5674212
# 17    14 0.5617360
# 18    13 0.5831796
# 19    12 0.5654845
# 20    11 0.5707561
# 21    10 0.5510396
# 22     9 0.5409427
# 23     8 0.5306717
# 24     7 0.5327888
# 25     6 0.5645195
# 26     5 0.5547228
# 27     4 0.5709279
# 28     3 0.5851456
# 29     2 0.5864100
# 30     1 0.6164948

NoOfParams = seq(30,1,-1)
library(ggplot2)
ggplot() + geom_line(aes(x=NoOfParams,y=train_auc[,2]),color='red') + 
  geom_line(aes(x=NoOfParams,y=test_auc[,2]),color='blue') +  geom_vline(xintercept = 1) + geom_hline(yintercept = 0.6164948) +
  xlab('No. of Parameters')+ylab('AUC')

# Ideal number of parameters: 1

############## Minimum of samples at root node ##############

train_auc <- data.frame(matrix(ncol = 2, nrow = 0))
test_auc <- data.frame(matrix(ncol = 2, nrow = 0))

for (NodeSize in seq(50,500,50)){
  gc()
  set.seed(777)
  classifier = randomForest(x = data.rose,y = loan_default,do.trace = TRUE,ntree = 50,nodesize = NodeSize)
  data.rose$predicted = predict(classifier, newdata = data.rose, type="prob")
  testing$predicted = predict(classifier, newdata = testing, type="prob")
  
  ####### Train ####### 
  auc <- auc(loan_default, data.rose$predicted[,2])[1]
  new_row <- list(NodeSize,auc)
  train_auc <- rbind(new_row,train_auc)
  
  ####### Test ####### 
  auc <- auc(testing$loan_default, testing$predicted[,2])[1]
  new_row <- list(NodeSize,auc)
  test_auc <- rbind(new_row,test_auc)
  
  testing$predicted<- NULL
  data.rose$predicted<- NULL
}

colnames(train_auc) <- c("Nodesize","AUC")
colnames(test_auc) <- c("Nodesize","AUC")

NodeSize = seq(500,50,-50)
library(ggplot2)
ggplot() + geom_line(aes(x=NodeSize,y=train_auc[,2]),color='red') + 
  geom_line(aes(x=NodeSize,y=test_auc[,2]),color='blue') +  geom_vline(xintercept = 350) + geom_hline(yintercept = 0.5697733) +
  xlab('No. of Samples at leaf node')+ylab('AUC')

# > test_auc
# X50 X0.535770896949797
# 1  500          0.5250464
# 2  450          0.5299721
# 3  400          0.5292060
# 4  350          0.5697733
# 5  300          0.5176558
# 6  250          0.5507275
# 7  200          0.5291474
# 8  150          0.5140379
# 9  100          0.5607272
# 10  50          0.5357709
# > train_auc
# X50 X0.988152069455189
# 1  500          0.9609633
# 2  450          0.9610936
# 3  400          0.9632725
# 4  350          0.9639352
# 5  300          0.9659609
# 6  250          0.9683847
# 7  200          0.9708638
# 8  150          0.9748796
# 9  100          0.9798772
# 10  50          0.9881521

# Ideal node size:350

############## Applying everything to actual dataset ##############

levels(test$PERFORM_CNS.SCORE.DESCRIPTION) <- levels(train$PERFORM_CNS.SCORE.DESCRIPTION)

data.rose.train <- ROSE(loan_default~., data=train, seed=3)$data

loan_def <- data.rose.train$loan_default
data.rose.train$loan_default <- NULL

gc()
set.seed(777)
classifier = randomForest(x = data.rose.train ,y = loan_def ,do.trace = TRUE,ntree = 225, mtry =1, nodesize = 350)
test$predicted = predict(classifier, newdata = test)

sub <- data.frame()
sub <- test[,c(1,43)]
write.csv(sub, file = "ltfs2.csv")



