require("ggplot2")
require("Hmisc")
require("caret")
library("dummies")
library("caTools")
library("MLmetrics")
setwd("C:\\Users\\Home\\Desktop\\Saptha")
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_121')

############# For increasing R's default number of rows to be printed ############ 
options(max.print=1000000)

###############Reading Training and Test Datasets#############
set.seed(100)
house_train <- read.csv("train.csv")
house_test <- read.csv("test.csv")

##############Viewing the datasets#################
dim(house_train)
colnames(house_train)
head(house_train)
summary(house_train)
str(house_train)

############### Removing ID column #################
ID <- house_train[,1]
house_train<-house_train[,-1]

##################### Imputing NA columns for few based on description ####################

############# Common Function to impute ###############
imputeGiven <- function(feature,Value)
{
  feature <- as.character(feature)
  feature <- ifelse(is.na(feature),Value,feature)
  feature <- as.factor(feature)
  return (feature)
}

house_train$Alley  <- imputeGiven(house_train$Alley ,"No alley access") 
house_train$BsmtQual  <- imputeGiven(house_train$BsmtQual ,"No Basement") 
house_train$BsmtCond<- imputeGiven(house_train$BsmtCond ,"No Basement")  
house_train$BsmtExposure <- imputeGiven(house_train$BsmtExposure ,"No Basement") 
house_train$BsmtFinType1 <- imputeGiven(house_train$BsmtFinType1 ,"No Basement") 
house_train$BsmtFinType2 <- imputeGiven(house_train$BsmtFinType2 ,"No Basement") 
house_train$BsmtExposure <- imputeGiven(house_train$BsmtExposure ,"No Basement") 
house_train$FireplaceQu <- imputeGiven(house_train$FireplaceQu ,"No Fireplace") 
house_train$GarageType  <- imputeGiven(house_train$GarageType ,"No Garage") 
house_train$GarageFinish  <- imputeGiven(house_train$GarageFinish ,"No Garage") 
house_train$GarageQual<- imputeGiven(house_train$GarageQual,"No Garage") 
house_train$GarageCond<- imputeGiven(house_train$GarageCond,"No Garage") 
house_train$PoolQC <- imputeGiven(house_train$PoolQC,"No Pool")
house_train$Fence<- imputeGiven(house_train$Fence,"No Fence")
house_train$MiscFeature <- imputeGiven(house_train$MiscFeature,"None")


# Do the same for test data #

house_test$Alley  <- imputeGiven(house_test$Alley ,"No alley access") 
house_test$BsmtQual  <- imputeGiven(house_test$BsmtQual ,"No Basement") 
house_test$BsmtCond<- imputeGiven(house_test$BsmtCond ,"No Basement")  
house_test$BsmtExposure <- imputeGiven(house_test$BsmtExposure ,"No Basement") 
house_test$BsmtFinType1 <- imputeGiven(house_test$BsmtFinType1 ,"No Basement") 
house_test$BsmtFinType2 <- imputeGiven(house_test$BsmtFinType2 ,"No Basement") 
house_test$BsmtExposure <- imputeGiven(house_test$BsmtExposure ,"No Basement") 
house_test$FireplaceQu <- imputeGiven(house_test$FireplaceQu ,"No Fireplace") 
house_test$GarageType  <- imputeGiven(house_test$GarageType ,"No Garage") 
house_test$GarageFinish  <- imputeGiven(house_test$GarageFinish ,"No Garage") 
house_test$GarageQual<- imputeGiven(house_test$GarageQual,"No Garage") 
house_test$GarageCond<- imputeGiven(house_test$GarageCond,"No Garage") 
house_test$PoolQC <- imputeGiven(house_test$PoolQC,"No Pool")
house_test$Fence<- imputeGiven(house_test$Fence,"No Fence")
house_test$MiscFeature <- imputeGiven(house_test$MiscFeature,"None")

############### Finding Rows with NA values #############
na_columns = colnames(house_train)[colSums(is.na(house_train)) > 0]
non_na_columns = setdiff(colnames(house_train),na_columns)

##################### Removing Dependent(target) variable from non NA columns ###########
non_na_columns <- non_na_columns[-75]

############## Finding Categorical variables ###############
isfactor <- house_train[,sapply(house_train,is.factor)]
isfactorcolumns <- colnames(isfactor)

################### Finding Continuos variables ############
iscontinuoscolumns <- setdiff(colnames(house_train),isfactorcolumns)
iscontinuoscolumns <- iscontinuoscolumns[-37]
iscontinuos <- house_train[,iscontinuoscolumns]

################# Treating NA columns #############################
print (na_columns)

############## house_train$LotFrontage #################
hist(house_train$LotFrontage, freq = TRUE )
Mean <- mean(house_train$LotFrontage,na.rm = TRUE)
house_train$LotFrontage<- ifelse(is.na(house_train$LotFrontage),Mean, house_train$LotFrontage)
house_train$LotFrontage<- as.numeric(house_train$LotFrontage)
hist(house_train$LotFrontage, freq = TRUE )
sum(is.na(house_train$LotFrontage))
summary(house_train$LotFrontage)

############################ house_train$MasVnrType #####################
summary(house_train$MasVnrType)
ggplot(house_train,aes(x = house_train$MasVnrType)) + geom_bar(color = "black" , aes(fill = house_train$MasVnrType)) + labs(title = "MasVnrType")
house_train$MasVnrType<- as.character(house_train$MasVnrType)
# Imputing with mode #
house_train$MasVnrType<- ifelse(is.na(house_train$MasVnrType),"None", house_train$MasVnrType)
house_train$MasVnrType<- as.factor(house_train$MasVnrType)
summary(house_train$MasVnrType)

############################ house_train$MasVnrArea #####################
summary(house_train$MasVnrArea)
hist(house_train$MasVnrArea, freq = TRUE)
# The distribution is positively skewed #
Median <- median(house_train$MasVnrArea,na.rm = TRUE)
house_train$MasVnrArea<- ifelse(is.na(house_train$MasVnrArea),Median, house_train$MasVnrArea)
house_train$MasVnrArea<- as.numeric(house_train$MasVnrArea)
summary(house_train$MasVnrArea)
hist(house_train$MasVnrArea, freq = TRUE)

############################house_train$Electrical#####################
summary(house_train$Electrical)
ggplot(house_train,aes(x = house_train$Electrical)) + geom_bar(color = "black" , aes(fill = house_train$Electrical)) + labs(title = "Electrical")
# Imputing with mode #
house_train$Electrical<- as.character(house_train$Electrical)
house_train$Electrical<- ifelse(is.na(house_train$Electrical),"SBrkr", house_train$Electrical)
house_train$Electrical<- as.factor(house_train$Electrical)
summary(house_train$Electrical)

############################ house_train$GarageYrBlt #####################
summary(house_train$GarageYrBlt)
hist(house_train$GarageYrBlt, freq = TRUE)
# The distribution is negatively skewed #
Median <- median(house_train$GarageYrBlt,na.rm = TRUE)
house_train$GarageYrBlt<- ifelse(is.na(house_train$GarageYrBlt),Median, house_train$GarageYrBlt)
house_train$GarageYrBlt<- as.numeric(house_train$GarageYrBlt)
summary(house_train$GarageYrBlt)
hist(house_train$GarageYrBlt, freq = TRUE)

# Apply the same imputations to test data #

Mean <- mean(house_test$LotFrontage,na.rm = TRUE)
house_test$LotFrontage<- ifelse(is.na(house_test$LotFrontage),Mean, house_test$LotFrontage)
house_test$LotFrontage<- as.numeric(house_test$LotFrontage)
house_test$MasVnrType<- as.character(house_test$MasVnrType)
house_test$MasVnrType<- ifelse(is.na(house_test$MasVnrType),"None", house_test$MasVnrType)
house_test$MasVnrType<- as.factor(house_test$MasVnrType)
Median <- median(house_test$MasVnrArea,na.rm = TRUE)
house_test$MasVnrArea<- ifelse(is.na(house_test$MasVnrArea),Median, house_test$MasVnrArea)
house_test$MasVnrArea<- as.numeric(house_test$MasVnrArea)
house_test$Electrical<- as.character(house_test$Electrical)
house_test$Electrical<- ifelse(is.na(house_test$Electrical),"SBrkr", house_test$Electrical)
house_test$Electrical<- as.factor(house_test$Electrical)
Median <- median(house_test$GarageYrBlt,na.rm = TRUE)
house_test$GarageYrBlt<- ifelse(is.na(house_test$GarageYrBlt),Median, house_test$GarageYrBlt)
house_test$GarageYrBlt<- as.numeric(house_test$GarageYrBlt)

#################### Common functions to impute unexpected NA's in test data #################
imputeContinuous <- function(feature)
{
  Median <- median(feature,na.rm = TRUE)
  feature<- ifelse(is.na(feature),Median, feature)
  feature<- as.numeric(feature)
  return (feature)
}

imputeCategorical <- function(feature)
{
  uniqv <- unique(feature)
  Mode <- uniqv[which.max(tabulate(match(feature, uniqv)))]
  feature <- impute(feature, Mode)
  feature<- as.character(feature)
  feature<- as.factor(feature)
}

ContinuousOrCategorical <- function(feature)
{
  if(is.numeric(feature))
  {
    feature <- imputeContinuous(feature)
  }
  else
  {
    feature <- imputeCategorical(feature)
  }
  return (feature)
}

#################### Building a basic model with all the independent variables #####################
m1 <- lm(house_train$SalePrice ~ ., data = house_train[,-80])
summary(m1)

########### Handling new NA's in test data #############
house_test$MSZoning <- ContinuousOrCategorical(house_test$MSZoning)
house_test$Utilities <- ContinuousOrCategorical(house_test$Utilities)
house_test$Exterior1st <- ContinuousOrCategorical(house_test$Exterior1st)
house_test$Exterior2nd <- ContinuousOrCategorical(house_test$Exterior2nd)
house_test$BsmtFinSF1 <- ContinuousOrCategorical(house_test$BsmtFinSF1)
house_test$BsmtFinSF2 <- ContinuousOrCategorical(house_test$BsmtFinSF2)
house_test$BsmtUnfSF <- ContinuousOrCategorical(house_test$BsmtUnfSF)
house_test$TotalBsmtSF <- ContinuousOrCategorical(house_test$TotalBsmtSF)
house_test$BsmtFullBath <- ContinuousOrCategorical(house_test$BsmtFullBath)
house_test$BsmtHalfBath <- ContinuousOrCategorical(house_test$BsmtHalfBath)
house_test$KitchenQual <- ContinuousOrCategorical(house_test$KitchenQual)
house_test$Functional <- ContinuousOrCategorical(house_test$Functional)
house_test$GarageCars <- ContinuousOrCategorical(house_test$GarageCars)
house_test$GarageArea <- ContinuousOrCategorical(house_test$GarageArea)
house_test$SaleType <- ContinuousOrCategorical(house_test$SaleType)

colnames(house_test)[colSums(is.na(house_test)) > 0]
p1 <- predict.lm(m1, newdata = house_test, na.action = na.pass )
house_test$SalePrice<- p1
sub <- data.frame()
sub<-house_test[,c(1,81)]
write.csv(sub, file = "submission1.csv")

############### Feature Selection ###############

########## Continuos variable analysis ###############
rc <- house_train

######### Finding highly correlated variables with dependent variable "Saleprice" ###############
oc <- abs(cor(rc[,iscontinuoscolumns],rc$SalePrice))
oc <- data.frame(oc)
colnames(oc)<- c("correlation")
oc$columns <- rownames(oc)
rownames(oc) <- NULL
oc <- oc[order(oc$correlation,decreasing = TRUE),]
print (oc)

############## List of variables with least correlation with dependent variable "Saleprice" ###############
# 0.26384335       LotArea
# 0.22712223  BsmtFullBath
# 0.21447911     BsmtUnfSF
# 0.16821315  BedroomAbvGr
# 0.13590737  KitchenAbvGr
# 0.12857796 EnclosedPorch
# 0.11144657   ScreenPorch
# 0.09240355      PoolArea
# 0.08428414    MSSubClass
# 0.07785589   OverallCond
# 0.04643225        MoSold
# 0.04458367    X3SsnPorch
# 0.02892259        YrSold
# 0.02560613  LowQualFinSF
# 0.02118958       MiscVal
# 0.01684415  BsmtHalfBath
# 0.01137812    BsmtFinSF2
# Multiple R Squared : 0.9196

###### Removing variables among this which increases multiple R Squared: ######

m2 <- lm(rc$SalePrice ~ ., data = rc[,-c(80,36,48,75,45,77,1,68,47)])
summary(m2)  

###### Checking Pearson correlation with removed variables with p values #######
colnames(rc)[c(80,36,48,75,45,77,1,68,47)]
cor.test(rc$BsmtHalfBath,rc$SalePrice)
cor.test(rc$BsmtFinSF2,rc$SalePrice)
cor.test(rc$MiscVal,rc$SalePrice)
cor.test(rc$LowQualFinSF,rc$SalePrice)
cor.test(rc$YrSold,rc$SalePrice)
cor.test(rc$MSSubClass,rc$SalePrice)
cor.test(rc$EnclosedPorch,rc$SalePrice)
cor.test(rc$BsmtFullBath,rc$SalePrice)

##### Making prediction using m2 ####
p2 <- predict.lm(m2, newdata = house_test, na.action = na.pass )
house_test$SalePrice<- p2
sub2 <- data.frame()
sub2 <- house_test[,c(1,81)]
write.csv(sub2, file = "submission2.csv")

############# categorical variable analysis using AVOVA ##############
############# For Continuous dependent variable and categorical independent variables we do feature analysis using ANOVA ############# 
# Analysis of variance (ANOVA) is a statistical technique that is used to check if the means of two or more groups are significantly different from 
# each other. ANOVA checks the impact of one or more factors by comparing the means of different samples.

isfactorcolumns
m3 <- lm(house_train$SalePrice ~ ., data = house_train[,isfactorcolumns])
anova(m3)

########## variables with least significance in f test #########
# Df     Sum Sq    Mean Sq  F value    Pr(>F)    
# Street           1 8.0927e+08 8.0927e+08   0.7860  0.375474    
# BsmtCond         3 3.3025e+09 1.1008e+09   1.0692  0.361111    
# BsmtFinType2     6 3.6318e+09 6.0530e+08   0.5879  0.740219    
# Electrical       4 2.6577e+09 6.6442e+08   0.6453  0.630234    
# Functional       6 1.0681e+10 1.7801e+09   1.7290  0.110751    
# PavedDrive       2 8.7702e+08 4.3851e+08   0.4259  0.653265    
# Fence            4 1.7307e+09 4.3267e+08   0.4202  0.794132    
# MiscFeature      4 1.0633e+09 2.6582e+08   0.2582  0.904727    


########### m1 performed well in test data than m2, so divide our train data as train and test and do remaining analysis ##################
########## For this we need to do dummy encoding of categorical data, to handle new factor levels in test data ################
############# Train the model and compare MAPE value of test data to get better model accuracy #############

################ Installing package for creating dummy variables #################
#install.packages("dummies")
house_train.new <- dummy.data.frame(house_train, sep = ".")
dim(house_train.new)

set.seed(123)
split <- sample.split(house_train.new,SplitRatio = 0.80)
train<- subset(house_train.new,split == TRUE)
test <- subset(house_train.new,split == FALSE)

################ Doing feature analysis by adding the above least f scored categorical variables(i.e. in ANOVA test) and comparing mape score ##############
# These features were removed one by one(say BsmtCond first, then BsmtCond,Fence next and so on, adding features one by one to removal list ), 
#ensuring that removal of any feature did'nt increase MAPE without removing anything.

train_names <- names(train)
m4_names <- setdiff(train_names,"SalePrice")
m4 <- lm(train$SalePrice ~ ., data = train[,m4_names])
p4 <- predict.lm(m4, newdata = test)
MAPE(y_pred = p4 , y_true = test$SalePrice)
#MAPE without removing anything : 0.1374622

name <- grep(pattern = "BsmtCond",x = names(train),value = T)
m4_names <- setdiff(m4_names,name)
grep(pattern = "BsmtCond",x = m4_names,value = T)

name <- grep(pattern = "Fence",x = names(train),value = T)
m4_names <- setdiff(m4_names,name)
grep(pattern = "Fence",x = m4_names,value = T)

name <- grep(pattern = "MiscFeature",x = names(train),value = T)
m4_names <- setdiff(m4_names,name)
grep(pattern = "MiscFeature",x = m4_names,value = T)

name <- grep(pattern = "Electrical",x = names(train),value = T)
m4_names <- setdiff(m4_names,name)
grep(pattern = "Electrical",x = m4_names,value = T)

name <- grep(pattern = "PavedDrive",x = names(train),value = T)
m4_names <- setdiff(m4_names,name)
grep(pattern = "PavedDrive",x = m4_names,value = T)

name <- grep(pattern = "BsmtFinType2",x = names(train),value = T)
m4_names <- setdiff(m4_names,name)
grep(pattern = "BsmtFinType2",x = m4_names,value = T)

name <- grep(pattern = "Functional",x = names(train),value = T)
m4_names <- setdiff(m4_names,name)
grep(pattern = "Functional",x = m4_names,value = T)

name <- grep(pattern = "Street",x = names(train),value = T)
m4_names <- setdiff(m4_names,name)
grep(pattern = "Street",x = m4_names,value = T)

m4 <- lm(train$SalePrice ~ ., data = train[,m4_names])
p4 <- predict.lm(m4, newdata = test)
MAPE(y_pred = p4 , y_true = test$SalePrice)

#MAPE after removing these eight variables : 0.1152548

#Now continuous variable analysis:
################################# Considering continuous independent variables which are least correlated with the target variable(0-0.3 correlation value) #######################

# 0.28410768      HalfBath
# 0.26384335       LotArea
# 0.22712223  BsmtFullBath
# 0.21447911     BsmtUnfSF
# 0.16821315  BedroomAbvGr
# 0.13590737  KitchenAbvGr
# 0.12857796 EnclosedPorch
# 0.11144657   ScreenPorch
# 0.09240355      PoolArea
# 0.08428414    MSSubClass
# 0.07785589   OverallCond
# 0.04643225        MoSold
# 0.04458367    X3SsnPorch
# 0.02892259        YrSold
# 0.02560613  LowQualFinSF
# 0.02118958       MiscVal
# 0.01684415  BsmtHalfBath
# 0.01137812    BsmtFinSF2

m4_names <- setdiff(m4_names, c("BsmtFinSF2","BsmtHalfBath","MiscVal","LowQualFinSF","YrSold","X3SsnPorch","MoSold","OverallCond","MSSubClass","PoolArea","ScreenPorch","EnclosedPorch","KitchenAbvGr","BedroomAbvGr","BsmtUnfSF","BsmtFullBath","LotArea","HalfBath") )
m4 <- lm(train$SalePrice ~ ., data = train[,m4_names])
p4 <- predict.lm(m4, newdata = test)
print (m4_names)
MAPE(y_pred = p4 , y_true = test$SalePrice)
# These features were removed one by one(say BsmtFinSF2 first, then BsmtFinSF2,BsmtHalfBath next and so on, adding features one by one to removal list ), 
#ensuring that removal of any feature did'nt increase MAPE without removing anything.
#0.1180879 after removing variables is less than, MAPE without removing anything : 0.1374622

########################################### Now train the model on entire train data #########################
m5_names <- colnames(house_train)
m5_names <- setdiff(m5_names,"SalePrice")
m5_names <- setdiff(m5_names, c("BsmtFinSF2","BsmtHalfBath","MiscVal","LowQualFinSF","YrSold","X3SsnPorch","MoSold","OverallCond","MSSubClass","PoolArea","ScreenPorch","EnclosedPorch","KitchenAbvGr","BedroomAbvGr","BsmtUnfSF","BsmtFullBath","LotArea","HalfBath") )
m5_names <- setdiff(m5_names, c("Street","BsmtCond","BsmtFinType2","Electrical","Functional","PavedDrive","Fence","MiscFeature"))
print (m5_names)
m5 <- lm(house_train$SalePrice ~ ., data = house_train[,m5_names])
p5 <- predict.lm(m5, newdata = house_test)

house_test$SalePrice<- p5
sub3 <- data.frame()
sub3 <- house_test[,c(1,81)]
write.csv(sub3, file = "submission3.csv")
