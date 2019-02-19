library("caTools") #For sample.split function
library("pROC") #For ROC curve
require("ggplot2")

setwd("C:\\Users\\Home\\Desktop\\Saptha")
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_121')

#################### For increasing R's default number of rows to be printed ####################
options(max.print=1000000)

#################### Reading Training and Test Datasets ####################
set.seed(100)
house_train <- read.csv("training.csv",stringsAsFactors = FALSE)
house_test <- read.csv("testing.csv",stringsAsFactors = FALSE)

#################### Viewing the datasets ####################
dim(house_train)
colnames(house_train)
head(house_train)
summary(house_train)
str(house_train)

#################### Removing PassengerID column ####################
#################### It is unique for every record, so they are not useful in model building ####################

Id <- house_train[,c('PassengerId')]
house_train$PassengerId <- NULL

#################### Removing name and ticket columns ####################
#################### Name of a passenger and ticket no. has nothing to with their survival chance ####################

name <- house_train[,c('Name')]
ticket <- house_train[,c('Ticket')]
house_train$Name <- NULL
house_train$Ticket <- NULL

#################### Meaning of variables in titanic dataset ####################

# survival - Survival (0 = No; 1 = Yes)
# Pclass - Passenger Class (1 = 1st; 2 = 2nd; 3 = 3rd)
# name - Name
# sex - Sex
# age - Age
# sibsp - Number of Siblings/Spouses Aboard
# parch - Number of Parents/Children Aboard
# ticket - Ticket Number
# fare - Passenger Fare
# cabin - Cabin
# embarked - Port of Embarkation (C = Cherbourg; Q = Queenstown; S = Southampton)

#################### Properly identifying factor columns and making them as factors, we took stringsAsFactors = FALSE in creating ####################
#################### data.frame as R wrongly identified factors in this case ####################

house_train$Survived <- as.factor(house_train$Survived)
house_train$Pclass <- as.factor(house_train$Pclass)
house_train$Sex <- as.factor(house_train$Sex)
house_train$Embarked <- as.factor(house_train$Embarked)

# Doing same for test data
house_test$Pclass <- as.factor(house_test$Pclass)
house_test$Sex <- as.factor(house_test$Sex)
house_test$Embarked <- as.factor(house_test$Embarked)

#################### Finding NA columns in the dataframe #################### 
na_columns = colnames(house_train)[colSums(is.na(house_train)) > 0]

#################### Imputing NA column #################### 

############## house_train$Age is the only NA column #################
summary(house_train$Age)
hist(house_train$Age, freq = TRUE )
Mean <- mean(house_train$Age,na.rm = TRUE)
Median <- median(house_train$Age,na.rm = TRUE)
impute <- round((Mean + Median)/2)
house_train$Age<- ifelse(is.na(house_train$Age),impute, house_train$Age)
house_train$Age<- as.numeric(house_train$Age)
hist(house_train$Age, freq = TRUE )
sum(is.na(house_train$Age))
summary(house_train$Age)

# Doing same for test data
Mean <- mean(house_test$Age,na.rm = TRUE)
Median <- median(house_test$Age,na.rm = TRUE)
impute <- round((Mean + Median)/2)
house_test$Age<- ifelse(is.na(house_test$Age),impute, house_test$Age)
house_test$Age<- as.numeric(house_test$Age)

#################### Removing the rows with "Embarked" as none of C = Cherbourg; Q = Queenstown; S = Southampton factors as we have just two such rows #################### 
str(house_train$Embarked)
#################### Querying the dataframe to find rows with "Embarked" as none of C = Cherbourg; Q = Queenstown; S = Southampton factor levels #################### 
empty_rows <- subset(house_train, house_train$Embarked == "")
# Doing same for test data
empty_rows <- subset(house_test, house_test$Embarked == "")

#     Survived Pclass                                      Name    Sex Age SibSp Parch Ticket Fare Cabin Embarked
# 62         1      1                       Icard, Miss. Amelie female  38     0     0 113572   80   B28         
# 830        1      1 Stone, Mrs. George Nelson (Martha Evelyn) female  62     0     0 113572   80   B28  
# None in test data

house_train <- house_train[-c(62,830),]

#################### Converting to chr and reconverting to factor to ensure that factor has just three levels, as it shows four #################### 
#################### levels even after removing the above two rows due to initial assignment #################### 
house_train$Embarked <- as.character(house_train$Embarked)
house_train[,c('Embarked')] <- as.factor(house_train[,c('Embarked')])

#################### Adding new features #################### 
house_train$family <- house_train$SibSp + house_train$Parch
# Removing parch and sibsp as we already added them to family 
parch <- house_train$Parch
sibsp <- house_train$SibSp
house_train$Parch <- NULL
house_train$SibSp <- NULL

# Doing same for test data
house_test$family <- house_test$SibSp + house_test$Parch
house_test$has_cabin <- ifelse(house_test$Cabin == "", "No","Yes") 
house_test$has_cabin <- as.factor(house_test$has_cabin)

house_train$has_cabin <- ifelse(house_train$Cabin == "", "No","Yes") 
house_train$has_cabin <- as.factor(house_train$has_cabin)
cabin <- house_train$Cabin
# Removing cabin
house_train$Cabin <- NULL

set.seed(100)
#################### Splitting train data as train and test #################### 
house_train.new <- house_train
split <- sample.split(house_train.new,SplitRatio = 0.80)
train<- subset(house_train.new,split == TRUE)
test <- subset(house_train.new,split == FALSE)

#################### Model building #################### 
m1 <- glm(train$Survived~.,train,family="binomial")
summary(m1)  
# Null deviance: 889.25  on 666  degrees of freedom
# Residual deviance: 575.27  on 657  degrees of freedom
# AIC: 595.27

# Check how the output class is distributed, to see if it is skewed, i.e when one class is one class is over-represented in the data set 
ggplot(test,aes(x = test$Survived)) + geom_bar(color = "black" , aes(fill = test$Survived)) + labs(title = "Survived")
survivalrate <-table(test$Survived)[2]/(table(test$Survived)[1]+table(test$Survived)[2])
deathrate <- table(test$Survived)[1]/(table(test$Survived)[1]+table(test$Survived)[2])
print (paste("Survival rate in test data is",round(survivalrate*100),"%"))
print (paste("Death rate in test data is",round(deathrate*100),"%"))

#################### Making prediction #################### 
predicted <- predict(m1,test,type = "response")
test$Predict <- ifelse(predicted>=0.5,1,0)
table(test$Survived,test$Predict)
table(test$Survived)
tab<-table(test$Survived,test$Predict)

#################### Calculating metrics from confusion matrix ####################

# For skewed classes, not just accuracy is important, but Precision(P) and Recall(R) should also be high.
# So we choose thresold having highest F1 score, F1 score = 2(PR)/(P+R)
# Of all people who are predicted as survived, what percentage of people actually survived, for this we go for precision, precision = TP/(TP+FP)
# Of all people who actually survived, what percentage of people are predicted as survived, for this we go for recall, precision = TP/(TP+FN)

accuracy <-sum(diag(tab))/sum(tab)
precision <- tab[2+2]/(tab[1,2]+tab[2,2])
recall <- tab[2+2]/(tab[2,1]+tab[2+2])
f1_score <- (2*precision*recall)/(precision+recall)
print (c(accuracy,precision,recall,f1_score))
# 0.7612613 0.6875000 0.6626506 0.6748466 for a thresold of 0.5

#################### Picking correct thresold based on f1-score #################### 
# 1. Lets divide train data as train,cross-validate(CV) and test data 
# 2. calculate the f1-score for different thresolds in cv data and 
# 3. Make prediction in test data, by taking thresold that gave high f1-score in cv

#################### Splitting train data as train, cv and test #################### 
house_train.new <- house_train

###### Step 1 ###### 
set.seed(999)
ss <- sample(1:3,size=nrow(house_train.new),replace=TRUE,prob=c(0.6,0.2,0.2))
training <- house_train.new[ss==1,]
testing <- house_train.new[ss==2,]
cv <- house_train.new[ss==3,]

###### Step 2 ###### 
#################### Build model and make prediction #################### 
m2 <- glm(training$Survived~.,training,family="binomial")
predicted <- predict(m2,cv,type = "response")
th_f1score <- data.frame(matrix(ncol = 2, nrow = 0))

for (i in seq(0.3,0.9,0.1)){
  cv$Predict <- ifelse(predicted>=i,1,0)
  tab<-table(cv$Survived,cv$Predict)
  acc <-sum(diag(tab))/sum(tab)
  prec <- tab[2+2]/(tab[1,2]+tab[2,2])
  rec <- tab[2+2]/(tab[2,1]+tab[2+2])
  f1score <- (2*prec*rec)/(prec+rec)
  new_row <- list(i,acc,prec,rec,f1score)
  th_f1score <- rbind(new_row,th_f1score)
}

colnames(th_f1score) <- c("Thresold","Accuracy","Precision","Recall","F1score")
th_f1score <- th_f1score[order(th_f1score$F1score),]
thresold <- th_f1score[7,1]

###### Step 3 ######
predicted <- predict(m2,testing,type = "response")
testing$Predict <- ifelse(predicted>=thresold,1,0)
tab<-table(testing$Survived,testing$Predict)
acc <-sum(diag(tab))/sum(tab)

#################### Calculating ROC on the test data we used to predict F1-Score #################### 
# response - Actual Values in 0's(control) and 1's(Cases)
# predictor - Predicted Probabilities
rocobj <- roc(testing$Survived,predicted)
plot(rocobj)
# The ROC curve plots out the sensitivity and specificity for every possible decision rule cutoff between 0 and 1 for a model
# Get best cutoff, based on "Youdan's index" from ROC curve
# Youden's index is often used in conjunction with receiver operating characteristic (ROC) analysis.[2] 
# Youden's index is defined for all points of an ROC curve, and the maximum value of the index may be used as a criterion for selecting 
# the optimum cut-off point.
# Youden's J statistic : J = sensitivity + specificity - 1
coords(rocobj, x="best", input="threshold", best.method="youden")

# Thresold in F1-score analyis is 0.3, in ROC curve analysis is 0.3618195

# coords(roc, x="best", input="threshold", best.method="youden")
# threshold specificity sensitivity 
# 0.3618195   0.8317757   0.6714286 
# thresold (F1-score analysis) 
# [1] 0.3

# AUC - Area Under Curve is used for comparing different models, auc value of closer to one, the best
# AUC - ROC curve is a performance measurement for classification problem at various thresholds settings. 
# ROC is a probability curve and AUC represents degree or measure of separability. It tells how much model is capable of distinguishing between classes. 
# Higher the AUC, better the model is at predicting 0s as 0s and 1s as 1s.
auc(testing$Survived,predicted)

#################### Applying Everything to test data of kaggle, thresold picked based on F1-Score #################### 
m <- glm(house_train$Survived~.,house_train,family="binomial")
summary(m)
predicted <- predict(m,house_test,type = "response")
house_test$Survived <- ifelse(predicted>=thresold,1,0)
ggplot(house_test,aes(x = house_test$Survived)) + geom_bar(color = "black" , aes(fill = house_test$Survived)) + labs(title = "Survived")
ggplot(house_train,aes(x = house_train$Survived)) + geom_bar(color = "black" , aes(fill = house_train$Survived)) + labs(title = "Survived")

sub <- data.frame()
sub <- house_test[,c(1,14)]
write.csv(sub, file = "titanic.csv")
# When predicted with this thresold kaggle gave same accuracy as in test data, 73% #




