library("MASS") # For LDA function
library(caTools) # For split function

setwd("C:\\Users\\Home\\Desktop\\Saptha\\Machine-Learning\\Satander")
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_121')

#################### For increasing R's default number of rows to be printed ####################
options(max.print=1000000)

#################### Reading Training and Test Datasets ####################
set.seed(100)
train <- read.csv("train.csv",stringsAsFactors = FALSE)
test <- read.csv("test.csv",stringsAsFactors = FALSE)

#################### Viewing the datasets ####################
dim(train)
colnames(train)
head(train)
summary(train)
str(train)

#################### Data Preparation  #################### 

# Finding NA columns in the dataframe #
na_columns = colnames(train)[colSums(is.na(train)) > 0]
train$target <- as.factor(train$target)
ID <- train$ID_code
train$ID_code <- NULL

#################### LDA as a classifier #################### 

set.seed(123)
split <- sample.split(train,SplitRatio = 0.80)
training<- subset(train,split == TRUE)
testing <- subset(train,split == FALSE)

target <- training$target
m1 <- lda(target ~ .,data =  training)
predicted <- predict(object = m1,newdata = testing)
testing$Predict <- predicted$class

tab<-table(testing$target,testing$Predict)
acc <-sum(diag(tab))/sum(tab)

# Applying Everything to test data of kaggle  
target <- train$target
m1 <- lda(target ~ .,data =  train)
predicted <- predict(object = m1,newdata = test)
test$target <- predicted$class
table(predicted$class)

sub <- data.frame()
sub <- test[,c(1,202)]
write.csv(sub, file = "santander.csv")

#################### LDA as a dimensionality Reducer #################### 
# 1. Select features using LDA.
# 2. Apply Logistic Regression to selected features.

# Splitting the dataset into the Train, validation and Test set
# install.packages('caTools')

set.seed(999)
ss <- sample(1:3,size=nrow(train),replace=TRUE,prob=c(0.6,0.2,0.2))
training_set <- train[ss==1,]
testing_set <- train[ss==2,]
cv <- train[ss==3,]

# Prior probabilities of target #
table(training_set$target)
table(testing_set$target)
table(cv$target)

# our split has not affected the distribution of target, i.e the target in train, test and validation sets are in similar proportion 
# with original dataset .
target <- training_set$target
LDA <- lda(target ~ .,data =  training_set)

# Changing the dimension of dataset based on LDA:
train_target <- training_set$target
test_target <- testing_set$target
cv_target <- cv$target

# Apply LDA based feature scaling to train,cv and test data:
training_set <- as.matrix( training_set[,2:201] ) %*% LDA$scaling
testing_set <- as.matrix( testing_set[,2:201] ) %*% LDA$scaling
cv <- as.matrix( cv[,2:201] ) %*% LDA$scaling

# Converting data sets back to a dataframe to perform further operations:
training_set <- as.data.frame(training_set)
testing_set <- as.data.frame(testing_set)
cv <- as.data.frame(cv)

# Because we apply LDA only to independent features
training_set$target <- train_target 
testing_set$target <- test_target 
cv$target <- cv_target 

m2 <- glm(training_set$target~.,training_set,family="binomial")
predicted <- predict(m2,cv,type = "response")
th_f1score <- data.frame(matrix(ncol = 2, nrow = 0))

for (i in seq(0.3,0.9,0.1)){
  cv$Predict <- ifelse(predicted>=i,1,0)
  tab<-table(cv$target,cv$Predict)
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

predicted <- predict(m2,testing_set,type = "response")
testing_set$Predict <- ifelse(predicted>=thresold,1,0)
tab<-table(testing_set$target,testing_set$Predict)
acc <-sum(diag(tab))/sum(tab)

#################### Applying Everything to test data of kaggle, thresold picked based on F1-Score #################### 
target <- train$target
ID <- test$ID_code
LDA <- lda(target ~ .,data =  train)

train_target <- train$target

train <- as.matrix( train[,2:201] ) %*% LDA$scaling
test <- as.matrix( test[,2:201] ) %*% LDA$scaling

train <- as.data.frame(train)
test <- as.data.frame(test)

train$target <- train_target 
test$ID_code <- ID

m2 <- glm(train$target~.,train,family="binomial")
predicted <- predict(m2,test,type = "response")
test$target <- ifelse(predicted>=thresold,1,0)

sub <- data.frame()
sub <- test[,c(2,3)]
write.csv(sub, file = "santander1.csv")

