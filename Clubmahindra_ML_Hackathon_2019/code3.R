setwd("C:\\Users\\Home\\Desktop\\Saptha\\Machine-Learning\\clubmahindra")
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_121')

library('eeptools')
library("MASS") 
library(caTools)
library('randomForest')
library("pROC") #For ROC curve
library('ggplot2')

options(max.print=1000000)

set.seed(100)
train <- read.csv("train.csv",stringsAsFactors = FALSE)
test <- read.csv("test.csv",stringsAsFactors = FALSE)
str(train)

na_columns = colnames(train)[colSums(is.na(train)) > 0]
non_na_columns = setdiff(colnames(train),na_columns)

# > na_columns
# [1] "season_holidayed_code" "state_code_residence" 

na_columns = colnames(test)[colSums(is.na(test)) > 0]
non_na_columns = setdiff(colnames(test),na_columns)

################# Generating interaction terms #################  

train$room_type_booked_booking_type_code <- paste(as.character(train$room_type_booked_code),as.character(train$booking_type_code), sep = "")
train$room_type_booked_booking_type_code <- as.factor(train$room_type_booked_booking_type_code)
test$room_type_booked_booking_type_code <- paste(as.character(test$room_type_booked_code),as.character(test$booking_type_code), sep = "")
test$room_type_booked_booking_type_code <- as.factor(test$room_type_booked_booking_type_code)

train$resort_type_booking_type_code <- paste(as.character(train$resort_type_code),as.character(train$booking_type_code), sep = "")
train$resort_type_booking_type_code <- as.factor(train$resort_type_booking_type_code)
test$resort_type_booking_type_code <- paste(as.character(test$resort_type_code),as.character(test$booking_type_code), sep = "")
test$resort_type_booking_type_code <- as.factor(test$resort_type_booking_type_code)

train$resort_region_code_member_age_buckets <- paste(as.character(train$resort_region_code),as.character(train$member_age_buckets), sep = "")
train$resort_region_code_member_age_buckets <- as.factor(train$resort_region_code_member_age_buckets)
test$resort_region_code_member_age_buckets <- paste(as.character(test$resort_region_code),as.character(test$member_age_buckets), sep = "")
test$resort_region_code_member_age_buckets <- as.factor(test$resort_region_code_member_age_buckets)

train$channel_main_product_code <- paste(as.character(train$channel_code),as.character(train$main_product_code), sep = "")
train$channel_main_product_code <- as.factor(train$channel_main_product_code)
test$channel_main_product_code <- paste(as.character(test$channel_code),as.character(test$main_product_code), sep = "")
test$channel_main_product_code <- as.factor(test$channel_main_product_code)

train$state_code_resort_residence <- paste(as.character(train$state_code_residence),as.character(train$state_code_resort), sep = "")
train$state_code_resort_residence <- as.factor(train$state_code_resort_residence)
test$state_code_resort_residence <- paste(as.character(test$state_code_residence),as.character(test$state_code_resort), sep = "")
test$state_code_resort_residence <- as.factor(test$state_code_resort_residence)

reservation <- as.factor(train$reservation_id)
str(reservation)
train$reservation_id <- NULL
reservationTest <- as.factor(test$reservation_id)
str(reservationTest)
test$reservation_id <- NULL
member <- as.factor(train$memberid)
str(member)
train$memberid <- NULL
test$memberid <- NULL

booking_checkin_diff <- abs(as.numeric(as.Date(train$checkin_date,"%d/%m/%Y") - as.Date(train$booking_date,"%d/%m/%Y")))
checkin_checkout_diff <- abs(as.numeric(as.Date(train$checkout_date,"%d/%m/%Y") - as.Date(train$checkin_date,"%d/%m/%Y")))
train$booking_checkin_diff <- booking_checkin_diff
train$checkin_checkout_diff <- checkin_checkout_diff
booking <- train$booking_date
train$booking_date <- NULL
checkin <- train$checkin_date
train$checkin_date <- NULL
checkout <- train$checkout_date
train$checkout_date <- NULL
summary(train)

booking_checkin_diff <- abs(as.numeric(as.Date(test$checkin_date,"%d/%m/%Y") - as.Date(test$booking_date,"%d/%m/%Y")))
checkin_checkout_diff <- abs(as.numeric(as.Date(test$checkout_date,"%d/%m/%Y") - as.Date(test$checkin_date,"%d/%m/%Y")))
test$booking_checkin_diff <- booking_checkin_diff
test$checkin_checkout_diff <- checkin_checkout_diff
test$booking_date <- NULL
test$checkout_date <- NULL
test$checkin_date <- NULL

train$channel_code <- as.factor(train$channel_code)
train$main_product_code <- as.factor(train$main_product_code)

test$channel_code <- as.factor(test$channel_code)
test$main_product_code <- as.factor(test$main_product_code)

############### winzortization to remove outliers ##############
boxplot(train$numberofadults)
hist(train$numberofadults, freq = TRUE)
IQR(train$numberofadults)
median(train$numberofadults)
outlierRange <- 4.000 + (1.5*2)
outlier <- train[train$numberofadults>7, ]
summary(outlier)
quantile(train$numberofadults, c(.01,.99))
hist(train$amount_spent_per_room_night_scaled, freq = TRUE)
hist(outlier$amount_spent_per_room_night_scaled, freq = TRUE)
train$numberofadults<- ifelse(train$numberofadults>10,10, train$numberofadults)
train$numberofadults<- as.numeric(train$numberofadults)
train$numberofadults<- ifelse(train$numberofadults<2,2, train$numberofadults)
train$numberofadults<- as.numeric(train$numberofadults)

quantile(test$numberofadults, c(.01,.99))
test$numberofadults<- ifelse(test$numberofadults>10,10, test$numberofadults)
test$numberofadults<- as.numeric(test$numberofadults)
test$numberofadults<- ifelse(test$numberofadults<2,2, test$numberofadults)
test$numberofadults<- as.numeric(test$numberofadults)

boxplot(train$numberofchildren)
hist(train$numberofchildren, freq = TRUE)
IQR(train$numberofchildren)
median(train$numberofchildren)
quantile(train$numberofchildren, c(.01,.99))
outlier <- train[train$numberofchildren>2, ]
summary(outlier)
dim(outlier)
hist(train$amount_spent_per_room_night_scaled, freq = TRUE)
hist(outlier$amount_spent_per_room_night_scaled, freq = TRUE)
train$numberofchildren<- ifelse(train$numberofchildren>3,3, train$numberofchildren)
train$numberofchildren<- as.numeric(train$numberofchildren)
train$numberofchildren<- ifelse(train$numberofchildren<0,0, train$numberofchildren)
train$numberofchildren<- as.numeric(train$numberofchildren)

quantile(test$numberofchildren, c(.01,.99))
test$numberofchildren<- ifelse(test$numberofchildren>3,3, test$numberofchildren)
test$numberofchildren<- as.numeric(test$numberofchildren)
test$numberofchildren<- ifelse(test$numberofchildren<0,0, test$numberofchildren)
test$numberofchildren<- as.numeric(test$numberofchildren)

boxplot(train$checkin_checkout_diff)
hist(train$checkin_checkout_diff, freq = TRUE)
quantile(train$checkin_checkout_diff, c(.01,.99))
outlier <- train[train$checkin_checkout_diff>4, ]
summary(outlier)
train$checkin_checkout_diff<- ifelse(train$checkin_checkout_diff>6,6, train$checkin_checkout_diff)
train$checkin_checkout_diff<- as.numeric(train$checkin_checkout_diff)
train$checkin_checkout_diff<- ifelse(train$checkin_checkout_diff<1,1, train$checkin_checkout_diff)
train$checkin_checkout_diff<- as.numeric(train$checkin_checkout_diff)

quantile(test$checkin_checkout_diff, c(.01,.99))
test$checkin_checkout_diff<- ifelse(test$checkin_checkout_diff>6,6, test$checkin_checkout_diff)
test$checkin_checkout_diff<- as.numeric(test$checkin_checkout_diff)
test$checkin_checkout_diff<- ifelse(test$checkin_checkout_diff<1,1, test$checkin_checkout_diff)
test$checkin_checkout_diff<- as.numeric(test$checkin_checkout_diff)

boxplot(train$booking_checkin_diff)
hist(train$booking_checkin_diff, freq = TRUE)
quantile(train$booking_checkin_diff, c(.01,.99))
outlier <- train[train$booking_checkin_diff>109, ]
summary(outlier)
train$booking_checkin_diff <- ifelse(train$booking_checkin_diff>118,118, train$booking_checkin_diff)
train$booking_checkin_diff<- as.numeric(train$booking_checkin_diff)
train$booking_checkin_diff<- ifelse(train$booking_checkin_diff<0,0, train$booking_checkin_diff)
train$booking_checkin_diff<- as.numeric(train$booking_checkin_diff)

quantile(test$booking_checkin_diff, c(.01,.99))
test$booking_checkin_diff <- ifelse(test$booking_checkin_diff>118,118, test$booking_checkin_diff)
test$booking_checkin_diff<- as.numeric(test$booking_checkin_diff)
test$booking_checkin_diff<- ifelse(test$booking_checkin_diff<0,0, test$booking_checkin_diff)
test$booking_checkin_diff<- as.numeric(test$booking_checkin_diff)

boxplot(train$roomnights)
hist(train$roomnights, freq = TRUE)
quantile(train$roomnights, c(.01,.99))
outlier <- train[train$roomnights>6, ]
summary(outlier)
train$roomnights <- ifelse(train$roomnights>12,12, train$roomnights)
train$roomnights<- as.numeric(train$roomnights)
train$roomnights<- ifelse(train$roomnights<1,1, train$roomnights)
train$roomnights<- as.numeric(train$roomnights)

quantile(test$roomnights, c(.01,.99))
test$roomnights <- ifelse(test$roomnights>12,12, test$roomnights)
test$roomnights<- as.numeric(test$roomnights)
test$roomnights<- ifelse(test$roomnights<1,1, test$roomnights)
test$roomnights<- as.numeric(test$roomnights)

boxplot(train$total_pax)
hist(train$total_pax, freq = TRUE)
quantile(train$total_pax, c(.01,.99))
train$total_pax <- ifelse(train$total_pax>7,7, train$total_pax)
train$total_pax<- as.numeric(train$total_pax)
train$total_pax<- ifelse(train$total_pax<1,1, train$total_pax)
train$total_pax<- as.numeric(train$total_pax)

quantile(test$total_pax, c(.01,.99))
test$total_pax <- ifelse(test$total_pax>7,7, test$total_pax)
test$total_pax<- as.numeric(test$total_pax)
test$total_pax<- ifelse(test$total_pax<1,1, test$total_pax)
test$total_pax<- as.numeric(test$total_pax)

################ Identifying categorical columns and converting them to factors ################ 

personid <- as.factor(train$persontravellingid)
summary(personid)
train$persontravellingid <- as.factor(train$persontravellingid)
train$resort_region_code <- as.factor(train$resort_region_code)
train$resort_type_code <- as.factor(train$resort_type_code)
train$room_type_booked_code <- as.factor(train$room_type_booked_code)
train$season_holidayed_code <- as.factor(train$season_holidayed_code)
train$state_code_resort <- as.factor(train$state_code_resort)
stateid <- as.factor(train$state_code_residence)
summary(stateid)
train$state_code_residence <- as.factor(train$state_code_residence)
train$member_age_buckets <- as.factor(train$member_age_buckets)
train$booking_type_code <- as.factor(train$booking_type_code)
train$cluster_code <- as.factor(train$booking_type_code)
train$reservationstatusid_code <- as.factor(train$reservationstatusid_code)
train$resort_id <- as.factor(train$resort_id)

test$persontravellingid <- as.factor(test$persontravellingid)
test$resort_region_code <- as.factor(test$resort_region_code)
test$resort_type_code <- as.factor(test$resort_type_code)
test$room_type_booked_code <- as.factor(test$room_type_booked_code)
test$season_holidayed_code <- as.factor(test$season_holidayed_code)
test$state_code_resort <- as.factor(test$state_code_resort)
test$state_code_residence <- as.factor(test$state_code_residence)
test$member_age_buckets <- as.factor(test$member_age_buckets)
test$booking_type_code <- as.factor(test$booking_type_code)
test$cluster_code <- as.factor(test$booking_type_code)
test$reservationstatusid_code <- as.factor(test$reservationstatusid_code)
test$resort_id <- as.factor(test$resort_id)

#################### Generating new feature to indicate NAN's #################### 
train$season_holidayed_code_null <- ifelse(is.na(train$season_holidayed_code),TRUE, FALSE)
test$season_holidayed_code_null <- ifelse(is.na(test$season_holidayed_code),TRUE, FALSE)
train$state_code_residence_null <- ifelse(is.na(train$state_code_residence),TRUE, FALSE)
test$state_code_residence_null <- ifelse(is.na(test$state_code_residence),TRUE, FALSE)

########## Handling Missing values ########## 

summary(train$season_holidayed_code)
ggplot(train,aes(x = train$season_holidayed_code)) + geom_bar(color = "black" , aes(fill = train$season_holidayed_code)) + labs(title = "season_holidayed_code")
train$season_holidayed_code<- as.character(train$season_holidayed_code)
# Imputing with mode 
train$season_holidayed_code<- ifelse(is.na(train$season_holidayed_code),"2", train$season_holidayed_code)
train$season_holidayed_code<- as.factor(train$season_holidayed_code)
summary(train$season_holidayed_code)

summary(train$state_code_residence)
ggplot(train,aes(x = train$state_code_residence)) + geom_bar(color = "black" , aes(fill = train$state_code_residence)) + labs(title = "state_code_residence")
train$state_code_residence<- as.character(train$state_code_residence)
# Imputing with mode 
train$state_code_residence<- ifelse(is.na(train$state_code_residence),"8", train$state_code_residence)
train$state_code_residence<- as.factor(train$state_code_residence)
summary(train$state_code_residence)

# Handling single row category
train$reservationstatusid_code<- as.character(train$reservationstatusid_code)
# Imputing with mode 
train$reservationstatusid_code<- ifelse(train$reservationstatusid_code == 'D',"A", train$reservationstatusid_code)
train$reservationstatusid_code<- as.factor(train$reservationstatusid_code)
summary(train$reservationstatusid_code)


summary(test$season_holidayed_code)
ggplot(test,aes(x = test$season_holidayed_code)) + geom_bar(color = "black" , aes(fill = test$season_holidayed_code)) + labs(title = "season_holidayed_code")
test$season_holidayed_code<- as.character(test$season_holidayed_code)
# Imputing with mode 
test$season_holidayed_code<- ifelse(is.na(test$season_holidayed_code),"2", test$season_holidayed_code)
test$season_holidayed_code<- as.factor(test$season_holidayed_code)
summary(test$season_holidayed_code)

summary(test$state_code_residence)
ggplot(test,aes(x = test$state_code_residence)) + geom_bar(color = "black" , aes(fill = test$state_code_residence)) + labs(title = "state_code_residence")
test$state_code_residence<- as.character(test$state_code_residence)
# Imputing with mode 
test$state_code_residence<- ifelse(is.na(test$state_code_residence),"8", test$state_code_residence)
test$state_code_residence<- as.factor(test$state_code_residence)
summary(test$state_code_residence)

############## Finding Categorical variables ###############
isfactor <-train[,sapply(train,is.factor)]
isfactorcolumns <- colnames(isfactor)

################### Finding Continuos variables ############
iscontinuoscolumns <- setdiff(colnames(train),isfactorcolumns)
iscontinuoscolumns <- iscontinuoscolumns[-5]
iscontinuos <- train[,iscontinuoscolumns]

summary(train)
dim(train)

summary(test)
dim(test)

################### More Feature Generation ###################  
train$MoreStayed <- ifelse(train$roomnights < train$checkin_checkout_diff, 1 , 0)
train$MoreStayed <- as.factor(train$MoreStayed)

train$LessStayed <- ifelse(train$roomnights > train$checkin_checkout_diff, 1 , 0)
train$LessStayed <- as.factor(train$LessStayed)

train$EqualStayed <- ifelse(train$roomnights == train$checkin_checkout_diff, 1 , 0)
train$EqualStayed <- as.factor(train$EqualStayed)

test$MoreStayed <- ifelse(test$roomnights < test$checkin_checkout_diff, 1 , 0)
test$MoreStayed <- as.factor(test$MoreStayed)

test$LessStayed <- ifelse(test$roomnights > test$checkin_checkout_diff, 1 , 0)
test$LessStayed <- as.factor(test$LessStayed)

test$EqualStayed <- ifelse(test$roomnights == test$checkin_checkout_diff, 1 , 0)
test$EqualStayed <- as.factor(test$EqualStayed)

library(dummies)
train.new <- dummy.data.frame(train, sep = ".")
dim(train.new)
test.new <- dummy.data.frame(test, sep = ".")
dim(test.new)

train.new$state_code_resort_residence.NA10 <- NULL                                                  
train.new$state_code_resort_residence.NA11 <- NULL                                                 
train.new$state_code_resort_residence.NA13 <- NULL                                                  
train.new$state_code_resort_residence.NA2 <- NULL                                                  
train.new$state_code_resort_residence.NA3 <- NULL                                                   
train.new$state_code_resort_residence.NA4 <- NULL                                                   
train.new$state_code_resort_residence.NA5 <- NULL                                                  
train.new$state_code_resort_residence.NA6 <- NULL                                                  
train.new$state_code_resort_residence.NA7 <- NULL                                                  
train.new$state_code_resort_residence.NA9 <- NULL
train.new$state_code_resort_residence.1113 <- NULL
train.new$state_code_resort_residence.1313 <- NULL

m1 <- lm(train.new$amount_spent_per_room_night_scaled ~ ., data = train.new)
p1 <- predict.lm(m1, newdata = test.new)

test$reservation_id <- reservationTest
test$amount_spent_per_room_night_scaled <- p1
sub3 <- data.frame()
sub3 <- test[,c(30,31)]
write.csv(sub3, file = "submission14.csv")
test$reservation_id <- NULL
test$amount_spent_per_room_night_scaled <- NULL

summary(m1)
# Residual standard error: 0.9955 on 341281 degrees of freedom
# Multiple R-squared:  0.1663,	Adjusted R-squared:  0.166 
# F-statistic: 479.6 on 142 and 341281 DF,  p-value: < 2.2e-16