###########Load bostin data 
require("MASS")
data(Boston)
help(Boston)
summary(Boston)
hist(Boston$medv)
summary(Boston$medv)
###################75% of data are less than 3rd quartile
###########
Boston$medv_class<-ifelse(Boston$medv >=25,1,0)
summary(Boston)
hist(Boston$medv_class)
plot(Boston$medv_class,Boston$lstat)
plot(Boston$medv,Boston$lstat)
plot(Boston$medv_class~Boston$lstat)
###########Create a sample model
model=lm(medv_class~lstat,data=Boston)
summary(model)
##################
Boston$predict<-predict.lm(model,Boston)
Boston$prob_class_1<-exp(Boston$predict)/(1+exp(Boston$predict))
View(Boston)
names(Boston)
plot(Boston$prob_class_1,Boston$lstat)
#################Expected probablity of a high value property
####value of lstat vs probablity of housing greater than 25k
###########Now build a logistic regression model
colnames(Boston)
model<-glm(formula=medv_class~.-medv,data=Boston[c(0:250),],family=binomial("logit"))
out <- predict.glm(model,newdata=Boston[c(250:506),],type="response")
predicted.classes <- ifelse(out > 0.8, "pos", "neg")
summary(Boston$medv_class)
class(Boston$medv_class)

?predict.glm
