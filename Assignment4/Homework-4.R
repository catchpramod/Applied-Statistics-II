library(MASS)
library(class)

# 10
data("Weekly", package = "ISLR")
head(Weekly)

weekly.train <- Weekly[!(Weekly$Year %in% c('2009','2010')),]
weekly.test <- Weekly[Weekly$Year %in% c('2009','2010'),]

#d

weekly.fit.train <- glm(Direction ~ Lag2, family=binomial, data=weekly.train)
summary(weekly.fit.train)
weekly.test.prob <- predict(weekly.fit.train,weekly.test,type="response")

weekly.test.pred <- rep("Down",nrow(weekly.test))
weekly.test.pred[weekly.test.prob > 0.50]="Up"
table(weekly.test.pred,weekly.test$Direction)
mean(weekly.test.pred==weekly.test$Direction)

#e
weekly.fit.lda <- lda(Direction ~ Lag2, data = weekly.train)
weekly.fit.lda
weekly.pred.lda <- predict(weekly.fit.lda, weekly.test)
table(weekly.pred.lda$class, weekly.test$Direction)
mean(weekly.pred.lda$class==weekly.test$Direction)

#f
weekly.fit.qda <- qda(Direction ~ Lag2, data = weekly.train)
weekly.fit.qda
weekly.pred.qda <- predict(weekly.fit.qda, weekly.test)
table(weekly.pred.qda$class, weekly.test$Direction)
mean(weekly.pred.qda$class==weekly.test$Direction)

#g
train.X <- as.matrix(weekly.train$Lag2)
test.X <- as.matrix(weekly.test$Lag2)
train.Direction <- weekly.train$Direction

set.seed (1)
knn.pred=knn(train.X,test.X,train.Direction ,k=1)
table(knn.pred,weekly.test$Direction)
mean(knn.pred==weekly.test$Direction)

#i
#logistic regression with lag1, lag2 interaction
print("logistic regression with lag1, lag2 interaction")
weekly.fit.train <- glm(Direction ~ Lag2*Lag1, family=binomial, data=weekly.train)
weekly.fit.train
weekly.test.prob <- predict(weekly.fit.train,weekly.test,type="response")
weekly.test.pred <- rep("Down",nrow(weekly.test))
weekly.test.pred[weekly.test.prob > 0.50]="Up"
table(weekly.test.pred,weekly.test$Direction)
mean(weekly.test.pred==weekly.test$Direction)


#lda with lag1, lag2 interaction
print("lda with lag1, lag2 interaction")
weekly.fit.lda <- lda(Direction ~ Lag2*Lag1, data = weekly.train)
weekly.fit.lda
weekly.pred.lda <- predict(weekly.fit.lda, weekly.test)
table(weekly.pred.lda$class, weekly.test$Direction)
mean(weekly.pred.lda$class==weekly.test$Direction)

#qda with lag1, lag2 interaction
weekly.fit.qda <- qda(Direction ~ Lag2*Lag1, data = weekly.train)
weekly.fit.qda
weekly.pred.qda <- predict(weekly.fit.qda, weekly.test)
table(weekly.pred.qda$class, weekly.test$Direction)
mean(weekly.pred.qda$class==weekly.test$Direction)

#KNN with K=3
print("KNN with K=3")
train.X <- as.matrix(weekly.train$Lag2)
test.X <- as.matrix(weekly.test$Lag2)
train.Direction <- weekly.train$Direction

set.seed (1)
knn.pred=knn(train.X,test.X,train.Direction ,k=3)
table(knn.pred,weekly.test$Direction)
mean(knn.pred==weekly.test$Direction)

#KNN with K=4
set.seed (1)
knn.pred=knn(train.X,test.X,train.Direction ,k=4)
table(knn.pred,weekly.test$Direction)
mean(knn.pred==weekly.test$Direction)

#KNN with K=5
set.seed (1)
knn.pred=knn(train.X,test.X,train.Direction ,k=5)
table(knn.pred,weekly.test$Direction)
mean(knn.pred==weekly.test$Direction)



#11


data("Auto", package = "ISLR")

head(Auto)
Auto$mpg01 <- ifelse(Auto$mpg > median(Auto$mpg),1,0)

set.seed(5)
rands <- rnorm(nrow(Auto))
test <- rands > quantile(rands,0.75)
auto.test <- Auto[test,]
auto.train <- Auto[!test,]

#d
auto.fit.lda <- lda(mpg01 ~ displacement + horsepower + weight + year, data = auto.train)
auto.fit.lda
auto.pred.lda <- predict(auto.fit.lda, auto.test)
table(auto.pred.lda$class, auto.test$mpg01)
mean(auto.pred.lda$class!=auto.test$mpg01)

#e
auto.fit.qda <- qda(mpg01 ~ displacement + horsepower + weight + year, data = auto.train)
auto.fit.qda
auto.fit.qda <- predict(auto.fit.qda, auto.test)
table(auto.fit.qda$class, auto.test$mpg01)
mean(auto.fit.qda$class!=auto.test$mpg01)


#g
train.X <- cbind(auto.train$cylinders, auto.train$weight, auto.train$displacement, auto.train$horsepower)
test.X <- cbind(auto.test$cylinders, auto.test$weight, auto.test$displacement, auto.test$horsepower)

train.mpg01 <- auto.train$mpg01

set.seed (1)
#K=1
knn.pred=knn(train.X,test.X,train.mpg01 ,k=1)
table(knn.pred,auto.test$mpg01)
mean(knn.pred!=auto.test$mpg01)

#K=5
knn.pred=knn(train.X,test.X,train.mpg01 ,k=5)
table(knn.pred,auto.test$mpg01)
mean(knn.pred!=auto.test$mpg01)

#K=10
knn.pred=knn(train.X,test.X,train.mpg01 ,k=10)
table(knn.pred,auto.test$mpg01)
mean(knn.pred!=auto.test$mpg01)

#K=100
knn.pred=knn(train.X,test.X,train.mpg01 ,k=100)
table(knn.pred,auto.test$mpg01)
mean(knn.pred!=auto.test$mpg01)


#Q6
data(iris)
head(iris)
summary(iris)

