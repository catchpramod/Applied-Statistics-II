library(ISLR)


#Q2
#a
fit.glm.default <- glm(default ~ income + balance, data = Default, family = "binomial")
summary(fit.glm.default)

#b
#i
obs = dim(Default)[1]
set.seed(702)
train.defaultSample <- sample(obs,obs/2)
train.default <-  Default[train.defaultSample,]
test.default <- Default[-train.defaultSample,]
#ii
fit.glm.val <- glm(default ~ income + balance, data = train.default, family = "binomial")
summary(fit.glm.val)
#iii
probs.val <- predict(fit.glm.val, newdata = test.default, type = "response")
pred.glm.val <- rep("No", length(probs.val))
pred.glm.val[probs.val > 0.5] <- "Yes"
table(pred.glm.val,test.default$default)
#iv
mean(pred.glm.val != test.default$default)

#c
train.defaultSample <- sample(obs,obs/2)
train.default <-  Default[train.defaultSample,]
test.default <- Default[-train.defaultSample,]
fit.glm.val <- glm(default ~ income + balance, data = train.default, family = "binomial")
probs.val <- predict(fit.glm.val, newdata = test.default, type = "response")
pred.glm.val <- rep("No", length(probs.val))
pred.glm.val[probs.val > 0.5] <- "Yes"
mean(pred.glm.val != test.default$default)

train.defaultSample <- sample(obs,obs/2)
train.default <-  Default[train.defaultSample,]
test.default <- Default[-train.defaultSample,]
fit.glm.val <- glm(default ~ income + balance, data = train.default, family = "binomial")
probs.val <- predict(fit.glm.val, newdata = test.default, type = "response")
pred.glm.val <- rep("No", length(probs.val))
pred.glm.val[probs.val > 0.5] <- "Yes"
mean(pred.glm.val != test.default$default)

train.defaultSample <- sample(obs,obs/2)
train.default <-  Default[train.defaultSample,]
test.default <- Default[-train.defaultSample,]
fit.glm.val <- glm(default ~ income + balance, data = train.default, family = "binomial")
probs.val <- predict(fit.glm.val, newdata = test.default, type = "response")
pred.glm.val <- rep("No", length(probs.val))
pred.glm.val[probs.val > 0.5] <- "Yes"
mean(pred.glm.val != test.default$default)

#d
train.defaultSample <- sample(obs,obs/2)
train.default <-  Default[train.defaultSample,]
test.default <- Default[-train.defaultSample,]
fit.glm.val <- glm(default ~ income + balance+ student, data = train.default, family = "binomial")
summary(fit.glm.val)
probs.val <- predict(fit.glm.val, newdata = test.default, type = "response")
pred.glm.val <- rep("No", length(probs.val))
pred.glm.val[probs.val > 0.5] <- "Yes"
mean(pred.glm.val != test.default$default)



#Q3
#a
fit.glm.dir <- glm(Direction ~ Lag1 + Lag2, data = Weekly, family = "binomial")
summary(fit.glm.dir)


#b
train.weekly <- Weekly[-1, ]
fit.glm.1 <- glm(Direction ~ Lag1 + Lag2, data = train.weekly, family = "binomial")
summary(fit.glm.1)

#c
test.weekly <- Weekly[1, ]
probs.1 <- predict(fit.glm.1, test.weekly, type = "response")
probs.1
print("Predicted direction:")
ifelse(probs.1 > 0.5,"Up","Down")
print("Actual direction:")
test.weekly$Direction


#d

error <- rep(0, obs.weekly)
for (i in 1:obs.weekly) {
  fit.glm.lcv <- glm(Direction ~ Lag1 + Lag2, data = Weekly[-i, ],  family = "binomial")
  probs.lcv<- predict(fit.glm.lcv, Weekly[i, ], type = "response")
  pred.lcv <- ifelse(probs.lcv > 0.5,"Up","Down")
  error[i] <- ifelse(pred.lcv!=Weekly[i,]$Direction,1,0)
}
summary(as.factor(error))
mean(error)




#Q4
head(Auto)

kFold <- split(sample(dim(Auto)[1]), 1:6)

error.cv <- rep(0,6)
for(i in 1:6){
  train.cv <- Auto[-kFold[[i]], ]
  test.cv <- Auto[kFold[[i]], ]
  auto.lm.cv <- lm(mpg ~  horsepower + poly(horsepower, 2), data=train.cv)
  pred.cv <- predict(auto.lm.cv, test.cv)
  error.cv[i] <- mean((pred.cv-test.cv$mpg)^2)
}

mean(error.cv)




#Q5

credit <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data",sep = ",", 
                     col.names = c('Male',	'Age',	'Debt',	'Married',	'BankCustomer',	'EducationLevel',	'Ethnicity',	'YearsEmployed',	'PriorDefault',	'Employed',	'CreditScore',	'DriversLicense',	'Citizen',	'ZipCode',	'Income',	'Approved'))

credit <- transform(credit, Approved = ifelse(Approved == '+','yes' ,'no'))
credit$Approved<- as.factor(credit$Approved)
credit$Age <- as.numeric(as.character(credit$Age))
head(credit)
summary(credit)
library(mice)
# creditImp <- complete(mice(credit),1)
# creditImp<-na.omit(credit)
creditImp<-credit
creditImp$Age[is.na(creditImp$Age) ] <- mean(creditImp$Age, na.rm = TRUE)

summary(creditImp)
head(creditImp)
obs<-dim(creditImp)[1]
train.creditSample <- sample(obs,obs/4)
train.credit <-  creditImp[-train.creditSample,]
test.credit <- creditImp[train.creditSample,]

# str(creditImp)

formula <- as.formula("Approved ~ Age + Debt + YearsEmployed + CreditScore  + Income + PriorDefault + Employed")
#logistic regression
c.logistic <- glm(formula, data = train.credit, family = "binomial")
summary(c.logistic)
# alias(c.logistic)
c.logistic.prob <- predict(c.logistic, test.credit, type = "response")
c.logistic.pred <- rep("no", length(c.logistic.prob))
c.logistic.pred[c.logistic.prob > 0.5] <- "yes"
table(c.logistic.pred,test.credit$Approved)
mean(c.logistic.pred != test.credit$Approved)

#KNN
library(class)
# head(train.credit[c(2,3,4,8,9,10,11,15)])
train.X <- as.matrix(train.credit[c(2,3,4,8,9,10,11,15)])
test.X <- as.matrix(test.credit[c(2,3,4,8,9,10,11,15)])

c.knn.pred=knn(train.X,test.X,train.credit$Approved ,k=1)
# table(c.knn.pred,train.credit$Approved)
# mean(c.knn.pred==train.credit$Approved)

library(MASS)
#LDA
c.lda<-lda(formula, data = train.credit)
c.lda.pred <- predict(c.lda, test.credit)
table(c.lda.pred$class, test.credit$Approved)
mean(c.lda.pred$class!=test.credit$Approved)

#QDA
c.qda<-qda(formula, data = train.credit)
c.qda.pred <- predict(c.qda, test.credit)
table(c.qda.pred$class, test.credit$Approved)
mean(c.qda.pred$class!=test.credit$Approved)

#MclustDA
library(mclust)

c.MclustDA <- MclustDA(train.credit[c(2,3,4,8,9,10,11,15)], train.credit$Approved)
summary(c.MclustDA)
summary(c.MclustDA, newdata =test.credit[c(2,3,4,8,9,10,11,15)], newclass = test.credit$Approved)

c.MclustDA <- MclustDA(train.credit[c(2,3,4,8,9,10,11,15)], train.credit$Approved, modelType = "EDDA")
summary(c.MclustDA)
summary(c.MclustDA, newdata =test.credit[c(2,3,4,8,9,10,11,15)], newclass = test.credit$Approved)

