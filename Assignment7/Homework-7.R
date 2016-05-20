library(ISLR)


#Q2
#a
head(Default)
summary(Default)
dim(Default)
set.seed(702)
fit.glm.default <- glm(default ~ income + balance, data = Default, family = "binomial")
summary(fit.glm.default)$coef[,2]
summary(fit.glm.default)
coefficients(fit.glm.default)

#b

boot.fn <- function(data, index) {
  fit.glm.index <- glm(default ~ income + balance, data = data, family = "binomial", subset = index)
  return (coefficients(fit.glm.index))
}

#c
library(boot)
set.seed(702)
bootstrap.glm <- boot(Default, boot.fn, 1000)
bootstrap.glm




#Q3

library(MASS)
head(Boston)
dim(Boston)
#a
mu.hat.mean <- mean(Boston$medv)
mu.hat.mean

#b
n <- dim(Boston)[1]
se.mean <- sd(Boston$medv) / sqrt(n)
se.mean

#c

boot.fn.se.mean <- function(data, index) {
  m <- mean(data[index])
  return (m)
}
set.seed(702)
b.se.mean <- boot(Boston$medv, boot.fn.se.mean, 1000)
b.se.mean


#d
confidence.interval <- c(22.53281-2*0.408103,22.53281+2*0.408103)
confidence.interval
t.test(Boston$medv)

#e
mu.hat.median <- median(Boston$medv)
mu.hat.median

#f

boot.fn.se.median <- function(data, index) {
  m <- median(data[index])
  return (m)
}
set.seed(702)
b.se.median <- boot(Boston$medv, boot.fn.se.median, 1000)
b.se.median

#g
mu.hat.percent <- quantile(Boston$medv, c(0.1))
mu.hat.percent
boot.fn.se.percent <- function(data, index) {
  q <- quantile(data[index], c(0.1))
  return (q)
}
set.seed(702)
b.se.percent <- boot(Boston$medv, boot.fn.se.percent, 1000)
b.se.percent


#Q4

credit <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data",sep = ",", 
                     col.names = c('Male',	'Age',	'Debt',	'Married',	'BankCustomer',	'EducationLevel',	'Ethnicity',	'YearsEmployed',	'PriorDefault',	'Employed',	'CreditScore',	'DriversLicense',	'Citizen',	'ZipCode',	'Income',	'Approved'))

credit <- transform(credit, Approved = ifelse(Approved == '+','yes' ,'no'))
credit$Approved<- as.factor(credit$Approved)
credit$Age <- as.numeric(as.character(credit$Age))
head(credit)
summary(credit)
creditImp<-credit
creditImp$Age[is.na(creditImp$Age) ] <- mean(creditImp$Age, na.rm = TRUE)

summary(creditImp)
head(creditImp)
obs<-dim(creditImp)[1]
set.seed(702)
train.creditSample <- sample(obs,obs/4)
train.credit <-  creditImp[-train.creditSample,]
test.credit <- creditImp[train.creditSample,]


formula <- as.formula("Approved ~ Age + Debt + YearsEmployed + CreditScore  + Income + PriorDefault + Employed")
#logistic regression
#vsa
c.logistic <- glm(formula, data = train.credit, family = "binomial")
summary(c.logistic)
c.logistic.prob <- predict(c.logistic, test.credit, type = "response")
c.logistic.pred <- rep("no", length(c.logistic.prob))
c.logistic.pred[c.logistic.prob > 0.5] <- "yes"
table(c.logistic.pred,test.credit$Approved)
print("Linear regression VSA error")
mean(c.logistic.pred != test.credit$Approved)

# loocv

N <- dim(creditImp)[1]
error <- rep(0, N)
for (i in 1:N) {
  fit.glm.lcv <- glm(formula, data = creditImp[-i,],  family = "binomial")
  probs.lcv<- predict(fit.glm.lcv, creditImp[i,], type = "response")
  pred.lcv <- ifelse(probs.lcv > 0.5,"yes","no")
  error[i] <- ifelse(pred.lcv!=creditImp[i,]$Approved,1,0)
}
summary(as.factor(error))
print("Linear regression LOOCV error")
mean(error)

#cv
K <- 5
kFold <- split(sample(dim(creditImp)[1]), 1:K)

error.cv <- rep(0,K)
for(i in 1:K){
  train.cv <- creditImp[-kFold[[i]], ]
  test.cv <- creditImp[kFold[[i]], ]
  modal.glm.cv <- glm(formula, data = train.cv,  family = "binomial")
  modal.glm.prob <- predict(modal.glm.cv, test.cv, type = "response")
  modal.glm.pred <- rep("no", length(modal.glm.prob))
  modal.glm.pred[modal.glm.prob > 0.5] <- "yes"
  table(modal.glm.pred,test.cv$Approved)
  error.cv[i] <- mean(modal.glm.pred != test.cv$Approved)
}
print("Linear regression CV error")
mean(error.cv)


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
#VSA
c.lda<-lda(formula, data = train.credit)
c.lda.pred <- predict(c.lda, test.credit)
table(c.lda.pred$class, test.credit$Approved)
print("LDA VSA error")
mean(c.lda.pred$class!=test.credit$Approved)

#LOOCV

error <- rep(0, N)
for (i in 1:N) {
  lc.lda<-lda(formula, data = creditImp[-i,])
  lc.lda.pred <- predict(lc.lda, creditImp[i,])
  error[i] <- ifelse(lc.lda.pred$class!=creditImp[i,]$Approved,1,0)
}
summary(as.factor(error))
print("LDA LOOCV error")
mean(error)

#CV
K <- 5
kFold <- split(sample(dim(creditImp)[1]), 1:K)

error.cv <- rep(0,K)
for(i in 1:K){
  train.cv <- creditImp[-kFold[[i]], ]
  test.cv <- creditImp[kFold[[i]], ]
  lc.lda<-lda(formula, data = train.cv)
  lc.lda.pred <- predict(lc.lda, test.cv)
  error.cv[i] <- mean(lc.lda.pred$class != test.cv$Approved)
}
print("LDA CV error")
mean(error.cv)



#QDA
#VSA
c.qda<-qda(formula, data = train.credit)
c.qda.pred <- predict(c.qda, test.credit)
table(c.qda.pred$class, test.credit$Approved)
mean(c.qda.pred$class!=test.credit$Approved)

#LOOCV

error <- rep(0, N)
for (i in 1:N) {
  lc.qda<-qda(formula, data = creditImp[-i,])
  lc.qda.pred <- predict(lc.qda, creditImp[i,])
  error[i] <- ifelse(lc.qda.pred$class!=creditImp[i,]$Approved,1,0)
}
summary(as.factor(error))
print("qda LOOCV error")
mean(error)

#CV
K <- 5
kFold <- split(sample(dim(creditImp)[1]), 1:K)

error.cv <- rep(0,K)
for(i in 1:K){
  train.cv <- creditImp[-kFold[[i]], ]
  test.cv <- creditImp[kFold[[i]], ]
  lc.qda<-qda(formula, data = train.cv)
  lc.qda.pred <- predict(lc.qda, test.cv)
  error.cv[i] <- mean(lc.qda.pred$class != test.cv$Approved)
}
print("qda CV error")
mean(error.cv)


#MclustDA
library(mclust)

c.MclustDA <- MclustDA(train.credit[c(2,3,4,8,9,10,11,15)], train.credit$Approved)
summary(c.MclustDA)
summary(c.MclustDA, newdata =test.credit[c(2,3,4,8,9,10,11,15)], newclass = test.credit$Approved)

c.MclustDA <- MclustDA(train.credit[c(2,3,4,8,9,10,11,15)], train.credit$Approved, modelType = "EDDA")
summary(c.MclustDA)
summary(c.MclustDA, newdata =test.credit[c(2,3,4,8,9,10,11,15)], newclass = test.credit$Approved)


#Naive Bayes
library(e1071)
nb.model <- naiveBayes(formula, data = train.credit)
summary(nb.model)
nb.pred<-predict(nb.model, test.credit)
summary(nb.pred)
table(nb.pred, test.credit$Approved)
print("NB VSA error")
mean(nb.pred != test.credit$Approved)


#LOOCV

error <- rep(0, N) 
for (i in 1:N) {
  lc.nb<-naiveBayes(formula, data = creditImp[-i,])
  lc.nb.pred <- predict(lc.nb, creditImp[i,])
  error[i] <- ifelse(lc.nb.pred != creditImp[i,]$Approved,1,0)
}
summary(as.factor(error))
print("NB LOOCV error")
mean(error)

#CV
K <- 5
kFold <- split(sample(dim(creditImp)[1]), 1:K)

error.cv.nb <- rep(0,K)
for(i in 1:K){
  train.cv <- creditImp[-kFold[[i]], ]
  test.cv <- creditImp[kFold[[i]], ]
  lc.nb<-naiveBayes(formula, data = train.cv)
  lc.nb.pred <- predict(lc.nb, test.cv)
  error.cv.nb[i] <- mean(lc.nb.pred != test.cv$Approved)
}
print("NB CV error")
mean(error.cv.nb)

