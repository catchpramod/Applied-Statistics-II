# Q1.
# a
plot(NA, NA, xlim = c(-4, 4), ylim = c(-1, 9), asp = 1, xlab = "X1", ylab = "X2", main="(1 + X1)^2 + (2 − X2)^2 = 4")
symbols(c(-1), c(2), circles = c(2), add = TRUE, inches = FALSE)

#b
text(c(-1), c(2), "fn <= 4", col = "red")
text(c(-4), c(2), "fn > 4", col = "blue")

#c
points(0,0, col = "blue", pch=16)
points(-1,1, col = "red", pch=16)
points(2,2, col = "blue", pch=16)
points(3,8, col = "blue", pch=16)


#Q2
library(ISLR)
library(e1071)
#a
head(Auto)
gas.median = median(Auto$mpg)
Auto$milage = as.factor( ifelse(Auto$mpg > gas.median, 1, 0))
summary(Auto$milage)

#b
set.seed(702)
auto.linear = tune(svm, milage ~ ., data = Auto, kernel = "linear", ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(auto.linear)

#c
set.seed(702)
auto.poly = tune(svm, milage ~ ., data = Auto, kernel = "polynomial", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100), degree = c(2, 3, 4, 5)))
summary(auto.poly)

set.seed(702)
auto.radial <- tune(svm, milage ~ ., data = Auto, kernel = "radial", ranges = list(cost = c(0.01, 0.1, 1, 10, 100, 1000), gamma = c(0.01, 0.1, 1, 5, 10, 100)))
summary(auto.radial)

#d
svm.linear = svm(milage ~ ., data = Auto, kernel = "linear", cost = 1)
svm.poly = svm(milage ~ ., data = Auto, kernel = "polynomial", cost = 100, degree = 2)
svm.radial = svm(milage ~ ., data = Auto, kernel = "radial", cost = 100, gamma = 0.01)

svmplots <- function(svmfit,fitname) {
  par(mfrow = c(2, 2))
  mypath <- file.path(paste(fitname, "_dp.jpg", sep = ""))
  jpeg(file=mypath)
  plot(svmfit, Auto, as.formula("mpg ~ displacement"))
  dev.off()
  
  mypath <- file.path(paste(fitname, "_hp.jpg", sep = ""))
  jpeg(file=mypath)
  plot(svmfit, Auto, as.formula("mpg ~ horsepower"))
  dev.off()
  
  mypath <- file.path(paste(fitname, "_we.jpg", sep = ""))
  jpeg(file=mypath)
  plot(svmfit, Auto, as.formula("mpg ~ weight"))
  dev.off()
  
  mypath <- file.path(paste(fitname, "_ac.jpg", sep = ""))
  jpeg(file=mypath)
  plot(svmfit, Auto, as.formula("mpg ~ acceleration"))
  dev.off()
}

svmplots(svm.linear,"/Users/pramod/OneDrive/Assignment12/linear")
svmplots(svm.poly,"/Users/pramod/OneDrive/Assignment12/poly")
svmplots(svm.radial,"/Users/pramod/OneDrive/Assignment12/radial")


#Q3

#a
set.seed(702)
head(oj)
summary(oj)
train = sample(dim(oj)[1], 800)
oj.train = oj[train, ]
oj.test = oj[-train, ]
dim(oj.train)
dim(oj.test)

#b
set.seed(702)
oj.linear = svm(Purchase ~ ., kernel = "linear", data = oj.train, cost = 0.01)
summary(oj.linear)

#c
oj.train.pred = predict(oj.linear, oj.train)
table(oj.train$Purchase, oj.train.pred)
mean(oj.train$Purchase != oj.train.pred)

oj.test.pred = predict(oj.linear, oj.test)
table(oj.test$Purchase, oj.test.pred)
mean(oj.test$Purchase != oj.test.pred)

#d
set.seed(702)
cs = 10^seq(-2, 1, by = 0.1)
oj.tune = tune(svm, Purchase ~ ., data = oj.train, kernel = "linear", ranges = list(cost = cs))
summary(oj.tune)

#e

set.seed(702)
oj.linear = svm(Purchase ~ ., kernel = "linear", data = oj.train, cost = 0.01995262)
summary(oj.linear)

oj.train.pred = predict(oj.linear, oj.train)
table(oj.train$Purchase, oj.train.pred)
mean(oj.train$Purchase != oj.train.pred)

oj.test.pred = predict(oj.linear, oj.test)
table(oj.test$Purchase, oj.test.pred)
mean(oj.test$Purchase != oj.test.pred)

#f
set.seed(702)
oj.radial = svm(Purchase ~ ., kernel = "radial", data = oj.train)
summary(oj.radial)

oj.train.pred = predict(oj.radial, oj.train)
table(oj.train$Purchase, oj.train.pred)
mean(oj.train$Purchase != oj.train.pred)

oj.test.pred = predict(oj.radial, oj.test)
table(oj.test$Purchase, oj.test.pred)
mean(oj.test$Purchase != oj.test.pred)


set.seed(702)
oj.tune = tune(svm, Purchase ~ ., data = oj.train, kernel = "radial", ranges = list(cost = cs))
summary(oj.tune)


set.seed(702)
oj.radial = svm(Purchase ~ ., kernel = "radial", data = oj.train, cost = 1.995262)
summary(oj.radial)

oj.train.pred = predict(oj.radial, oj.train)
table(oj.train$Purchase, oj.train.pred)
mean(oj.train$Purchase != oj.train.pred)

oj.test.pred = predict(oj.linear, oj.test)
table(oj.test$Purchase, oj.test.pred)
mean(oj.test$Purchase != oj.test.pred)

#g

set.seed(702)
oj.poly = svm(Purchase ~ ., data = oj.train, kernel = "polynomial", degree = 2)
summary(oj.poly)

oj.train.pred = predict(oj.poly, oj.train)
table(oj.train$Purchase, oj.train.pred)
mean(oj.train$Purchase != oj.train.pred)

oj.test.pred = predict(oj.poly, oj.test)
table(oj.test$Purchase, oj.test.pred)
mean(oj.test$Purchase != oj.test.pred)


set.seed(702)
oj.tune = tune(svm, Purchase ~ ., data = oj.train, kernel = "polynomial", ranges = list(cost = cs, degree = c(2, 3, 4, 5)))
summary(oj.tune)


set.seed(702)
oj.poly = svm(Purchase ~ ., data = oj.train, kernel = "polynomial", cost = 7.943282 ,  degree = 2)
summary(oj.poly)

oj.train.pred = predict(oj.poly, oj.train)
table(oj.train$Purchase, oj.train.pred)
mean(oj.train$Purchase != oj.train.pred)

oj.test.pred = predict(oj.poly, oj.test)
table(oj.test$Purchase, oj.test.pred)
mean(oj.test$Purchase != oj.test.pred)


#Q4


credit <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data",sep = ",", col.names = c('Male',	'Age',	'Debt',	'Married',	'BankCustomer',	'EducationLevel',	'Ethnicity',	'YearsEmployed',	'PriorDefault',	'Employed',	'CreditScore',	'DriversLicense',	'Citizen',	'ZipCode',	'Income',	'Approved'))

credit <- transform(credit, Approved = ifelse(Approved == '+','yes' ,'no'))
credit$Approved<- as.factor(credit$Approved)
credit$Age <- as.numeric(as.character(credit$Age))
credit <- na.omit(credit)

credit$Age[is.na(credit$Age) ] <- mean(credit$Age, na.rm = TRUE)

credit$Male[credit$Male=='?'] <- 'b'
credit$Married[credit$Married=='?'] <- 'u'
credit$BankCustomer[credit$BankCustomer=='?'] <- 'g'
credit$Ethnicity[credit$Ethnicity=='?'] <- 'v'

credit$Male <- factor(credit$Male)
credit$Married <- factor(credit$Married)
credit$BankCustomer <- factor(credit$BankCustomer)
credit$Ethnicity <- factor(credit$Ethnicity)

summary(credit)
obs<-dim(credit)[1]
set.seed(702)
train.creditSample <- sample(obs,obs/4)
train.credit <-  credit[-train.creditSample,]
test.credit <- credit[train.creditSample,]

formula <- as.formula("Approved ~ Age + Debt + YearsEmployed + CreditScore  + Income + PriorDefault + Employed")


# Using radial kernel

#VSA
set.seed(702)
credit.radial = svm(formula, data = train.credit, kernel = "radial")
credit.predict = predict(credit.radial, test.credit)
table(test.credit$Approved, credit.predict)
mean(test.credit$Approved != credit.predict)

#LOOCV
summary(credit)
c.errors  = rep(NA, nrow(credit))
for (i in 1:nrow(credit)){
  credit.loocv = svm(formula, data = credit[-i,], kernel = "radial")
  credit.loocv.predic = predict(credit.loocv, credit[i,])
  error = ifelse((credit.loocv.predic == credit[i,]$Approved), 0, 1)
  c.errors[i] = error
  print(i)
} 
mean(c.errors)

#CV
set.seed(702)
credit.cv.radial = tune(svm, formula, data = credit, kernel = "radial" )
summary(credit.cv.radial)
