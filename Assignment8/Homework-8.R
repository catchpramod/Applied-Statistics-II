library(ISLR)

#Q2 
data(College)
head(College)
#a
set.seed(20)
obs<-dim(College)[1]
sIndex <- sample(obs,obs/4)
train.college <-  College[-sIndex,]
test.college <- College[sIndex,]
dim(train.college)
dim(test.college)

#b
set.seed(20)
lm.college <- lm(Apps ~ ., data = train.college)
pred.college <- predict(lm.college, test.college)
mean((pred.college - test.college$Apps)^2)

#c
library(glmnet)
train.x.college <- model.matrix(Apps ~ ., train.college)[,-1]
train.y.college <- train.college$Apps
test.x.college <- model.matrix(Apps ~ ., test.college)[,-1]
test.y.college <- test.college$Apps

grid <- 10 ^ seq(4, -2, length = 100)
ridge.college <- glmnet(train.x.college, train.y.college, alpha = 0, lambda = grid)
set.seed(20)
cv.ridge.college <- cv.glmnet(train.x.college, train.y.college, alpha = 0)
plot(cv.ridge.college)
bestlam.ridge <- cv.ridge.college$lambda.min
bestlam.ridge

ridge.pred.college <- predict(ridge.college,s=bestlam.ridge ,newx=test.x.college)
mean((ridge.pred.college - test.y.college)^2)


#d

lasso.college <- glmnet(train.x.college, train.y.college, alpha = 1, lambda = grid)
set.seed(20)
cv.lasso.college <- cv.glmnet(train.x.college, train.y.college, alpha = 1)
plot(cv.lasso.college)
bestlam.lasso <- cv.lasso.college$lambda.min
bestlam.lasso

lasso.pred.college <- predict(lasso.college,s=bestlam.lasso ,newx=test.x.college)
mean((lasso.pred.college - test.y.college)^2)

lasso.coeff.college <- predict(lasso.college,s=bestlam.lasso ,type = "coefficients")
lasso.coeff.college
lasso.coeff.college[lasso.coeff.college !=0]

#e
library(pls)
set.seed(20)
pcr.fit <- pcr(Apps~., data=train.college ,scale=TRUE,validation ="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")

pcr.pred=predict(pcr.fit,test.college,ncomp=17)  
mean((pcr.pred-test.college$Apps)^2)
rsq
#f
set.seed(20)
plsr.fit <- plsr(Apps~., data=train.college ,scale=TRUE,validation ="CV")
summary(plsr.fit)
validationplot(plsr.fit,val.type="MSEP")

plsr.pred=predict(plsr.fit,test.college,ncomp=12)  
mean((plsr.pred-test.college$Apps)^2)

#g
#calculate R^2 value for the models
test.avg <- mean(test.college$Apps)
r.lm <- 1- (sum((pred.college - test.college$Apps)^2)/sum((test.avg - test.college$Apps)^2))
r.ridge <- 1- (sum((ridge.pred.college - test.y.college)^2)/sum((test.avg - test.y.college)^2))
r.lasso <- 1- (sum((lasso.pred.college - test.y.college)^2)/sum((test.avg - test.y.college)^2))
r.pcr <- 1- (sum((pcr.pred-test.college$Apps)^2)/sum((test.avg-test.college$Apps)^2))
r.plsr <- 1- (sum((plsr.pred-test.college$Apps)^2)/sum((test.avg-test.college$Apps)^2))
r.lm
r.ridge
r.lasso
r.pcr
r.plsr


#Q3
library(MASS)
#a
data(Boston)
head(Boston)
#a

train.x.boston <- model.matrix(crim ~ ., Boston)[,-1]
train.y.boston <- Boston$crim

#ridge
set.seed(20)
cv.ridge.boston <- cv.glmnet(train.x.boston, train.y.boston, alpha = 0,type.measure = "mse")
plot(cv.ridge.boston)
#min value of lambda
cv.ridge.boston$lambda.min
#cross validation error for mean lambda
min(cv.ridge.boston$cvm)

#lasso
set.seed(20)
cv.lasso.boston <- cv.glmnet(train.x.boston, train.y.boston, alpha = 1,type.measure = "mse")
plot(cv.lasso.boston)
#min value of lambda
cv.lasso.boston$lambda.min
#cross validation error for mean lambda
min(cv.lasso.boston$cvm)


#pcr
library(pls)
set.seed(20)
pcr.fit <- pcr(crim~., data=Boston ,scale=TRUE,validation ="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")
MSEP(pcr.fit)

#c
cv.lasso.boston$nzero[cv.lasso.boston$lambda == cv.lasso.boston$lambda.min]
