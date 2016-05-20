
#Q 2.
# In the lab, a classification tree was applied to the Carseats data set af- ter converting Sales into a qualitative response variable. Now we will seek to predict Sales using regression trees and related approaches, treating the response as a quantitative variable.
library(ISLR)
library(tree)
library(randomForest)

#(a) Split the data set into a training set and a test set.
size = nrow(Carseats)
set.seed(702)
tsample <- sample(size, size / 4)
train.carseats <- Carseats[-tsample, ]
test.carseats <- Carseats[tsample, ]
head(Carseats)
summary(Carseats)
dim(train.carseats)
dim(test.carseats)

#(b) Fit a regression tree to the training set. Plot the tree, and inter- pret the results. What test error rate do you obtain?
set.seed(702)
tree.carseats <- tree(Sales ~ ., data = train.carseats)
summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats, pretty = 0)

predict.carseats <- predict(tree.carseats, newdata = test.carseats)
mean((predict.carseats - test.carseats$Sales)^2)

#(c) Use cross-validation in order to determine the optimal level of tree complexity. 
# Does pruning the tree improve the test error rate?
set.seed(702)
cv.carseats <- cv.tree(tree.carseats)
plot(cv.carseats$size, cv.carseats$dev, type = "b")
len <- length(cv.carseats$size)
tree.min <- which.min(cv.carseats$dev)
minsize <- (len - tree.min) + 1
points(minsize ,cv.carseats$dev[tree.min], col = "red", cex = 2, pch = 20)
#par(mfrow = c(1, 1))

set.seed(702)
prune.carseats <- prune.tree(tree.carseats, best = minsize)
plot(prune.carseats)
text(prune.carseats, pretty = 0)

predict.prune.carseats <- predict(prune.carseats, newdata = test.carseats)
mean((predict.prune.carseats - test.carseats$Sales)^2)

#(d) Use the bagging approach in order to analyze this data. 
# What test error rate do you obtain? Use the importance() function to determine which variables are most important.
set.seed(702)
bag.carseats <- randomForest(Sales ~ ., data = train.carseats, mtry = 10, importance = TRUE)
predict.bag.carseats <- predict(bag.carseats, newdata = test.carseats)
mean((predict.bag.carseats - test.carseats$Sales)^2)

importance(bag.carseats)

#(e) Use random forests to analyze this data. What test error rate do you obtain? 
# Use the importance() function to determine which variables are most important. 
# Describe the effect of m, the num- ber of variables considered at each split, on the error rate obtained.
set.seed(702)
rf.carseats <- randomForest(Sales ~ ., data = train.carseats, mtry = 3, ntree = 500, importance = TRUE)
predict.rf <- predict(rf.carseats, newdata = test.carseats)

rf.carseats1 <- randomForest(Sales ~ ., data = train.carseats, mtry = 4, ntree = 500, importance = TRUE)
predict.rf1 <- predict(rf.carseats1, newdata = test.carseats)

rf.carseats2 <- randomForest(Sales ~ ., data = train.carseats, mtry = 5, ntree = 500, importance = TRUE)
predict.rf2 <- predict(rf.carseats2, newdata = test.carseats)


mean((predict.rf - test.carseats$Sales)^2)
importance(rf.carseats)

mean((predict.rf1 - test.carseats$Sales)^2)
importance(rf.carseats1)

mean((predict.rf2 - test.carseats$Sales)^2)
importance(rf.carseats2)



# Q 3. This problem involves the OJ data set which is part of the ISLR package.

# (a) Create a training set containing a random sample of 800 observations, and a test set containing the remaining observations.
set.seed(702)
train <- sample(nrow(OJ), 800)
train.oj <- OJ[train, ]
test.oj <- OJ[-train, ]
dim(OJ)
head(OJ)
dim(train.oj)
dim(test.oj)

# (b) Fit a tree to the training data, with Purchase as the response and the other variables 
# as predictors. Use the summary() function to produce summary statistics about the tree,
# and describe the results obtained. What is the training error rate? How many terminal nodes does the tree have?
tree.oj <- tree(Purchase ~ ., data = train.oj)
summary(tree.oj)

# (c) Type in the name of the tree object in order to get a detailed text output.
# Pick one of the terminal nodes, and interpret the information displayed.
tree.oj

# (d) Create a plot of the tree, and interpret the results.
plot(tree.oj)
text(tree.oj, pretty = 0)

# (e) Predict the response on the test data, and produce a confusion matrix comparing the test labels to 
# the predicted test labels. What is the test error rate?
predict.tree.oj <- predict(tree.oj, test.oj, type = "class")
cm <- table(predict.tree.oj, test.oj$Purchase)
cm
(cm[1,2]+cm[2,1])/ sum(cm)

# (f) Apply the cv.tree() function to the training set in order to determine the optimal tree size.
set.seed(702)
cv.oj <- cv.tree(tree.oj, FUN = prune.misclass)
cv.oj
plot(cv.oj$size, cv.oj$dev, type = "b")
len <- length(cv.oj$size)
tree.min <- which.min(cv.oj$dev)
minsize <- tree.min
#minsize <- (len - tree.min) + 1
points(cv.oj$size[minsize] ,cv.oj$dev[tree.min], col = "red", cex = 2, pch = 20)


# (g) Produce a plot with tree size on the x-axis and cross-validated classification error rate on the y-axis.
plot(cv.oj$size, cv.oj$dev, type = "b", xlab = "Tree size", ylab = "CV error")

# (h) Which tree size corresponds to the lowest cross-validated classi- fication error rate?
# (i) Produce a pruned tree corresponding to the optimal tree size obtained using cross-validation. 
# If cross-validation does not lead to selection of a pruned tree, then create a pruned tree with five terminal nodes.
prune.oj <- prune.misclass(tree.oj, best = 6)
plot(prune.oj)
text(prune.oj, pretty = 0)

# (j) Compare the training error rates between the pruned and unpruned trees. Which is higher?
summary(tree.oj)
summary(prune.oj)
# (k) Compare the test error rates between the pruned and unpruned trees. Which is higher?

prune.pred.oj <- predict(prune.oj, test.oj, type = "class")
mean(prune.pred.oj != test.oj$Purchase)


# Q 4. We now use boosting to predict Salary in the Hitters data set.
library(gbm)
library(glmnet)
library(pls)

# (a) Remove the observations for whom the salary information is unknown, 
# and then log-transform the salaries.
Hitters <- na.omit(Hitters)
Hitters$Salary <- log(Hitters$Salary)

# (b) Create a training set consisting of the first 200 observations, 
# and a test set consisting of the remaining observations.
train <- 1:200
head(Hitters)
dim(Hitters)
train.hitters <- Hitters[train, ]
test.hitters <- Hitters[-train, ]

# (c) Perform boosting on the training set with 1,000 trees for a range of values of the shrinkage parameter λ. 
# Produce a plot with different shrinkage values on the x-axis and the corresponding training set MSE on the y-axis.

set.seed(702)
pows <- seq(-10, -0.2, by = 0.1)
lambdas <- 10^pows
error.train <- rep(NA, length(lambdas))
error.test <- rep(NA, length(lambdas))
for (i in 1:length(lambdas)) {
  set.seed(702)
  boost.hitters <- gbm(Salary ~ ., data = train.hitters, distribution = "gaussian", n.trees = 1000, 
                       shrinkage = lambdas[i])
  set.seed(702)
  pred.train.boost <- predict(boost.hitters, train.hitters, n.trees = 1000)
  error.train[i] <- mean((pred.train.boost - train.hitters$Salary)^2)
  set.seed(702)
  pred.test.boost <- predict(boost.hitters, test.hitters, n.trees = 1000)
  error.test[i] <- mean((pred.test.boost - test.hitters$Salary)^2)
}

plot(lambdas, error.train, type = "b", xlab = "Shrinkage", ylab = "Train MSE", col = "dark green", pch = 20)


# (d) Produce a plot with different shrinkage values on the x-axis and the corresponding test set MSE on the y-axis.
plot(lambdas, error.test, type = "b", xlab = "Shrinkage", ylab = "Test MSE", col = "dark red", pch = 20)

min(error.test)
lambdas[which.min(error.test)]

# (e) Compare the test MSE of boosting to the test MSE that results from applying two of the regression approaches 
# seen in Chapters 3 and 6.


set.seed (702)
lm.hitters <- lm(Salary ~ ., data = train.hitters)
lm.pred <- predict(lm.hitters,test.hitters)
mean((lm.pred - test.hitters$Salary)^2)

set.seed (702)
pcr.fit=pcr(Salary ~ ., data=train.hitters,scale=TRUE, validation ="CV")
#which.min(pcr.fit$validation$MSEP)
validationplot(pcr.fit,val.type="MSEP")
pcr.pred=predict(pcr.fit,test.hitters,ncomp=8)
mean((pcr.pred- test.hitters$Salary)^2)

# (f) Which variables appear to be the most important predictors in the boosted model?

boost.hitters <- gbm(Salary ~ ., data = train.hitters, distribution = "gaussian", n.trees = 1000, shrinkage = lambdas[which.min(error.test)])
summary(boost.hitters)
# (g) Now apply bagging to the training set. What is the test set MSE for this approach?
set.seed(702)
bag.hitters <- randomForest(Salary ~ ., data = train.hitters, mtry = 19, ntree = 500)
predict.bag.hitters <- predict(bag.hitters, newdata = test.hitters)
mean((predict.bag.hitters - test.hitters$Salary)^2)


# Q 5

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

# Using decision tree
set.seed(702)
tree.credit <- tree(formula,data = train.credit)

# validation set 
predict.credit.tree <- predict(tree.credit,test.credit,type="class")
# error rate
mean(test.credit$Approved != predict.credit.tree)


cv.credit.tree <- cv.tree(tree.credit,FUN=prune.misclass)
# plot(cv.credit.tree$size, cv.credit.tree$dev, type = "b")
minsize <- which.min(cv.credit.tree$dev)
# points(cv.credit.tree$size[minsize], cv.credit.tree$dev[minsize], col = "red", cex = 2, pch = 20)

prune.credit.tree <- prune.misclass(tree.credit,best=cv.credit.tree$size[minsize])
# plot(prune.credit.tree)
# text(prune.credit.tree,pretty=0)
#predict the test - find test error
predict.credit.tree.cv <- predict(prune.credit.tree,test.credit,type="class")

# cv error rate
mean(test.credit$Approved != predict.credit.tree.cv)


# Using boosting
set.seed(702)
pows = seq(-10, -0.2, by = 0.1)
lambdas = 10^pows
length.lambdas = length(lambdas)
error.train <- rep(NA, length(lambdas))
error.test <- rep(NA, length(lambdas))

for (i in 1:length(lambdas)) {
  set.seed(702)
  boost.credit <- gbm(formula, data = train.credit, distribution = "multinomial", n.trees = 1000, 
                       shrinkage = lambdas[i])
  summary(boost.credit)
  set.seed(702)
  pred.test.boost.credit <- predict(boost.credit, train.credit, n.trees = 1000)
  cre.test = data.frame(pred.test.boost.credit)
  test.pred <- ifelse(cre.test[, c(2)] > 0.5, 'yes', 'no')
  # train.errors[i] = mean(train$class != train.pred)
  error.test[i] = mean(test.credit$Approved != test.pred)
}


min(error.test, na.rm = T)

# Using randomforest
bag.credit = randomForest(formula, data = train.credit, mtry = 5, ntree = 500, importance = T)
bag.pred = predict(bag.credit, test.credit)
mean(test.credit$Approved != bag.pred)
