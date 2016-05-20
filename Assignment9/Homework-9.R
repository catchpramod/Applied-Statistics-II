library(ISLR)
library(boot)
par(mfrow=c(1,1))
# Q1

# a
set.seed(10)
cvmse <- rep(NA, 10)
for (i in 1:10) {
  fit <- glm(wage ~ poly(age, i), data = Wage)
  cvmse[i] <- cv.glm(Wage, fit, K = 10)$delta[1]
}
plot(1:10, cvmse, xlab = "Ploynomial Degree", ylab = "Test CV MSE", type = "l")
title("Polynomial regression CV result",outer=T)
points(which.min(cvmse), min(cvmse), col = "red", cex = 2, pch = 20)


fit.1 <- lm(wage ~ age, data = Wage)
fit.2 <- lm(wage ~ poly(age, 2), data = Wage)
fit.3 <- lm(wage ~ poly(age, 3), data = Wage)
fit.4 <- lm(wage ~ poly(age, 4), data = Wage)
fit.5 <- lm(wage ~ poly(age, 5), data = Wage)
fit.6 <- lm(wage ~ poly(age, 6), data = Wage)

anova(fit.1, fit.2, fit.3, fit.4, fit.5, fit.6)

agelims <- range(Wage$age)
age.grid <- seq(from = agelims[1], to = agelims[2])
fit <- lm(wage ~ poly(age, 3), data = Wage)
preds <- predict(fit, newdata = list(age = age.grid))
plot(age,wage,data = Wage, xlim=agelims,cex=.5,col="darkgrey")
title("Degree-4 Polynomial",outer=T)
lines(age.grid, preds, col = "blue", lwd = 2)
se.bands <- cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
matlines(age.grid,se.bands,lwd=1,col="red",lty=3)

# b
set.seed(10)
cvs <- rep(NA, 10)
for (i in 2:10) {
  Wage$age.cut <- cut(Wage$age, i)
  fit <- glm(wage ~ age.cut, data = Wage)
  cvs[i] <- cv.glm(Wage, fit, K = 10)$delta[1]
}
plot(2:10, cvs[-1], xlab = "Cuts", ylab = "Test MSE", type = "l")
title("Step function regression CV result",outer=T)
points(which.min(cvs), cvs[which.min(cvs)], col = "red", cex = 2, pch = 20)

fit <- glm(wage ~ cut(age, 8), data = Wage)
preds <- predict(fit, data.frame(age = age.grid))
plot(age,wage,data = Wage, xlim=agelims,cex=.5,col="darkgrey")
title("8-cuts Step Function",outer=T)
lines(age.grid, preds, col = "blue", lwd = 2)

#Q2

library(MASS)
attach(Boston)

#a
lm.boston <- lm(nox ~ poly(dis, 3), data = Boston)
summary(lm.boston)
dislims <- range(Boston$dis)
dis.grid <- seq(dislims[1], dislims[2], by = 0.1)
preds.boston <- predict(lm.boston, list(dis = dis.grid))

plot(nox ~ dis, data = Boston,cex=.5,col="darkgrey")
title("Cubic regression")
lines(dis.grid, preds.boston, col = "blue", pch = 20, lwd = 3)

#b
set.seed(10)
rss = rep(NA,10)
for(i in 1:10){
  fit <- lm(nox ~ poly(dis, i), data = Boston)  
  rss[i] <- sum(fit$residuals^2)
}
plot(1:10, rss, xlab = "Degree", ylab = "RSS", type = "l")
title("RSS vs Ploynomial Degree")
points(which.min(rss), min(rss), col = "red", cex = 2, pch = 20)

# c
set.seed(10)
deltas = rep(NA,10)
for(i in 1:10){
  fit = glm(nox ~ poly(dis, i), data = Boston)
  deltas[i] = cv.glm(Boston, fit, K = 10)$delta[2]
}

plot(1:10, deltas, xlab = "Degree", ylab = "Test CV MSE", type = "l")
title("Ploynomial Regression CV Test")
points(which.min(deltas), min(deltas), col = "red", cex = 2, pch = 20)

#d
library(splines)
set.seed(10)
boston.spline=lm(nox ~ bs(dis, df = 4),data=Boston)
summary(spline.fit)
pred.boston=predict(spline.fit, list(dis=dis.grid))

plot(nox ~ dis, data = Boston,cex=.5,col="darkgrey")
title("Spline regression with 4 degree of freedom")
lines(dis.grid, pred.boston, col = "blue", lwd = 2)


#e
set.seed(10)
rss <- rep(NA, 20)
for (i in 3:20) {
  fit <- lm(nox ~ bs(dis, df = i), data = Boston)
  rss[i] <- sum(fit$residuals^2)
}
plot(3:20, rss[-c(1, 2)], xlab = "Degrees of freedom", ylab = "RSS", type = "l", col="red")
title("Spline RSS vs Degrees of Freedom")
points(3:20,rss[-c(1,2)], col="black",cex = 1, pch = 20)

#f
set.seed(10)
deltas = rep(NA,20)
for(i in 3:20){
  fit = glm(nox ~ bs(dis, df = i), data = Boston)
  deltas[i] = cv.glm(Boston, fit, K = 10)$delta[2]
}

plot(3:20, deltas[3:20], xlab = "Degree of freedom", ylab = "CV Test MSE", type = "l")
title("CV for Regression splines with with variable degree of freedom")
points(3:20,deltas[3:20], col="blue",cex = 1, pch = 20)
points(which.min(deltas), deltas[which.min(deltas)], col = "red", cex = 2, pch = 20)


#Q3
library(leaps)

#a
set.seed(10)
sindex <- sample(nrow(College), nrow(College) / 4)

train <- College[-sindex,]
test <- College[sindex,]
dim(train)
dim(test)
forward.selection <- regsubsets(Outstate ~ ., train, nvmax = 17, method="forward")
for.summary <- summary(forward.selection)


par(mfrow=c(1,3))
plot(for.summary$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l")
points(which.max(for.summary$adjr2),for.summary$adjr2[which.max(for.summary$adjr2)], col="red",cex=2,pch=20)
max.adjr2 <- max(for.summary$adjr2)
std.adjr2 <- sd(for.summary$adjr2)
abline(v = 10, col = "red", lty = 2)
abline(v = 8, col = "red", lty = 2)
abline(h = max.adjr2 - 0.2 * std.adjr2, col = "red", lty = 2)


plot(for.summary$cp ,xlab="Number of Variables ",ylab="Cp", type='l')
points(which.min(for.summary$cp ),for.summary$cp[which.min(for.summary$cp )],col="red",cex=2,pch=20)
min.cp <- min(for.summary$cp)
std.cp <- sd(for.summary$cp)
abline(h = min.cp + 0.2 * std.cp, col = "red", lty = 2)
abline(v = 10, col = "red", lty = 2)
abline(v = 8, col = "red", lty = 2)

plot(for.summary$bic ,xlab="Number of Variables ",ylab="BIC",type='l')
points(which.min(for.summary$bic ),for.summary$bic[which.min(for.summary$bic )],col="red",cex=2,pch=20)
min.bic <- min(for.summary$bic)
std.bic <- sd(for.summary$bic)
abline(h = min.bic + 0.2 * std.bic, col = "red", lty = 2)
abline(v = 10, col = "red", lty = 2)
abline(v = 8, col = "red", lty = 2)

names(coef(forward.sel, id = 10 ))


#b
library(gam)
gam.fit <- gam(Outstate ~ Private+s(Apps, 2)+s(Accept, 2)+s(Enroll, 2)+s(Room.Board, 2)+s(Personal, 2)+s(Terminal, 2)+s(perc.alumni, 2)+s(Expend, 2)+s(Grad.Rate, 2), data = train)

par(mfrow = c(2,3))
plot(gam.fit, se = T, col = "blue")
summary(gam.fit)

#c
predicted <- predict(gam.fit, test)
rss <- mean((test$Outstate - predicted)^2)
tss <- mean((test$Outstate - mean(test$Outstate))^2)
rsq <- 1 - rss / tss
rsq

#d
summary(gam.fit)

#Q4
set.seed(10)
N <- 100
df  <- data.frame( x1 = sample(1:1000, size = N, replace = TRUE), x2 = sample(1:1000, size = N, replace = TRUE),y=rnorm(100))

attach(df)
beta1 = 4
for (i in 1:1000) {
  a=y-beta1*x1
  beta2=lm(a ~ x2)$coef[2]
  a=y-beta2*x2
  beta1=lm(a~x1)$coef[2]
}
