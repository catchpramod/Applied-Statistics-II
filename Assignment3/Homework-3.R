# 10
data("Weekly", package = "ISLR")
head(Weekly)

#a
summary(Weekly)
cor(Weekly[,-9])
plot(Weekly$Volume, Weekly$Year, ylab = "Year", xlab ="Volume of shares traded", cex = .5, col = "red")


#b

weekly.glm <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, family = binomial)
summary(weekly.glm)

#c
weekly.prob <- predict(weekly.glm,type="response")
l<- dim(Weekly)
weekly.pred=rep("Down",l[1:1])
weekly.pred[weekly.prob>.5]="Up"
cm <- table(weekly.pred,Weekly$Direction)
mean(weekly.pred == Weekly$Direction)
length(weekly.pred[weekly.pred=='Up'])

#d
weekly.train <- Weekly[!(Weekly$Year %in% c('2009','2010')),]
weekly.test <- Weekly[Weekly$Year %in% c('2009','2010'),]
unique(weekly.train$Year)
unique(weekly.test$Year)

weekly.fit.train <- glm(Direction ~ Lag2, family=binomial, data=weekly.train)
summary(weekly.fit.train)
weekly.test.prob <- predict(weekly.fit.train,weekly.test,type="response")

weekly.test.pred <- rep("Down",nrow(weekly.test))
weekly.test.pred[weekly.test.prob > 0.50]="Up"
table(weekly.test.pred,weekly.test$Direction)
mean(weekly.test.pred==weekly.test$Direction)


#11

#a

data("Auto", package = "ISLR")

head(Auto)
Auto$mpg01 <- ifelse(Auto$mpg > median(Auto$mpg),1,0)

#b
cor(Auto[,-9])
pairs(Auto)
par(mfrow=c(2,4))
boxplot(acceleration ~ mpg01, data = Auto, main = "Acceleration vs mpg01")
boxplot(cylinders ~ mpg01, data = Auto, main = "Cylinders vs mpg01")
boxplot(displacement ~ mpg01, data = Auto, main = "Displacement vs mpg01")
boxplot(horsepower ~ mpg01, data = Auto, main = "Horsepower vs mpg01")
boxplot(weight ~ mpg01, data = Auto, main = "Weight vs mpg01")
boxplot(year ~ mpg01, data = Auto, main = "Year vs mpg01")
boxplot(origin ~ mpg01, data = Auto, main = "Origin vs mpg01")
boxplot(mpg ~ mpg01, data = Auto, main = "mpg vs mpg01")


#c
set.seed(5)
rands <- rnorm(nrow(Auto))
test <- rands > quantile(rands,0.75)
auto.test <- Auto[test,]
auto.train <- Auto[!test,]
nrow(Auto)
nrow(auto.test)
nrow(auto.train)

#f
auto.fit.train <- glm(mpg01 ~ displacement + horsepower + weight + year, family=binomial, data=auto.train)
summary(auto.fit.train)
auto.test.prob <- predict(auto.fit.train,auto.test,type="response")
auto.test.pred <- rep(0,nrow(auto.test))
auto.test.pred[auto.test.prob > 0.50]=1
tt = table(auto.test.pred,auto.test$mpg01)
mean(auto.test.pred!=auto.test$mpg01)


#4

calculateMetrics <- function(cutoff, pd.prob, org.resp){
  prediction <- rep(contrasts(org.resp)[1,1],length(org.resp))
  prediction[pd.prob > cutoff] = contrasts(org.resp)[2,1]
  tab <- table(prediction, org.resp)
  mcrate<- (tab[2,1]+tab[1,2])/(sum(tab))
  sensitivity <- tab[2,2]/(tab[2,1]+tab[2,2])
  specificity <- tab[1,1]/(tab[1,1]+tab[1,2])
  retval <- c(mcrate,sensitivity,specificity)
  return(retval)
}


# calculateMetrics(0.5,weekly.test.prob,weekly.test$Direction)

