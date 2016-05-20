library(MASS)
library(class)

# Q3
data("Weekly", package = "ISLR")
head(Weekly)

weekly.train <- Weekly[!(Weekly$Year %in% c('2009','2010')),]
weekly.test <- Weekly[Weekly$Year %in% c('2009','2010'),]

library(mclust)
# a
weekly.MclustDA <- MclustDA(weekly.train[,3], weekly.train$Direction)
summary(weekly.MclustDA)
summary(weekly.MclustDA, newdata = weekly.test[,3], newclass = weekly.test$Direction)

# b

weekly.MclustDA <- MclustDA(weekly.train[,3], weekly.train$Direction, modelType = "EDDA")
summary(weekly.MclustDA)
summary(weekly.MclustDA, newdata = weekly.test[,3], newclass = weekly.test$Direction)



#Q4


data("Auto", package = "ISLR")

head(Auto)
Auto$mpg01 <- ifelse(Auto$mpg > median(Auto$mpg),1,0)

set.seed(5)
rands <- rnorm(nrow(Auto))
test <- rands > quantile(rands,0.75)
auto.test <- Auto[test,]
auto.train <- Auto[!test,]

head(auto.train[,c(3,4,5,7)])

auto.MclustDA <- MclustDA(auto.train[,c(3,4,5,7)], auto.train$mpg01)
summary(auto.MclustDA)
summary(auto.MclustDA, newdata = auto.test[,c(3,4,5,7)], newclass = auto.test$mpg01)

# b

auto.MclustDA <- MclustDA(auto.train[,c(3,4,5,7)], auto.train$mpg01, modelType = "EDDA")
summary(auto.MclustDA)
summary(auto.MclustDA, newdata = auto.test[,c(3,4,5,7)], newclass = auto.test$mpg01)

#Q6
credit <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data",sep = ",", 
                     col.names = c('Male',	'Age',	'Debt',	'Married',	'BankCustomer',	'EducationLevel',	'Ethnicity',	'YearsEmployed',	'PriorDefault',	'Employed',	'CreditScore',	'DriversLicense',	'Citizen',	'ZipCode',	'Income',	'Approved'))
head(credit)
str(credit)
credit <- transform(credit, Approved = ifelse(Approved == '+','yes' ,'no'))
credit$Approved<- as.factor(credit$Approved)
credit$Age <- as.numeric(as.character(credit$Age))

summary(credit)
nf<- data.frame(credit$Age,credit$Debt,credit$YearsEmployed,credit$CreditScore,credit$Income)
nf<-na.omit(nf)

a<- cor(nf, use = "complete.obs") #neglect missing values for correlation
library('corrplot') 
par(mfrow=c(1,1))
corrplot(a, method = "circle", main="Correlation between numeric variables") 


par(mfrow=c(3,2))
#credit$Age,credit$Debt,credit$YearsEmployed,credit$CreditScore,credit$Income
plot(credit$Approved, credit$Age, horizontal=TRUE, main="Approved vs Age")
plot(credit$Approved, credit$Debt, horizontal=TRUE, main="Approved vs Debt" )
plot(credit$Approved, credit$YearsEmployed, horizontal=TRUE, main="Approved vs YearsEmployed" )
plot(credit$Approved, credit$CreditScore, horizontal=TRUE, main="Approved vs CreditScore" )
plot(credit$Approved, credit$Income, horizontal=TRUE, main="Approved vs Income" )

#	'DriversLicense',	'Citizen',	'ZipCode',	'Income',
par(mfrow=c(2,2))
plot(credit$Approved, credit$Male, main="Approved vs Male")
plot(credit$Approved, credit$Married, main="Approved vs Married")
plot(credit$Approved, credit$Ethnicity, main="Approved vs Ethinicity")
plot(credit$Approved, credit$BankCustomer, main="Approved vs Bank Customer")
plot(credit$Approved, credit$EducationLevel, main="Approved vs EducationLevel")
plot(credit$Approved, credit$PriorDefault, main="Approved vs PriorDefault")
plot(credit$Approved, credit$Employed, main="Approved vs Employed")
plot(credit$Approved, credit$DriversLicense, main="Approved vs DriversLicense")
plot(credit$Approved, credit$Citizen, main="Approved vs Citizen")

