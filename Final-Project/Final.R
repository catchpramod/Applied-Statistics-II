data.initial <- read.csv("/Users/pramod/OneDrive/STAT-702/Final-Project/data1.csv")
str(data.initial)
data<- data.initial
data$shot_made_flag <- as.factor(data$shot_made_flag)
data$game_event_id <- as.factor(data$game_event_id)
data$game_id <- as.factor(data$game_id)
data$playoffs <- as.factor(data$playoffs)
data$team_id <- as.factor(data$team_id)
data$shot_id <- as.factor(data$shot_id)
data$period <- as.factor(data$period)
#transform the matchup column to either home or away
data$matchup<- ifelse(grepl( "@" , data$matchup),"Away","Home")
data$matchup <- as.factor(data$matchup)

data.known <- data[!is.na(data$shot_made_flag), ]

library(ggplot2)
library('corrplot') 
# library(gridExtra)
ggplot( data.known, aes(x=lon, y=lat, color=shot_made_flag)) + geom_point(shape=10, size=1)
cordf <- data.frame("lat"=data.known$lat, "lon"=data.known$lon, "loc_x" = data.known$loc_x, "loc_y" = data.known$loc_y, 
                    "minutes_remaining"= data.known$minutes_remaining,"seconds_remaining"= data.known$seconds_remaining, 
                    "shot_distance" =data.known$shot_distance) 
head(cordf)
c<- cor(cordf, use = "complete.obs")
corrplot(c, method = "circle") 

dnum <- data.frame("shot_made_flag" = data.known$shot_made_flag, "loc_x" = data.known$loc_x, "loc_y" = data.known$loc_y, 
                    "minutes_remaining"= data.known$minutes_remaining,"seconds_remaining"= data.known$seconds_remaining) 

dcat <- data.frame("shot_made_flag" = data.known$shot_made_flag,data.known$action_type,data.known$combined_shot_type,data.known$period
                   ,data.known$playoffs,data.known$season,data.known$shot_type,data.known$shot_zone_area,data.known$shot_zone_basic
                   ,data.known$shot_zone_range,data.known$matchup,data.known$opponent) 

x11()
pairs(dcat, col=dcat$shot_made_flag)
pairs(dnum, col=dnum$shot_made_flag)
dev.off()

par(mfrow=c(2,2))
mosaicplot(table(data.known$action_type, data.known$shot_made_flag), col= data.known$shot_made_flag, main="shot_made_flag vs action_type")
mosaicplot(table(data.known$combined_shot_type, data.known$shot_made_flag), col= data.known$shot_made_flag , main="shot_made_flag vs combined_shot_type")
mosaicplot(table(data.known$playoffs, data.known$shot_made_flag), col= data.known$shot_made_flag , main="shot_made_flag vs playoffs")
mosaicplot(table(data.known$season, data.known$shot_made_flag), col= data.known$shot_made_flag , main="shot_made_flag vs season")
mosaicplot(table(data.known$shot_type, data.known$shot_made_flag), col= data.known$shot_made_flag , main="shot_made_flag vs shot_type")
mosaicplot(table(data.known$shot_zone_area, data.known$shot_made_flag), col= data.known$shot_made_flag , main="shot_made_flag vs shot_zone_area")
mosaicplot(table(data.known$shot_zone_basic, data.known$shot_made_flag), col= data.known$shot_made_flag , main="shot_made_flag vs shot_zone_basic")
mosaicplot(table(data.known$shot_zone_range, data.known$shot_made_flag), col= data.known$shot_made_flag , main="shot_made_flag vs shot_zone_range")
mosaicplot(table(data.known$matchup, data.known$shot_made_flag), col= data.known$shot_made_flag , main="shot_made_flag vs matchup")
mosaicplot(table(data.known$opponent, data.known$shot_made_flag), col= data.known$shot_made_flag , main="shot_made_flag vs opponent")

#remove the columns identified in exploration from dataset
dropcols <- c("lat", "lon",  "shot_distance", "game_id", "game_event_id", "team_id", "team_name", "game_date","shot_id")
data.known <- data.known[ , !(names(data.known) %in% dropcols)]
#variable selection
#stepwise selection
vs.log.fit <- glm(shot_made_flag~ ., family = binomial, data= data.known)
step(vs.log.fit)


library(Hmisc)
# merge and reduce levels for action_type
mdata <- data
mdata$action_type <- combine.levels(mdata$action_type, minlev = 0.001)
mdata <- mdata[ , !(names(mdata) %in% dropcols)]
keepcols <- c("shot_made_flag","loc_y" ,"shot_type","seconds_remaining","period",
              "shot_zone_area","minutes_remaining","season","shot_zone_range","action_type","matchup")
mdata <- mdata[ , (names(mdata) %in% keepcols)]

#divide into known(test and train), predict
data.known <- mdata[!is.na(mdata$shot_made_flag), ]
data.predict <- mdata[is.na(mdata$shot_made_flag), ]


obs <- dim(data.known)[1]
set.seed(702)
testSample <- sample(obs,obs/3.3)
data.train <-  data.known[-testSample,]
data.test <-  data.known[testSample,]

# Logistic regression
formula <- as.formula(shot_made_flag ~ loc_y + shot_type + seconds_remaining + period + shot_zone_area 
                      + minutes_remaining + season + shot_zone_range + action_type + matchup)
set.seed(702)
logical.fit <- glm(formula , family = binomial, data= data.train)
logical.predict <- predict(logical.fit, newdata = data.test, type= "response")
logical.response <- ifelse(logical.predict >=0.5, 1, 0)
logical.matrix <- table(logical.response, data.test$shot_made_flag)
mean(logical.response!=data.test$shot_made_flag)

library(ROCR)
fp.logical = prediction(logical.predict,data.test$shot_made_flag)
fpf.logical = performance(fp.logical,"tpr","fpr")
performance(fp.logical, measure = "auc")@y.values[[1]]
plot(fpf.logical,col="red",lwd=2,main="ROC Curve for Logistic Regression")
abline(a=0,b=1,lwd=2,lty=2,col="gray")


# Decision Tree
library(tree)
set.seed(702)
# Create a decision tree model using the target field as the response and all 93 features as inputs
dt.fit <- tree(formula, data=data.train)
plot(dt.fit)
title(main="Decision Tree")
text(dt.fit)
# Test the tree model on the holdout test dataset
dt.predict <- predict(dt.fit, data.test, type="class")
table(dt.predict,data.test$shot_made_flag)
mean(dt.predict!= data.test$shot_made_flag)

dt.predict.roc <- predict(dt.fit, data.test)[,2]
fp.dt = prediction(dt.predict.roc,data.test$shot_made_flag)
fpf.dt = performance(fp.dt,"tpr","fpr")
performance(fp.dt, measure = "auc")@y.values[[1]]



# Random Forest
library("randomForest")
set.seed(702)
rf.fit <- randomForest(formula, data=data.train, mtry = 3, ntree = 500, importance = TRUE)
rf.predict <- predict(rf.fit, newdata = data.test)
rf.matrix <- table(rf.predict, data.test$shot_made_flag)
mean(rf.predict!= data.test$shot_made_flag)

rf.predict.roc <- predict(rf.fit, newdata = data.test, type = "prob")[,2]
fp.rf = prediction(rf.predict.roc,data.test$shot_made_flag)
fpf.rf = performance(fp.rf,"tpr","fpr")

plot(rf.fit)
performance(fp.rf, measure = "auc")@y.values[[1]]



# SVM
library(e1071)
set.seed(702)
svm.linear.tune = tune(svm, formula, data = data.train, kernel = "linear", ranges = list(cost = c( 0.01, 0.1, 1, 5, 10)))
summary(svm.linear.tune)
# - best parameters:
#   cost
# 0.1

set.seed(702)
svm.fit <- svm(formula, data = data.train, kernel = "linear", cost = 0.1)
svm.predict <- predict(svm.fit, data.test)
svm.matrix <- table(svm.predict, data.test$shot_made_flag)
mean(svm.predict!= data.test$shot_made_flag)

set.seed(702)
svm.fit.roc <- svm(formula, data = data.train, kernel = "linear", cost = 0.1, probability=TRUE)
svm.predict.roc <- predict(svm.fit.roc, data.test, probability = TRUE)
fp.svm = prediction(attr(svm.predict.roc, "probabilities")[,1],data.test$shot_made_flag)
fpf.svm = performance(fp.svm,"tpr","fpr")

performance(fp.svm, measure = "auc")@y.values[[1]]
plot(fpf.svm,col="red",lwd=2,main="ROC Curve for SVM")
abline(a=0,b=1,lwd=2,lty=2,col="gray")


#LDA
library(MASS)
#LDA
lda.fit<-lda(formula, data = data.train)
lda.predict <- predict(lda.fit, data.test)
table(lda.predict$class, data.test$shot_made_flag)
mean(lda.predict$class!=data.test$shot_made_flag)
fp.lda = prediction(lda.predict$posterior[,2],data.test$shot_made_flag)
fpf.lda = performance(fp.lda,"tpr","fpr")
performance(fp.lda, measure = "auc")@y.values[[1]]
plot(fpf.svm,col="red",lwd=2,main="ROC Curve for LDA")
abline(a=0,b=1,lwd=2,lty=2,col="gray")



#plotting ROC for the models
plot(fpf.logical, main="ROC", colorize=T)
plot(fpf.logical, col=1, add=TRUE)
plot(fpf.rf, col=2, add=TRUE)
plot(fpf.dt, col=3, add=TRUE)
plot(fpf.svm, col=4, add=TRUE)
plot(fpf.lda, col=5, add=TRUE)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
legend(0.7, 0.3, c('logistic','rforest','dtree', 'svm', 'lda'), 1:5)

# Logistic model summary

summary(logical.fit)
# predict the unknown records

predict.out <- predict(logical.fit, newdata = data.predict, type= "response")
predict.class <- ifelse(logical.predict >=0.5, 1, 0)
summary(as.factor(predict.class))

head(predict.class)
data.unknown <- data[is.na(data$shot_made_flag), ]

shot_id = data.unknown$shot_id
shot_made_flag = predict.class

result <- data.frame(shot_id, shot_made_flag)
head(result)
#write to result
write.csv(result, "/Users/pramod/OneDrive/STAT-702/Final-Project/result.csv", row.names = FALSE)

