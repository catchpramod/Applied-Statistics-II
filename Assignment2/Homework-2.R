# 10
data("Carseats", package = "ISLR")
head(Carseats)

#(a) Fit a multiple regression model to predict Sales using Price, Urban, and US. 
lm.carsets = lm(Sales ~ Price + Urban + US, data=Carseats)
summary(lm.carsets)

#(e) On the basis of your response to the previous question, fit a smaller model that only uses the predictors for which there is evidence of association with the outcome.

lm.carsets.new = lm(Sales ~ Price + US, data=Carseats)
summary(lm.carsets.new)

#(f)
confint(lm.carsets.new)

#(h)
par(mfrow=c(2,2))
plot(lm.carsets.new)

#15

data("Boston", package = "MASS")
head(Boston)

#(a) For each predictor, fit a simple linear regression model to predict the response. Describe your results. 
# In which of the models is there a statistically significant association between the predictor and the response? 
# Create some plots to back up your assertions.
par(mfrow=c(1,1))
input.variables <- colnames(Boston)
input.variables<- input.variables[-which(input.variables=='crim')]
lm.boston<- setNames(as.list(c(1:length(input.variables))),input.variables)
for(variable in input.variables ){
  model.formula <- as.formula(paste('crim', variable, sep="~"))
  lm.boston[[variable]] <- lm(model.formula,data=Boston)
  print(paste("Summary for predictor",variable))
  print(summary(lm.boston[[variable]]))
#   png(paste('/Users/pramod/OneDrive/STAT-702/Assignment2/',variable,".png",sep =""), width=5.5, height=4, units="in", res=300)
#   par(mfrow=c(1,1))
#   plot(Boston[[variable]], Boston$crim, xlab = variable, ylab="crim",  main = paste(variable,"vs crim plot"))
#   abline(lm.boston[[variable]], col="red",lwd=3)
#   dev.off()
#   
  png(paste('/Users/pramod/OneDrive/STAT-702/Assignment2/',variable,"-res.png",sep =""), width=5.5, height=4, units="in", res=300)
  par(mfrow=c(2,2),oma=c(0,0,2,0))
  par(mfrow=c(1,1))
  
  plot(lm.boston[[variable]])
  title(paste(variable,"residual plots"), outer=TRUE)
  dev.off()
}



#(b)

lm.boston.all <- lm(crim ~ ., data=Boston)
summary(lm.boston.all)


#(c)
input.variables <- colnames(Boston)
input.variables<- input.variables[-which(input.variables=='crim')]

coefficient <- numeric(length(input.variables))
i=1
for(var in input.variables ){
  lmf <- coefficients(lm.boston[[var]])
  coefficient[i] <- lmf[2]
  i=i+1
}

multi.coefficient <- coefficients(lm.boston.all)[-1]

plot(coefficient,multi.coefficient, xlab = "Univariate regression coefficients",
     ylab = "Multiple regression coefficients",
     main = "Univariate regression coefficients vs multiple regression coefficients")


# (d)

input.variables <- colnames(Boston)
input.variables<- input.variables[-which(input.variables=='crim')]
input.variables<- input.variables[-which(input.variables=='chas')]
lm.boston<- setNames(as.list(c(1:length(input.variables))),input.variables)
for(variable in input.variables ){
  model.formula <- as.formula(paste('crim ~ poly(', variable,',3)', sep=""))
  lm.boston[[variable]] <- lm(model.formula,data=Boston)
  print(paste("Summary for predictor",variable))
  print(summary(lm.boston[[variable]]))
}
