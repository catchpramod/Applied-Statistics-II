#read college.csv

college <- read.csv("/Users/pramod/OneDrive/STAT-702/Assignment1/College.csv")
head(college)
rownames(college) <- college[,1]
fix(college)
college<-college[,-1]
fix(college)
summary(college)
pairs(college[,1:10])
plot(college$Outstate,college$Private)
boxplot(Outstate ~ Private, data = college, ylab = "Outstate", xlab= "Private")

Elite <- rep("No",nrow(college))
Elite[college$Top10perc >50] <- "Yes"
Elite <- as.factor(Elite)
college <- data.frame(college ,Elite)
head(college)
summary(college)
boxplot(Outstate ~ Elite, data = college, ylab = "Outstate", xlab= "Elite")


par(mfrow=c(2,2))
hist(college$Outstate, col = 2, breaks = 50, xlab = "Outstate", ylab = "Count")
hist(college$PhD, col = 3, breaks = 50, xlab = "PhD", ylab = "Count")
hist(college$Grad.Rate, col = 4, breaks = 50, xlab = "Grad Rate", ylab = "Count")
hist(college$perc.alumni, col = 6, breaks = 50, xlab = "Percentage alumni who donate", ylab = "Count")

par(mfrow=c(1,0))

plot(college[,18],col= 2, pch = 19, cex = .6, lty = "solid", lwd = .1)


