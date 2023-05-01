#Load Libraries
library(dplyr)
library(ggplot2)

#View & Review Dataset
View(MultiRegDataset)
str(MultiRegDataset)

#Basic Statistics of MultiRegDataset
summary(MultiRegDataset)

#Standard Deviation
sd(MultiRegDataset$age)
sd(MultiRegDataset$bmi)
sd(MultiRegDataset$children)
sd(MultiRegDataset$expenses)

#histogram of expenses with density line
x=MultiRegDataset$expenses
h<-hist(x, breaks=10, col="blue", xlab="Expenses",main="Histogram of Household Expenses")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="red", lwd=2)

#Conducting T-test

#Create the mean, standard deviation, and standard error
mean.x <- mean(x)
sd.x <- sd(x)
SE.x <- sd(x) / sqrt(length(x))

#Show the mean, standard deviation and standard error
mean.x
sd.x
SE.x

#State the Ho value and calculate the z-score
Ho <- 10000
z <- (mean.x - Ho) / SE.x
z

#Two-tail Test
2*pnorm(abs(z),lower.tail=FALSE)

#Linear Regression Model
#smoker vs. expenses
ggplot(MultiRegDataset,aes(x = smoker, y = expenses)) + geom_point(colour = "red")  +
  geom_smooth(method = "lm", fill = NA)

#Build Linear Model
simple.fit<-lm(expenses~smoker, data=MultiRegDataset)
LinearModel<-simple.fit

#Summary of Key Statistics of the Model
summary(LinearModel)

#Building Multiple Linear Regression Model
#Create Model1
model1 <- lm(expenses~., data = MultiRegDataset)

#Summary of Model1
print(model1)
summary(model1)


