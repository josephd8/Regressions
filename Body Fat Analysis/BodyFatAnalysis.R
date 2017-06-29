setwd("~/3Fall2016/stat330/data")

# Data and Original Model #
fat <- read.table("BodyFat.txt",header=TRUE)
head(fat)
lm.fat <- lm(brozek ~ .,data=fat)

# Exploratory graphics and such #
plot(fat$age,fat$brozek,main="Body Fat by Age",
     xlab="Age",ylab="Body Fat")
plot(fat$weight,fat$brozek,main="Body Fat by Weight",
     xlab="Weight",ylab="Body Fat")
plot(fat$forearm,fat$brozek,main="Body Fat by \nForearm Circumference",
     xlab="Forearm",ylab="Body Fat")


# Assumptions 
pairs(fat)
library(corrplot)
fa <- cor(fat)
corrplot(fa, method="number")

plot(lm.fat$fitted.values,lm.fat$residuals,
     main="Residuals vs. Fitted Values",
     xlab="Fitted Values",ylab="Residuals")
abline(h=0,col='red')

library(MASS)
hist(stdres(lm.fat),main="Histogram of Residuals",xlab="Residuals")

library(car)
avpl <- avPlots(lm.fat)


# Table of Coefficients 
summary(lm.fat)
#install.packages("sjPlot")
library(sjPlot)
sjt.lm(lm.fat)

# Cross-Validation

pred.width <- numeric(0)
coverage <- numeric(0)
bias <- numeric(0)
rpmse <- numeric(0)
for (i in 1:1000){
  n <- 25
  sampl <- sample(1:length(fat$brozek),n)
  
  train <- fat[-sampl,]
  test <- fat[sampl,]
  train.lm <- lm(brozek ~ .,data=fat)
  pred.fat <- test
  pred.bro <- predict.lm(train.lm,pred.fat)
  pred.fat$predbrozek <- pred.bro
  bias[i] <- mean(pred.fat$predbro - pred.fat$brozek)
  rpmse[i] <- sqrt(mean((pred.fat$predbro - pred.fat$brozek)^2))
  
  pred.int <- predict.lm(train.lm,test,
                         interval="prediction",level=.95)
  covers <- mean(pred.int[,2] < test$brozek & 
                   test$brozek < pred.int[,3])
  coverage[i] <- covers
  int.width <- mean(pred.int[,3] - pred.int[,2])
  pred.width[i] <- int.width
}
mean(bias)
mean(rpmse)
mean(coverage)
mean(pred.width)


# Prediction and Error Estimation
dframe <- data.frame(age=c(50),weight=c(203),
                     height=c(67),neck=c(40.2),
                     chest=c(114.8),abdom=c(108.1),
                     hip=c(102.5),thigh=c(61.3),
                     knee=c(41.1),ankle=c(24.7),
                     biceps=c(34.1),forearm=c(31),
                     wrist=c(18.3))
predict.lm(lm.fat,newdata=dframe,interval="prediction",
           level=.95)


