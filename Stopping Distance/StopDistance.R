setwd("~/3Fall2016/stat330/Homework")

## Read in the Data ##
stops <- read.table("StoppingDistance.txt",header=TRUE)
head(stops)

## Assumptions ##
library(lmtest)
library(MASS)
require(ggplot2)
sdplot <- ggplot(stops,aes(x=Speed,y=Distance)) 
sdplot + geom_point() + geom_smooth() + theme_grey() + 
  ggtitle("Stopping Distance by Speed") + 
  xlab("Speed") + ylab("Stopping Distance")

with(stops,{cor(Speed,Distance)})


lm.sd <- with(stops,{lm(Distance ~ Speed)})
names(lm.sd)

plot(lm.sd$fitted.values,lm.sd$residuals,
     main="Residuals vs. Fitted Values",
     xlab="Fitted Values",ylab="Residuals")

hist(stdres(lm.sd),main="Histogram of Residuals",xlab="Residuals")

## Transformation (Square Root) ##
lm.sq <- with(stops,{lm(sqrt(Distance)~sqrt(Speed))})
lm.log <- with(stops,{lm(log(Distance)~log(Speed))})
require(ggplot2)
sqplot <- ggplot(stops,aes(x=sqrt(Speed),y=sqrt(Distance))) 
sqplot + geom_point() + geom_smooth() + theme_grey() + 
  ggtitle("Stopping Distance by Speed (Square Root)") + 
  xlab("Speed") + ylab("Stopping Distance")

plot(lm.sq$fitted.values,lm.sq$residuals,
     main="Residuals vs. Fitted Values (Square Root)",
     xlab="Fitted Values",ylab="Residuals")
plot(lm.log$fitted.values,lm.sq$residuals,
     main="Residuals vs. Fitted Values (Log)",
     xlab="Fitted Values",ylab="Residuals")

hist(stdres(lm.sq),main="Histogram of Residuals (Square Root)",xlab="Residuals")
hist(stdres(lm.log),main="Histogram of Residuals (Log)",xlab="Residuals")

## Model Fit and Predictive Accuracy ##
lm.sq <- with(stops,{lm(sqrt(Distance)~sqrt(Speed))})
summary(lm.sq)
names(lm.sq)


bias <- numeric(0)
rpmse <- numeric(0)
for (i in 1:100){
  n <- 9
  sampl <- sample(1:62,n)
  train <- stops[-sampl,]
  test <- stops[sampl,]
  train.lm <- with(train,{lm(sqrt(Distance)~sqrt(Speed))})
  pred.speed <- data.frame(Speed=test$Speed,Distance=test$Distance)
  pred.sd <- (predict.lm(train.lm,pred.speed))^2
  pred.speed$pred.dist <- pred.sd
  bias[i] <- (1/n)*(sum(pred.speed$pred.dist - pred.speed$Distance))
  rpmse[i] <- sqrt((1/n)*(sum((pred.speed$pred.dist - pred.speed$Distance)^2)))
}
hist(bias)
hist(rpmse)
mean(bias)
mean(rpmse)

# 
# with(stops,{plot(sqrt(Speed),sqrt(Distance),pch=19,main="Stopping Distance by Speed",
#                  xlab="Speed",ylab="Distance")})
# abline(a=lm.sq$coefficients[1],b=lm.sq$coefficients[2],col='red')


## Regression line on the data ## 
## 
with(stops,{plot(Speed,Distance,pch=19,main="Stopping Distance by Speed",
                 xlab="Speed",ylab="Distance")})
pred.speed <- data.frame(Speed=seq(0,40,length=100))
pred.sd <- (predict.lm(lm.sq,newdata=pred.speed))^2
lines(pred.speed$Speed,pred.sd,col='red',lwd=3)



