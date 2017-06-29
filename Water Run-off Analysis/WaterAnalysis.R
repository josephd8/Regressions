setwd("~/3Fall2016/stat330/Homework")

# Read in Data #
water <- read.table("water.txt",header=TRUE)
head(water)

# Libraries #
library(lmtest)
library(MASS)

# Scatterplot  and Correlation #
require(ggplot2)
water.plot <- ggplot(water,aes(x=Precip,y=Runoff)) 
water.plot + geom_point() + geom_smooth() + theme_grey() + 
  ggtitle("Runoff by Precipitation") + 
  xlab("Precipitation") + ylab("Runoff")


with(water,{cor(Precip,Runoff)})


# Centered Model and Regular Model#
water$cent <- water$Precip - mean(water$Precip)

lm.water <- with(water,{lm(Runoff~Precip)})
lm.cent <- with(water,{lm(Runoff~cent)})

summ <- summary(lm.cent)
summary(lm.water)


# Assumptions # 
plot(lm.water$fitted.values,lm.water$residuals,
     main="Residuals vs. Fitted Values",
     xlab="Fitted Values",ylab="Residuals")
abline(h=0)

hist(stdres(lm.water),main="Histogram of Residuals",xlab="Residuals")


# Plot with Regression Line #
plot(x=water$cent,water$Runoff,
     main="Runoff by Precipitation",
     xlab="Precipitation - Average Precipitation",
     ylab="Runoff")
abline(reg=lm.cent,col="blue",lwd=3)


# Centered Assumptions #
require(ggplot2)
cent.plot <- ggplot(water,aes(x=cent,y=Runoff)) 
cent.plot + geom_point() + geom_smooth() + theme_grey() + 
  ggtitle("Runoff by Precipitation") + 
  xlab("Precipitation") + ylab("Runoff")

with(water,{cor(cent,Runoff)})

plot(lm.cent$fitted.values,lm.cent$residuals,
     main="Residuals vs. Fitted Values",
     xlab="Fitted Values",ylab="Residuals")

hist(stdres(lm.cent),main="Histogram of Residuals",xlab="Residuals")

# Cross-Validation #
pred.width <- numeric(0)
coverage <- numeric(0)
bias <- numeric(0)
rpmse <- numeric(0)
for (i in 1:1000){
  n <- 4
  sampl <- sample(1:length(water$cent),n)
  train <- water[-sampl,]
  test <- water[sampl,]
  train.lm <- with(train,{lm(Runoff~cent)})
  pred.cent <- data.frame(cent=test$cent,Runoff=test$Runoff)
  pred.run <- predict.lm(train.lm,pred.cent)
  pred.cent$predrun <- pred.run
  bias[i] <- mean(pred.cent$predrun - pred.cent$Runoff)
  rpmse[i] <- sqrt(mean((pred.cent$predrun - pred.cent$Runoff)^2))
  
  pred.int <- predict.lm(train.lm,test,
                          interval="prediction",level=.95)
  covers <- mean(pred.int[,2] < test$Runoff & 
                   test$Runoff < pred.int[,3])
  coverage[i] <- covers
  int.width <- mean(pred.int[,3] - pred.int[,2])
  pred.width[i] <- int.width
  
  
}
mean(bias)
mean(rpmse)
mean(coverage)
mean(pred.width)


# Parameter Confidence Intervals #
conf.parms <- confint(lm.cent)
conf.parms


# A few Predictions #
preds <- data.frame(cent=c(4.5-mean(water$Precip)))
preds$pred <- predict.lm(lm.cent,newdata=preds,
                               interval='prediction',level=.95)
preds

