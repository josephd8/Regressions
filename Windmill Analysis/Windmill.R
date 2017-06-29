#### WINDMILL ANALYSIS ####

setwd("~/3Fall2016/stat330/Homework")

wind <- read.table("Windmill.txt", header=TRUE)
head(wind)
# CSpd = measurement of wind speed (m/s) at a candidate site
# RSpd = measurement of wind speed (m/s) at the reference site


## 1 ##
Windmill companies are always looking at new sites where they might
invest in building a new wind farm. In order to determine if a
"candidate site" is a good place to build, they need to be able to
know that the windspeed at that site is high enough to create enough
energy to pay off the costs of building. However, the data
collection period can be costly in both time and money. Using
statistical modeling, we can avoid the high costs in data
by using windspeeds at "reference sites" to predict the windspeeds
at a candidate site. A reference site is a nearby site at which
the company is already monitoring the windspeed and it should be
similar to the candidate site.

## 2 ##
#Linearity
require(ggplot2)
windplot <- ggplot(wind,aes(x=RSpd,y=CSpd)) 
windplot + geom_point() + geom_smooth() + theme_grey() + 
  ggtitle("Cite Speed by Reference Speed") + xlab("Reference Speed") + 
  ylab("Cite Speed")

cor(wind$CSpd,wind$RSpd)

#Homoscedasticity
windmod <- with(wind,{lm(CSpd~RSpd)})
plot(fitted(windmod),residuals(windmod), pch=20, col='orange',
     xlab='Predicted',ylab='Residuals',main="Predicted vs Residuals")
?plot
#Normality (Q-Q Plot)
?qqnorm

#Independence

# plot(wind$CSpd, wind$RSpd, scatter.smooth(wind$CSpd, wind$RSpd,col='blue',pch=20
#                                           ,xlab="Weight", ylab="MPG",
#                                           main="MPG by Weight"))


## 4 ## 


windmod <- with(wind,{lm(CSpd~RSpd)})
names(windmod)
summary(windmod)
windmod$coefficients

with(wind,{plot(RSpd,CSpd,pch=20,col='blue',xlab="Reference",ylab="Cite",main="Cite by Reference")})
abline(reg=windmod,col='red',lwd=3)


## 5 ##

cspd <- function(x){3.14+(x*.756)}
cspd(12)

## 6 ##

cspd(30)
