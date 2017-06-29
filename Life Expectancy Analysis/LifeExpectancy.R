

setwd("~/3Fall2016/stat330/data")

life <-read.table("LifeExpectancy.txt", header=TRUE)
head(life)


### Choosing Base Line ####
unique(life$Group)
life$Group <- factor(life$Group, levels=c("other", "africa", "oecd"))

#### Exploring Data with various plots ####
plot(LifeExp~. , data=life)

### Plotting Transformed Data ####
my.colors <- c("blue", "red", "orange")
plot(log(life$PPGDP), life$LifeExp, main = 
       "GDP vs Life Expectancy", xlab= "Log GDP Per Person", ylab="Life Expectancy",
     col=my.colors[life$Group], pch=19)

#### Exploring Boxplots (categorical data) ###
boxplot(life$LifeExp ~ life$Group, xlab="Group", ylab="Life Expectancy",
        main="Life Expectancy by Group")

#### Building Multiple Linear Model ###
life.lm <- lm(LifeExp~ Group + log(PPGDP)+ Group:log(PPGDP), data=life)

#### Evaluating Assumptions ####
library(car)
avPlots(life.lm)

with(life.lm, plot(fitted.values, residuals, pch=19, col= "steelblue", 
                   main="Fitted Values vs Residuals Plot", xlab="Fitted Values", ylab="Residuals"))
abline(h=0, col="red")

##### BP Test validates our homoskedicity ###
library(lmtest)
bptest(life.lm) ## P Value > 0.05 means data is homeskedastic ###

library(MASS)
stand.res <-stdres(life.lm)
hist(stand.res, main="Histogram of Standard Residuals", xlab=" Standard Residuals")
cooks <- cooks.distance(life.lm)
which(cooks > 4/length(stand.res))
ks.test(stand.res, "pnorm") ### Validates Normality ###

### R-squared and other coefficients #### 
summary(life.lm)
sum.life <- summary(life.lm)
coef(sum.life)
sum.life$r.squared


### Running F-Test to test to see if interaction term is significant ###
reduced.lm <- lm(LifeExp ~ Group+log(PPGDP), data=life)
anova(life.lm, reduced.lm)
summary(reduced.lm)

## Confidence Intervals ####
library(sjPlot)
sjt.lm(life.lm)
confint(life.lm, level=.95)

#### Plotting Regression Line to Fit Data ####
plot(log(life$PPGDP), life$LifeExp, main = 
       "GDP vs Life Expectancy", xlab= "Log GDP Per Person", ylab="Life Expectancy",
     col=my.colors[life$Group], pch=19)
legend("topleft", legend=c("other", "africa", "oecd"), 
       col=c("blue", "red", "orange"), lty=1, lwd=3)

coef(life.lm)
### PPGDP and other ### 
abline(coef(life.lm)[1], coef(life.lm)[4], col = "blue", lwd=2)
### PPGDP and africa ####
abline(sum(coef(life.lm)[c(1,2)]),sum(coef(life.lm)[c(4,5)]), col="red", lwd=2)
### PPGDP and oecd ###
abline(sum(coef(life.lm)[c(1,3)]),sum(coef(life.lm)[c(4,6)]), col="orange", lwd=2)


