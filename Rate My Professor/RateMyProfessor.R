setwd("~/3Fall2016/stat330/data")

#### Read in the Data ####
rmp<- read.table("ratemyprofessor.txt", header=TRUE)
head(rmp)
names(rmp)
rmp <- rmp[,c(1:7,9:31,8)]
str(rmp)
head(rmp)
library(car)
mlr <- lm(quality ~.,data=rmp)
pairs(rmp[,c(2:4,6:7,31)])
cor(rmp[,c(2:4,6:7,31)]) # Just take bottom row excluding quality #

### Boxplots ###
rate<- read.table("ratemyprofessor.txt", header=TRUE)
rate$Discipline<-"Other"
rate$Discipline[which(rate$disciplinePre.prof=="Yes")]<-"Pre.Prof"
rate$Discipline[which(rate$disciplineSocSci=="Yes")]<-"SocSci"
rate$Discipline[which(rate$disciplineSTEM=="Yes")]<-"STEM"
rate$Discipline<-factor(rate$Discipline, levels=c("Other","Pre.Prof","SocSci","STEM"))

rate$Dept<-"Other"
rate$Dept[which(rate$deptBiology=="Yes")]<-"Biology"
rate$Dept[which(rate$deptBusiness=="Yes")]<-"Business"
rate$Dept[which(rate$deptChemistry=="Yes")]<-"Chemistry"
rate$Dept[which(rate$deptCommunication=="Yes")]<-"Communication"
rate$Dept[which(rate$deptCS=="Yes")]<-"CS"
rate$Dept[which(rate$deptEducation=="Yes")]<-"Education"
rate$Dept[which(rate$deptEnglish=="Yes")]<-"English"
rate$Dept[which(rate$deptGeology=="Yes")]<-"Geology"
rate$Dept[which(rate$deptHistory=="Yes")]<-"History"
rate$Dept[which(rate$deptKins=="Yes")]<-"Kins"
rate$Dept[which(rate$deptLanguages=="Yes")]<-"Languages"
rate$Dept[which(rate$deptMath=="Yes")]<-"Math"
rate$Dept[which(rate$deptMusic=="Yes")]<-"Music"
rate$Dept[which(rate$deptPhilosophy=="Yes")]<-"Philosophy"
rate$Dept[which(rate$deptPhysics=="Yes")]<-"Physics"
rate$Dept[which(rate$deptPolySci=="Yes")]<-"PolySci"
rate$Dept[which(rate$deptPsychology=="Yes")]<-"Psychology"
rate$Dept[which(rate$deptReligion=="Yes")]<-"Religion"
rate$Dept[which(rate$deptSocialScience=="Yes")]<-"SocialScience"
rate$Dept[which(rate$deptSociology=="Yes")]<-"Sociology"
rate$Dept<-factor(rate$Dept, levels=c("Other","Biology","Business","Chemistry","Communication","CS","Education","English","Geology","History","Kins","Languages","Math","Music","Philosophy","Physics","PolySci","Psychology","Religion","SocialScience","Sociology"))
head(rate)
plot(rate$quality ~ rate$Discipline,ylab="Quality",xlab="Discipline")
plot(rate$quality ~ rate$Dept,xlab="Department",ylab="Quality")
#### 

#### Model ####
#install.packages("bestglm")
library(bestglm)
rmp.bic <- bestglm(rmp,IC="BIC",method="exhaustive")
mlr <- rmp.bic$BestModel
mlr
summary(mlr)
#compare to the AIC #
rmp.aic <- bestglm(rmp,IC="AIC",method="exhaustive")
mlr.aic <- rmp.aic$BestModel
summary(mlr.aic)
plot(rmp.bic$Subsets$BIC,type="b",pch=19,xlab="# of Vars", ylab="AIC")
# Confint #
confint(mlr)
library(MASS)

#### Variation Inflation Factors ####
library(car)
lm.all <- lm(quality~.,data=rmp)
vif(lm.all)
# VIF > 10 is an issue
# for disciplineSTEM, it is probably collinear with deptMath, so we will 
#just include one in the model, this is probably the reason why all of the 
#VIF's for disciplines are high

#### Table with Confidence Intervals ###
library(sjPlot)
sjt.lm(mlr)
confint(life.lm, level=.95)

#### Checking Assumptions #### 
residual <- stdres(mlr)
# Hist std resid #
hist(residual) 
ks.test(residual,"pnorm") #(high p-value means its good)
# Fitted Values vs Std Res #
plot(mlr$fitted.values,residual,xlab="Fitted Values",ylab="Residuals") 
abline(h=0)
# Equal Variance #
library(lmtest)
bptest(mlr) ## P-value is high so equal variance is met ###

#### Prediction ####
predict.lm(mlr,newdata=data.frame(numYears= 3, numRaters= 15, numCourses= 2, pepper="no",disciplineSTEM="Yes", 
                                  deptBusiness="No",deptPhysics="No",dept="Math", easiness= 4.1, raterInterest=4.2), interval="prediction",level=.95)


