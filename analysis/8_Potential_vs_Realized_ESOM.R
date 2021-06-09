#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Estimating potential vs realized DOM export
#Coder: Katie Wardinski (wardinskik@vt.edu)
#Created: 6/9/2021
#Updated: 
#Purpose: Thought experiment to see what realized export could be based on site 
#         hydrology in the Upland O and Wetland O horizon
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup Worskspace-----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#clear environment
remove(list=ls())

ChristDavid <- read.csv("data/ChristDavid1996.csv") 
Chow <- read.csv("data/Chow2006.csv") 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Modeling First Order Decay-----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Christ and David 1996
DOC <- ChristDavid$DOC_mgC_gSoil
time <- ChristDavid$Time_d
CD.fit <- nls(DOC ~ C0*exp(-k*time),
              start=list(C0=0.6,k=0.01))
summary(CD.fit,corr=T) #k = 0.014
confint(CD.fit)
qqnorm(resid(CD.fit))
qqline(resid(CD.fit))
shapiro.test(resid(CD.fit))
plot(fitted(CD.fit),resid(CD.fit), pch=19, xlab="Predicted
conc", ylab = "residual", main="Residual plot")
abline(h=0, lty="dashed")

plot(time,DOC,pch=19,ylab="DOC (mg C/g soil)",xlab="Time (d)")
tm <- seq(0,70,by=5)
new.time <- data.frame(time=tm)
predic.DOC <- predict(CD.fit,new.time)
plot(tm,predic.DOC,type="l",ylab="DOC (mg C/g soil)",xlab="Time (d)",
     main="Christ & David 1996 - First Order Decay")
points(time, DOC, pch=19)

#Chow 2006
#Christ and David 1996
DOC <- Chow$DOC_mgC_gSoil
time <- Chow$Time_d
CD.fit <- nls(DOC ~ C0*exp(-k*time),
              start=list(C0=0.4,k=0.01))
summary(CD.fit,corr=T) #k = 0.007
confint(CD.fit)
qqnorm(resid(CD.fit))
qqline(resid(CD.fit))
shapiro.test(resid(CD.fit))
plot(fitted(CD.fit),resid(CD.fit), pch=19, xlab="Predicted
conc", ylab = "residual", main="Residual plot")
abline(h=0, lty="dashed")

plot(time,DOC,pch=19,ylab="DOC (mg C/g soil)",xlab="Time (d)")
tm <- seq(0,70,by=5)
new.time <- data.frame(time=tm)
predic.DOC <- predict(CD.fit,new.time)
plot(tm,predic.DOC,type="l",ylab="DOC (mg C/g soil)",xlab="Time (d)",
     main="Christ & David 1996 - First Order Decay")
points(time, DOC, pch=19)

#Average K values
k = (0.014+0.007)/2
