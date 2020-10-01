#ISLR ch7 exercises#
library(ISLR)
attach(Wage)

#polynomial reg and step fxn
fit=lm(wage~poly(age ,4) ,data=Wage)
coef(summary (fit))
summary(fit)
#can fit the polynomials directly istead of linear combinations
#which is what the previous command does
fit=lm(wage~poly(age ,4, raw =TRUE) ,data=Wage)
coef(summary (fit))
summary(fit)

#can fit other ways too
fit2a=lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage)
coef(fit2a)

#create a grid of ages based on our range of age, and make predictions w/ them to see!
agelims =range(age)
age.grid=seq(from=agelims [1],to=agelims [2])
preds=predict (fit ,newdata =list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)

#plots for our commands
par(mfrow=c(1,2),mar=c(4.5,4.5,1,1) ,oma=c(0,0,4,0))
plot(age ,wage ,xlim=agelims ,cex =.5,col=" darkgrey ")
title(" Degree -4 Polynomial ",outer=T)
lines(age.grid ,preds$fit ,lwd=2,col="blue")
matlines (age.grid ,se.bands ,lwd=1, col=" blue",lty=3)

#using ANOVA to test what level of polynomial is needed
fit.1=lm(wage~age ,data=Wage)
fit.2=lm(wage~poly(age ,2),data=Wage)
fit.3=lm(wage~poly(age ,3),data=Wage)
fit.4=lm(wage~poly(age ,4),data=Wage)
fit.5=lm(wage~poly(age ,5),data=Wage)
anova(fit.1,fit.2,fit.3,fit.4,fit.5)

#if fit using poly, we can look at our model coeff significance directly (since it computes orthogonal polys)

#here we use the I wrapper to create our binary response variable 'on the fly'
fit=glm(I(wage >250)~poly(age ,4),data=Wage , family=binomial )
#generate predictions using the prds fxn
preds=predict (fit ,newdata =list(age=age.grid),se=T)
#and we get predictions for the LOGIT

#thus, we need to xform our confidence intervals for our actual prob, instead of the logit!
pfit=exp(preds$fit )/(1+exp(preds$fit ))
se.bands.logit = cbind(preds$fit +2* preds$se.fit , preds$fit -2*preds$se.fit)
se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))

#fitting Step fxn using cut fxn
#cut tells us what cut points make sense if we want 4 of them
table(cut(age ,4))

fit=lm(wage~cut(age ,4),data=Wage)
coef(summary (fit))
#intercept = avg salary for below 33 yrs of age, each category is just 'added on' to our intercept
#b/c the other cut points are coded in as dummy variables.

#fitting Splines
library(splines)
#knots prespec'd at 25,40,60
#bs wrapper used to create our matrix of basis functions
fit=lm(wage~bs(age ,knots=c(25,40,60) ),data=Wage)
pred=predict (fit ,newdata =list(age=age.grid),se=T)
plot(age ,wage ,col="gray")
lines(age.grid ,pred$fit ,lwd=2)
lines(age.grid ,pred$fit+2*pred$se ,lty="dashed")
lines(age.grid ,pred$fit -2*pred$se ,lty="dashed")

#can use df option to specify knots at uniform quantiles of the data
attr(bs(age ,df=6) ,"knots")
#note that bs has a degree option if we want a nondefault polynomial on our spline (default is 3)

#natural splines fit using ns fxn instead of bs fxn
fit2=lm(wage~ns(age ,df=4),data=Wage)
pred2=predict (fit2 ,newdata=list(age=age.grid),se=T)
lines(age.grid , pred2$fit ,col="red",lwd=2)

#fitting a smoothing spline using the smooth.spline fxn
plot(age ,wage ,xlim=agelims ,cex =.5,col="darkgrey")
title("Smoothing Spline ")
fit=smooth.spline(age ,wage ,df=16)
fit2=smooth.spline (age ,wage ,cv=TRUE)
fit2$df
lines(fit ,col="red",lwd =2)
lines(fit2 ,col="blue",lwd=2)
legend ("topright",legend=c("16 DF" ,"6.8 DF"),col=c("red","blue"),lty=1,lwd=2, cex =.8)
#we can specify DF in smooth.spline, but we can also let CV auto determine it w/ CV = TRUE command

#performing local regression w/ Loess fxn
plot(age ,wage ,xlim=agelims ,cex =.5,col=" darkgrey ")
title("Local Regression ")
fit=loess(wage~age ,span=.2,data=Wage)
fit2=loess(wage~age ,span=.5,data=Wage)
lines(age.grid ,predict (fit ,data.frame(age=age.grid)),
        col="red",lwd=2)
lines(age.grid ,predict (fit2 ,data.frame(age=age.grid)),
        col="blue",lwd=2)
legend ("topright",legend=c("Span=0.2"," Span=0.5"),
          col=c("red","blue"),lty=1,lwd=2, cex =.8)

#Generalized Additive Models in R
#fit using natural splines of year and age, education = qualitative predictor (not splined!)
gam1=lm(wage~ns(year ,4)+ns(age ,5)+education ,data=Wage)

#smoothing splines are harder b/c no least squares version (thus can't do it in the lm command :()
library(gam)
#s in the command tells us to use smoothing spline of this predictor w/ specified df
gam.m3=gam(wage~s(year ,4)+s(age ,5)+education ,data=Wage)
#plotting our 3 parameters in our model!
par(mfrow=c(1,3))
plot(gam.m3, se=TRUE ,col =" blue")
#note that above it is calling plot.gam b/c gam is a gam class item!
plot.Gam(gam1 , se=TRUE , col="red")

#we can then test if a given predictor is worth excluding, having as a linear fxn, or having as a spline!
gam.m1=gam(wage~s(age ,5)+education ,data=Wage)
gam.m2=gam(wage~year+s(age ,5)+education ,data=Wage)
anova(gam.m1,gam.m2,gam.m3,test="F")
#looking @ the results, a linear fit for year is good, but the spline is unneeded and costs DF so lets not!
summary (gam.m3)
#p values for year and age are testing linear vs nonlinear relationship
#thus we can see year makes more sense as a linear fxn!
#we can also make predictions from GAM objects
preds=predict (gam.m2,newdata =Wage)

#we can also use local regression fits as part of GAM
#this is using the lo() fxn in our GAM as a component
gam.lo=gam(wage~s(year ,df=4)+lo(age ,span =0.7)+education ,data=Wage)
plot.Gam(gam.lo, se=TRUE , col ="green")
#we can also specify interactions using lo
gam.lo.i=gam(wage~lo(year ,age , span=0.5)+education ,
             data=Wage)
#2 term model, first term is interaction b/w year and age, fit w/ local regression
#can plot it using Akima
library(akima)
plot(gam.lo.i)

#we can also fit a logistic regression GAM using our wrapper, I to construct the binary response var
#and set family to binomial
gam.lr=gam(I(wage >250)~year+s(age ,df=5)+education ,
           family=binomial ,data=Wage)
par(mfrow=c(1,3))
plot(gam.lr,se=T,col="green ")
#it looks all fuckted b/c there is no high earners in the <hs category!
#lets try it after removing the HS grad data
gam.lr.s=gam(I(wage >250)~year+s(age ,df=5)+education ,family=
               binomial ,data=Wage , subset =(education !="1. < HS Grad"))
plot(gam.lr.s,se=T,col="green")
View(Wage)
