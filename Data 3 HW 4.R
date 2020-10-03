#Data 3 HW 4
#2

library(ISLR)
library(MASS)
library(ggplot2)
data(Boston)

#A
#polynomail reg
p2_m1<-lm(nox~poly(dis ,3) ,data=Boston)
#coef(summary (fit))
summary(p2_m1)

#code to plot, first make predictions from our range of data
dis_lims =range(Boston$dis)
dis_grid=seq(from=dis_lims [1],to=dis_lims [2])
dis_preds=predict (p2_m1 ,newdata =list(dis=dis_grid),se=TRUE)
dis_se.bands=cbind(dis_preds$fit +2* dis_preds$se.fit ,dis_preds$fit -2* dis_preds$se.fit)
#actual plotting code
plot(Boston$dis ,Boston$nox, xlim=dis_lims ,cex =.5,col=" darkgrey ")
title(" Degree -3 Polynomial ",outer=T)
lines(dis_grid ,dis_preds$fit ,lwd=2,col="blue")
matlines (dis_grid ,dis_se.bands ,lwd=1, col=" blue",lty=3)

#B
#make a loop to fit all the polys
testlist<-list(NA)
for(i in 1:10){
  testlist[[i]]<-lm(nox~poly(dis ,i) ,data=Boston)
}

#print the deviance (same as RSS for linear models) on all polynomial fits
for(i in 1:10){
  print(deviance(testlist[[i]]))
}

#C
#setting up for CV
train=sample (506,253)
test=!train
#fitting the loop
testlist<-list(NA)
for(i in 1:10){
  testlist[[i]]<-lm(nox~poly(dis ,i) ,data=Boston, subset=train)
}

#test w/ p2_m1
for(i in 1:10){
print(mean((Boston$nox-predict(testlist[[i]],Boston))[-train ]^2))
}
#cross validation w/ test /training set chooses 3rd polynomial

#D
library(splines)
#those are my knot values 
#make a loop to fit all the splines w/ different df

testlist<-list(NA)
for(i in 3:6){
  testlist[[i]]<-lm(nox~bs(dis ,df = i) ,data=Boston, subset=train)
}

for(i in 3:6){
  print(mean((Boston$nox-predict(testlist[[i]],Boston))[-train ]^2))
}
#looks like our 3df model is best
#RSS is 0.003891
#code to plot, first make predictions from our range of data
dis_lims =range(Boston$dis)
dis_grid=seq(from=dis_lims [1],to=dis_lims [2])
dis_preds=predict (testlist[[3]] ,newdata =list(dis=dis_grid),se=TRUE)
dis_se.bands=cbind(dis_preds$fit +2* dis_preds$se.fit ,dis_preds$fit -2* dis_preds$se.fit)
#actual plotting code
plot(Boston$dis ,Boston$nox, xlim=dis_lims ,cex =.5,col=" darkgrey ")
title(" 3 degree freedom  ",outer=T)
lines(dis_grid ,dis_preds$fit ,lwd=2,col="blue")
matlines (dis_grid ,dis_se.bands ,lwd=1, col=" blue",lty=3)

#E
testlist<-list(NA)
for(i in 3:6){
  testlist[[i]]<-lm(nox~ns(dis ,df = i) ,data=Boston, subset=train)
}

for(i in 3:6){
  print(mean((Boston$nox-predict(testlist[[i]],Boston))[-train ]^2))
}
#4th df model best
#rss = 0.003822
#code to plot, first make predictions from our range of data
dis_lims =range(Boston$dis)
dis_grid=seq(from=dis_lims [1],to=dis_lims [2])
dis_preds=predict (testlist[[4]] ,newdata =list(dis=dis_grid),se=TRUE)
dis_se.bands=cbind(dis_preds$fit +2* dis_preds$se.fit ,dis_preds$fit -2* dis_preds$se.fit)
#actual plotting code
plot(Boston$dis ,Boston$nox, xlim=dis_lims ,cex =.5,col=" darkgrey ")
title(" 4 degree freedom  ",outer=T)
lines(dis_grid ,dis_preds$fit ,lwd=2,col="blue")
matlines (dis_grid ,dis_se.bands ,lwd=1, col=" blue",lty=3)

#F
#fitting a smoothing spline using the smooth.spline fxn
plot(x=Boston$dis ,y=Boston$nox ,xlim=dis_lims ,cex =.5,col="darkgrey")
title("Smoothing Spline ")
fit=smooth.spline(Boston$dis ,Boston$nox ,df=10)
fit2=smooth.spline(Boston$dis ,Boston$nox ,cv=TRUE)
fit2$df
lines(fit ,col="red",lwd =2)
lines(fit2 ,col="blue",lwd=2)
legend ("topright",legend=c("10 DF" ,"15.42 DF"),col=c("red","blue"),lty=1,lwd=2, cex =.8)
#we can specify DF in smooth.spline, but we can also let CV auto determine it w/ CV = TRUE command

fit2$spar
#what does this weighting mean??
fit2$pen.crit

#toy example from stackexchange
x = seq(1:18)
y = c(1:3,5,4,7:3,2*(2:5),rep(10,4))
splineres <- function(spar){
  res <- rep(0, length(x))
  for (i in 1:length(x)){
    mod <- smooth.spline(x[-i], y[-i], spar = spar)
    res[i] <- predict(mod, x[i])$y - y[i]
  }
  return(sum(res^2))
}

spars <- seq(0, 1.5, by = 0.001)
ss <- rep(0, length(spars))
for (i in 1:length(spars)){
  ss[i] <- splineres(spars[i])
}
plot(spars, ss, 'l', xlab = 'spar', ylab = 'Cross Validation Residual Sum of Squares' , main = 'CV RSS vs Spar')
spars[which.min(ss)]
R > spars[which.min(ss)]
[1] 0.381
## this code fits spar directly using spar fxn and cross validation?

