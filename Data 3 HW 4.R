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