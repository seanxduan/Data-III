#Data 3 HW2
#1
#A
set.seed(1)
x=rnorm(100)
y=x-2*x^2+rnorm (100)
?rnorm

#B
plot(x,y)
#seems like nonlinear relationship between x and y, given that the functional form of the data follows a parabola

#c
set.seed(3)
library(boot)
#loocv
data_1<-as.data.frame(cbind(x,y))


cv.error=rep(0,4)
for (i in 1:4){
  glm.fit=glm(y∼poly(x ,i),data=data_1)
  cv.error[i]=cv.glm(data_1 ,glm.fit)$delta [1]
}
cv.error

#f
for (i in 1:4){
  print(summary(glm(y∼poly(x ,i),data=data_1)))
  }

#we see that the exponential term up to 2 is significant in all our models, as our previous answer would lead us to expect,
#in the polynomial 3 and 4 models coefficients past the 2nd exponent are not significant.

#2
library(MASS)
data(Boston)
## A
mean(Boston$medv)
## B
(sd(Boston$medv))/sqrt(length(Boston$medv))

## C
library(boot)
#create our fxn for the bootstrap
boot_fn<-function(data, index){
  mean(data[index])
}
boot_result<-boot(data = Boston$medv, statistic = boot_fn, R=1000)
boot_result
# compare the std error from boot fxn against our calc'd value in B
#D
t.test(Boston$medv)
mean(Boston$medv)-2*0.4012719
mean(Boston$medv)+2*0.4012719
#Slightly larger confidence interval

#e
median(Boston$medv)

#f
boot_fn2<-function(data, index){
  median(data[index])
}
boot_result2<-boot(data = Boston$medv, statistic = boot_fn2, R=1000)
boot_result2
#the size of the std error is similar to that which we have found earlier for the mean
#this makes sense as our median estimate is less sensitive to outlier information
#however, our data doesn't seem to have a great deal of outliers, so our mean
#and median estimate would make sense to be similar

#g
?quantile
quantile(Boston$medv, probs = .1)

#h
boot_fn3<-function(data, index){
  quantile(data[index], probs = .1)
}
boot_result3<-boot(data = Boston$medv, statistic = boot_fn3, R=1000)
boot_result3


## 3
youtube<-read.csv("Data-III/youtube.csv")

library(boot)

library(e1071)

set.seed(1)

mods=list(1,2,3,4,c(1,2),c(1,3),c(1,4),c(2,3),c(2,4),c(3,4),
          
          c(1,2,3),c(1,2,4),c(1,3,4),c(2,3,4))

cv.error=rep(0,length(mods))

for(i in 1:length(mods)){
  
  youtube_mod=youtube[,c(1,1+mods[[i]])] 
  
  glm.fit=glm(utime~.,data=youtube_mod)
  
  cv.error[i]=cv.glm(youtube_mod,glm.fit,K=5)$delta[1]
  
}

cv.error

glm.fitbest=glm(utime~size+umem+OutputPixels,data=youtube)





shapiro.test(glm.fitbest$residuals)

par(mfrow=c(1,3))

boxplot(glm.fitbest$residuals)

hist(glm.fitbest$residuals,main="residuals")

qqplot(glm.fitbest$residuals,rnorm(1000),xlab="residuals",ylab="normal quantiles")


## 4
library(MASS)
data(Pima.tr)
data(Pima.te)
library(ISLR)
data(nodal)

#doesn't work b/c can't run an obs on the 'list' intsead of a batch of formulas

#extremely brutalist representation, create our namesvec w/ all names
#plan on throwing out a model in each chunk (the double v model)


#try something else but it might be garbage

vars <- c("npreg","glu", "bp", "skin", "bmi", "ped", "age")
comb.vars <- expand.grid(vars, vars, stringsAsFactors = FALSE)
comb.vars <- comb.vars[comb.vars[,1] != comb.vars[,2],]
i.vars <- apply(comb.vars, 1, paste, collapse = "+")
View(i.vars)


cv_error<-list(NA)
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)
modelfits<-list(NA)
for(i in 1:length(i.vars)) {
  modelformula <- paste("type ~", i.vars[i])
  modelfits[[i]] <- glm(as.formula(modelformula), family = "binomial", data = Pima.tr)
  cv_error[[i]]=cv.glm(Pima.tr,modelfits[[i]], cost = cost ,K=5)$delta[1]
}
cv_error
which.min(cv_error)

best_model<-modelfits[[38]]
summary(best_model)
glm.probs=predict(best_model, Pima.te ,type="response")


glm.pred=rep("No" ,332)
glm.pred[glm.probs >.5]="Yes"
table(glm.pred,Pima.te$type)

#ex<-glm(type ~ npreg+glu,data=Pima.tr,family="binomial")
#library(boot)
#ex.cv<-cv.glm(Pima.tr ,ex, cost = cost,K=5)
#ex.cv$delta[1]
#?cv.glm
#cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)

#we can use forward stepwise for c, or we can use domain knowledge to pick a 'best logistic reg'
library(MASS)
full_model<-glm(type~., data = Pima.tr, family = "binomial")
step_model<-stepAIC(full_model, direction = "both")
step_model_1<-stepAIC(glm(type~1, data = Pima.tr, family = "binomial"),scope=formula(full_model),direction = "forward")

#make sure to test it's performance on the test set!!!

#
cv_error<-list(NA)
#program a deviance based loss fxn
cost <- function(r,p) -2*(sum(log(p)))


modelfits<-list(NA)
for(i in 1:length(i.vars)) {
  modelformula <- paste("type ~", i.vars[i])
  modelfits[[i]] <- glm(as.formula(modelformula), family = "binomial", data = Pima.tr)
  cv_error[[i]]=cv.glm(Pima.tr,modelfits[[i]], cost = cost ,K=5)$delta[1]
}
cv_error
which.min(cv_error)
