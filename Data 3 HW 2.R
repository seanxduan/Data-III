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
youtube<-read.csv("youtube.csv")


## 4
library(MASS)
data(Pima.tr)
data(Pima.te)
library(ISLR)
?cv.glm
data(nodal)


#test material on combos
install.packages("MuMIn")
library(MuMIn)
data(iris)

globalmodel <- lm(Sepal.Length ~ Petal.Length + Petal.Width + Species, data = iris, na.action = "na.fail")

combinations <- dredge(globalmodel)

print(combinations)
cv.glm(iris ,combinations ,K=10) $delta [1]
#doesn't work b/c can't run an obs on the 'list' intsead of a batch of formulas

#extremely brutalist representation, create our namesvec w/ all names
#plan on throwing out a model in each chunk (the double v model)
install.packages("Hmisc")
install.packages("Design")
library(Hmisc)
library(Design)
#doing this for our first combo of var's, lets see if it works?
varnames <- Cs(glu, bp, skin, bmi, ped, age)
modelfits <- vector(length(varnames), mode = "list")
names(modelfits) <- varnames
for(i in varnames) {
  modelformula <- paste("type ~ npreg +", i)
  modelfits[[i]] <- glm(as.formula(modelformula), data = Pima.tr)
}

#doesn't work???
varnames <- Cs(npreg,glu, bp, skin, bmi, ped, age)
modelfits <- vector(length(varnames), mode = "list")
names(modelfits) <- varnames
for(i in varnames) {
  modelformula <- paste("type ~ npreg +", i)
  modelfits[[i]] <- glm(as.formula(modelformula), data = Pima.tr)
}
str(modelfits)


#try something else but it might be garbage

vars <- c("npreg","glu", "bp", "skin", "bmi", "ped", "age")
comb.vars <- expand.grid(vars, vars, stringsAsFactors = FALSE)
comb.vars <- comb.vars[!(comb.vars[,1] == comb.vars[,2]),]
i.vars <- apply(comb.vars, 1, paste, collapse = "+")
View(i.vars)

for(i in 1:length(i.vars)) {
  modelformula <- paste("type ~", i.vars[i])
  modelfits[[i]] <- glm(as.formula(modelformula), data = Pima.tr)
}

for(i in 1:length(i.vars)) {
  modelformula <-c("type ~", i.vars[i])
}

View(modelformula)
i.vars[1]

