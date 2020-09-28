#Data 3 HW 3
#2
library(MASS)
data(Boston)
View(Boston)

#a
set.seed(1)
train=sample(c(TRUE ,FALSE), nrow(Boston),rep=TRUE)
test=(!train)
Boston$chas<-as.factor(Boston$chas)

#b
p2_m1<-lm(crim~.,data=Boston, subset = train)
summary(p2_m1)
mean(p2_m1$residuals^2)

#c
library(ISLR)
library(leaps)
regfit.best=regsubsets (crim~.,data=Boston[train ,],
                        nvmax=13)
test.mat=model.matrix(crim~.,data=Boston [test ,])

val.errors =rep(NA ,13)
for(i in 1:13){
  coefi=coef(regfit.best ,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Boston$crim[test]-pred)^2)
}
val.errors
which.min(val.errors)
#best model is 9 variables
coef(regfit.best ,9)

predict.regsubsets =function (object , newdata ,id ,...){
  form=as.formula (object$call [[2]])
  mat=model.matrix(form ,newdata )
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

regfit.best=regsubsets (crim~.,data=Boston[test,] ,nvmax=13)
coef(regfit.best ,10)
p2_m2<-lm(crim~zn+chas+nox+rm+dis+rad+tax+ptratio+lstat+medv, data =Boston[test,])
mean(p2_m2$residuals^2)

#d
#rr
library(glmnet)
x=model.matrix(crim~.,Boston )[,-1]
y=Boston$crim
y.test<-y[test]

set.seed(1)
cv.out=cv.glmnet(x[train ,],y[ train],alpha=0)
plot(cv.out)
bestlam =cv.out$lambda.min
bestlam

grid=10^seq(10,-2, length =100)

ridge.mod=glmnet (x,y,alpha=0, lambda=grid)

ridge.pred=predict (ridge.mod ,s=bestlam ,newx=x[test ,])
mean((ridge.pred-y.test)^2)

out=glmnet(x,y,alpha=0)
predict (out ,type="coefficients",s= bestlam) [1:14,]

#e
lasso.mod=glmnet(x[train ,],y[ train],alpha=1, lambda =grid)
plot(lasso.mod)


set.seed(1)
cv.out=cv.glmnet(x[train ,],y[ train],alpha=1)
plot(cv.out)
bestlam =cv.out$lambda.min
bestlam
lasso.pred=predict (lasso.mod ,s=bestlam ,newx=x[test ,])
mean((lasso.pred -y.test)^2)

out=glmnet (x,y,alpha=1, lambda=grid)
lasso.coef=predict (out ,type="coefficients",s= bestlam) [1:14,]
lasso.coef
#age cuts to 0

# 4
#prep code
set.seed(1)
x1 = rnorm(1000)
x2 = rnorm(1000)
x3 = rnorm(1000)
x4 = rnorm(1000)
x5 = rnorm(1000)
e = .1*rnorm(1000)
beta.truth = c(1.3,.01,-1.2,-.02,.6)
x = cbind(x1,x2,x3,x4,x5)
y = x%*%beta.truth + e
data4<-cbind(y,x1,x2,x3,x4,x5)
data4<-as.data.frame(data4)

myRSSgen <- function(beta,x,y,lam,q){
  sum((y-x%*% beta)^2) + lam*sum((abs(beta))^q)
}
minigrid<-seq(from=0, to=4, by=.2)
length(minigrid)

cv.errors =matrix (NA,k,19, dimnames =list(NULL , paste (1:19) ))
?matrix
#a
blat<-optim(rep(0,ncol(x)),myRSSgen,method='CG',x=x,y=y,lam=0,q=0)
blat
  
optim_list<-matrix(NA, nrow=length(minigrid), ncol=5)

optim_list<-rep(NA)
k<-1
for(i in 1:length(minigrid)){
  for(j in 1:length(minigrid)){
    optim_list[k]<-optim(rep(0,ncol(x)),myRSSgen,method='CG',x=x,y=y,lam=minigrid[[i]],q=minigrid[[j]])[2]
    k<-k+1
  }
}
which.min(optim_list)
print(optim_list)
#lambda = 0, q = 0

#b
?seq
paralist<-rep(NA)
for(i in 1:length(minigrid)){
  paralist[i]<-optim(rep(0,ncol(x)),myRSSgen,method='CG',x=x,y=y,lam=10,q=minigrid[[i]])[1]
  }
print(paralist)

#c
K=5
folds = sample(1:K,nrow(data4),replace=T)
optim_list<-rep(NA)
bstlam<-rep(NA)

for(k in 1:K){
  CV.train = data4[folds != k,]
  CV.test = data4[folds == k,]
  for(j in 1:length(minigrid)){
    optim_list[j]<-optim(rep(0,ncol(x)),myRSSgen,method='CG',x=CV.train[,-1],y=CV.train[,1],lam=minigrid[[j]],q=2)[2]
  }
  bstlam[k]<-which.min(optim_list)
  }

View(bstlam)
class(CV.train[,1])
class(y)
