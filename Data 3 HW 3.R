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

#f
library(pls)
set.seed(2)
pcr.fit=pcr(crim~., data=Boston , scale=TRUE ,validation ="CV")

summary(pcr.fit)

validationplot(pcr.fit ,val.type= "MSEP")

pcr.fit=pcr(crim~., data=Boston , subset=train ,scale=TRUE ,
            validation ="CV")
validationplot(pcr.fit ,val.type="MSEP")

pcr.pred=predict (pcr.fit ,x[test,],ncomp =13)
mean((pcr.pred -y.test)^2)

pcr.fit=pcr(y~x,scale=TRUE ,ncomp=13)
summary (pcr.fit)
#M is 13
mean((pcr.pred -y.test)^2)

#g
set.seed(1)
pls.fit=plsr(crim~., data=Boston , subset=train , scale=TRUE ,
             validation ="CV")
summary (pls.fit)

pls.pred=predict (pls.fit ,x[test ,],ncomp =10)

pls.fit=plsr(crim~., data=Boston , scale=TRUE , ncomp=10)
summary (pls.fit)

#m is 10
mean((pls.pred -y.test)^2)

#3
load("lakes_DA3.Rdata")
lakes<-lakes_DA3
lakes$lsecchi<-log(lakes$secchi)
train=sample(c(TRUE ,FALSE), nrow(lakes),rep=TRUE)
test=(!train)
#RR
library(glmnet)
x=model.matrix(lsecchi~.-secchi,lakes )[,-c(4,25)]
View(x)
y=lakes$lsecchi
y.test<-y[test]
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam =cv.out$lambda.min
bestlam
grid=10^seq(10,-2, length =100)
ridge.mod=glmnet (x,y,alpha=0, lambda=grid)
ridge.pred=predict (ridge.mod ,s=bestlam ,newx=x[test ,])
out=glmnet(x,y,alpha=0)
predict (out ,type="coefficients",s= bestlam) [1:24,]
mean((ridge.pred-y.test)^2)

#Lasso
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
lasso.coef=predict (out ,type="coefficients",s= bestlam) [1:24,]
lasso.coef

#pcr
library(pls)
set.seed(1)
pcr.fit=pcr(lsecchi~. -secchi, data=lakes , scale=TRUE ,validation ="CV")
summary (pcr.fit)
validationplot(pcr.fit ,val.type= "MSEP")
pcr.fit=pcr(lsecchi~. -secchi, data=lakes , subset=train ,scale=TRUE ,
          validation ="CV")
validationplot(pcr.fit ,val.type="MSEP")

pcr.pred=predict (pcr.fit ,x[test ,],ncomp =12)
mean((pcr.pred -y.test)^2)

pcr.fit=pcr(y~x,scale=TRUE ,ncomp=12)
summary (pcr.fit)

#pls
set.seed(1)
pls.fit=plsr(lsecchi~.-secchi, data=lakes , subset=train , scale=TRUE ,
             validation ="CV")
summary (pls.fit)

pls.pred=predict (pls.fit ,x[test ,],ncomp =19)
mean((pls.pred -y.test)^2)
pls.fit=plsr(lsecchi~.-secchi, data=lakes , scale=TRUE , ncomp=19)
summary (pls.fit)

#lasso cv?
x <- matrix(rnorm(100*1000), nrow = 100, ncol = 1000)
y <- x[,1] * 2 + x[,2] * 2.5 + rnorm(100)
sel <- lasso.cv(x, y)
sel
# }
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
  
optim_list<-matrix(NA, nrow=length(minigrid), ncol=length(minigrid))
#fucking with it rn
optim_list<-rep(NA)
for(i in 1:length(minigrid)){
  for(j in 1:length(minigrid)){
    optim_list[i,j]<-as.numeric(optim(rep(0,ncol(x)),myRSSgen,method='CG',x=x,y=y,lam=minigrid[i],q=minigrid[j])[2])
    }
}
which.min(optim_list)
print(optim_list)

#b
?seq
paralist<-rep(NA)
for(i in 1:length(minigrid)){
  paralist[i]<-optim(rep(0,ncol(x)),myRSSgen,method='CG',x=x,y=y,lam=10,q=minigrid[[i]])[1]
  }
print(paralist)

beta<-paralist[1]
#
beta<-as.numeric(unlist(beta))

beta
View(beta)
class(beta)
View(beta.truth)
#c
K=5
folds = sample(1:K,nrow(data4),replace=T)
optim_list<-rep(NA)
bstlam<-rep(NA)
err_cv<-rep(NA)
betalist<-rep(NA)
betaguess<-rep(NA)
for(k in 1:K){
  CV.train = x[folds != k,]
  CV.test = x[folds == k,]
  CV.tr_y = y[folds != k,]
  CV.ts_y = y[folds == k,]
  for(j in 1:length(minigrid)){
    optim_list[j]<-optim(rep(0,ncol(x)),myRSSgen,method='CG',x=CV.train,y=CV.tr_y,lam=minigrid[[j]],q=2)[2]
  }
  bstlam[k]<-minigrid[[which.min(optim_list)]]
  betalist[k]<-optim(rep(0,ncol(x)),myRSSgen,method='CG',x=CV.train,y=CV.tr_y,lam=bstlam[[k]],q=2)[1]
  betaguess<-as.numeric(unlist(betalist[k]))
  err_cv[k]<-optim(rep(0,ncol(x)),myRSSgen,method='CG',x=CV.test,y=CV.ts_y,lam=bstlam[[k]],q=2)[2]
  }
err_cv
as.numeric(err_cv)

#wrong but it puts out?
K=5
folds = sample(1:K,nrow(data4),replace=T)
optim_list<-rep(NA)
bstlam<-rep(NA)
err_cv<-rep(NA)
betalist<-rep(NA)
for(k in 1:K){
  CV.train = x[folds != k,]
  CV.test = x[folds == k,]
  CV.tr_y = y[folds != k,]
  CV.ts_y = y[folds == k,]
  for(j in 1:length(minigrid)){
    optim_list[j]<-optim(rep(0,ncol(x)),myRSSgen,method='CG',x=CV.train,y=CV.tr_y,lam=minigrid[[j]],q=2)[2]
  }
  bstlam[k]<-minigrid[[which.min(optim_list)]]
  betalist[k]<-optim(rep(0,ncol(x)),myRSSgen,method='CG',x=CV.train,y=CV.tr_y,lam=bstlam[[k]],q=2)[1]
    err_cv[k]<-optim(rep(0,ncol(x)),myRSSgen,method='CG',x=CV.test,y=CV.ts_y,lam=bstlam[[k]],q=2)[2]
}
mean(as.numeric(unlist(err_cv)))
plot(x=err_cv, y=bstlam)

#d
#lasso
K=5
folds = sample(1:K,nrow(data4),replace=T)
optim_list<-rep(NA)
bstlam<-rep(NA)
err_cv<-rep(NA)
betalist<-rep(NA)
for(k in 1:K){
  CV.train = x[folds != k,]
  CV.test = x[folds == k,]
  CV.tr_y = y[folds != k,]
  CV.ts_y = y[folds == k,]
  for(j in 1:length(minigrid)){
    optim_list[j]<-optim(rep(0,ncol(x)),myRSSgen,method='CG',x=CV.train,y=CV.tr_y,lam=minigrid[[j]],q=1)[2]
  }
  bstlam[k]<-minigrid[[which.min(optim_list)]]
  err_cv[k]<-optim(rep(0,ncol(x)),myRSSgen,method='CG',x=CV.test,y=CV.ts_y,lam=bstlam[[k]],q=1)[2]
}
mean(as.numeric(unlist(err_cv)))
plot(x=err_cv, y=bstlam)

#e
K=5
folds = sample(1:K,nrow(data4),replace=T)
optim_list<-rep(NA)
bstlam<-rep(NA)
bstq<-rep(NA)
err_cv<-rep(NA)
betalist<-rep(NA)
optim_list<-matrix(NA, nrow=length(minigrid), ncol=length(minigrid))
for(k in 1:K){
  CV.train = x[folds != k,]
  CV.test = x[folds == k,]
  CV.tr_y = y[folds != k,]
  CV.ts_y = y[folds == k,]
  for(i in 1:length(minigrid)){
    for(j in 1:length(minigrid)){
      optim_list[i,j]<-as.numeric(optim(rep(0,ncol(x)),myRSSgen,method='CG',x=x,y=y,lam=minigrid[i],q=minigrid[j])[2])
    }
  }
  obj<-which(optim_list == min(optim_list), arr.ind=TRUE)
  bstlam[k]<-minigrid[[obj[1,1]]]
  bstq[k]<-minigrid[[obj[1,2]]]
  err_cv[k]<-optim(rep(0,ncol(x)),myRSSgen,method='CG',x=CV.test,y=CV.ts_y,lam=bstlam[[k]],q=bstq[[k]])[2]
}
mean(as.numeric(unlist(err_cv)))
bstlam
bstq

library(plotly)
fig <- plot_ly(
  x = bstlam,
  y = bstq,
  z = err_cv,
  type = "contour" )

fig
