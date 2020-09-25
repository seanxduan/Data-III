#ISLR ch6 exercises#
library(ISLR)
data(Hitters)
View(Hitters)
Hitters<-na.omit(Hitters)
dim(Hitters)
install.packages("leaps")

library(leaps)
#1 subset selection methods
## Best subset selection
regfit.full=regsubsets(Salary~.,Hitters)
summary(regfit.full)

regfit.full=regsubsets (Salary~.,data=Hitters ,nvmax=19)
reg.summary =summary (regfit.full)

reg.summary$rsq

par(mfrow=c(2,2))
plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS",
       type="l")
plot(reg.summary$adjr2 ,xlab="Number of Variables ",
       ylab="Adjusted RSq",type="l")

which.max(reg.summary$adjr2)
points (11,reg.summary$adjr2[11], col="red",cex=2,pch =20)

plot(reg.summary$cp ,xlab="Number of Variables ",ylab="Cp",
       type='l')
which.min(reg.summary$cp )
points (10,reg.summary$cp [10], col ="red",cex=2,pch =20)
which.min(reg.summary$bic )
plot(reg.summary$bic ,xlab="Number of Variables ",ylab="BIC",
       type='l')
points (6,reg.summary$bic [6],col="red",cex=2,pch =20)

#error calling the correct plot package?
#plot window wasn't big enough
?plot.regsubsets
?regsubsets
leaps:::plot.regsubsets(regfit.full ,scale="r2")
plot(regfit.full ,scale="adjr2")
plot(regfit.full ,scale="Cp")
plot(regfit.full ,scale="bic")

coef(regfit.full ,6)

#Forward and backward stepwise selection
regfit.fwd=regsubsets (Salary~.,data=Hitters , nvmax=19,
                         method ="forward")
summary (regfit.fwd)
regfit.bwd=regsubsets (Salary~.,data=Hitters , nvmax=19,
                         method ="backward")
summary (regfit.bwd)


coef(regfit.full ,7)
coef(regfit.fwd ,7)
coef(regfit.bwd ,7)

## Validation set approach and CV
set.seed(1)
train=sample(c(TRUE ,FALSE), nrow(Hitters ),rep=TRUE)
test=(!train)

regfit.best=regsubsets (Salary~.,data=Hitters[train ,],
                        nvmax=19)

test.mat=model.matrix(Salary~., data=Hitters[test,])

val.errors =rep(NA ,19)
for(i in 1:19){
  coefi=coef(regfit.best ,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean(( Hitters$Salary[test]-pred)^2)
}
val.errors
which.min(val.errors)

coef(regfit.best,10)

predict.regsubsets =function (object , newdata ,id ,...){
  form=as.formula (object$call [[2]])
  mat=model.matrix(form ,newdata )
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

regfit.best=regsubsets (Salary~.,data=Hitters ,nvmax=19)
coef(regfit.best ,10)

k=10
set.seed(1)
folds=sample (1:k,nrow(Hitters),replace=TRUE)
cv.errors =matrix (NA,k,19, dimnames =list(NULL , paste (1:19) ))

for(j in 1:k){
  best.fit=regsubsets (Salary~.,data=Hitters [folds!=j,],nvmax=19)
  for(i in 1:19){
    pred=predict (best.fit ,Hitters [folds ==j,],id=i)
    cv.errors[j,i]= mean( ( Hitters$Salary[ folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors ,2, mean)
mean.cv.errors
which.min(mean.cv.errors)

par(mfrow=c(1,1))
plot(mean.cv.errors ,type='b')

reg.best=regsubsets (Salary~.,data=Hitters , nvmax=19)
coef(reg.best ,10)

#2 Ridge Regression and LASSO
install.packages("survival")
install.packages("glmnet")
#a beast is me!
library(glmnet)

#Ridge Reg
x=model.matrix(Salary~.,Hitters )[,-1]
y=Hitters$Salary
grid=10^seq(10,-2, length =100)
length(grid)

ridge.mod=glmnet (x,y,alpha=0, lambda=grid)

ridge.mod$lambda[50]
coef(ridge.mod)[ ,50]

ridge.mod$lambda [60]
coef(ridge.mod)[ ,60]

predict(ridge.mod,s=50,type="coefficients") [1:20,]

set.seed(1)
train=sample (1: nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

ridge.mod=glmnet(x[train ,],y[ train],alpha=0, lambda =grid ,thresh =1e-12)
ridge.pred=predict (ridge.mod ,s=4, newx=x[test ,])
mean((ridge.pred -y.test)^2)

ridge.pred=predict (ridge.mod ,s=0, newx=x[test ,])
mean((ridge.pred -y.test)^2)

lm(y~x, subset=train)
predict (ridge.mod ,s=0,type="coefficients")[1:20,]

set.seed(1)
cv.out=cv.glmnet(x[train ,],y[ train],alpha=0)
plot(cv.out)
bestlam =cv.out$lambda.min
bestlam

ridge.pred=predict (ridge.mod ,s=bestlam ,newx=x[test ,])
mean((ridge.pred -y.test)^2)

out=glmnet(x,y,alpha=0)
predict (out ,type="coefficients",s= bestlam) [1:20,]

#lasso
lasso.mod=glmnet(x[train ,],y[ train],alpha=1, lambda =grid)
plot(lasso.mod)

set.seed(1)
cv.out=cv.glmnet(x[train ,],y[ train],alpha=1)
plot(cv.out)
bestlam =cv.out$lambda.min
lasso.pred=predict (lasso.mod ,s=bestlam ,newx=x[test ,])
mean((lasso.pred -y.test)^2)

out=glmnet (x,y,alpha=1, lambda=grid)
lasso.coef=predict (out ,type="coefficients",s= bestlam) [1:20,]
lasso.coef

# PCR and PLS reg
#pcr
install.packages("pls")
library(pls)

set.seed(2)
pcr.fit=pcr(Salary~., data=Hitters , scale=TRUE ,validation ="CV")

summary (pcr.fit)

validationplot(pcr.fit ,val.type= "MSEP")

pcr.fit=pcr(Salary~., data=Hitters , subset=train ,scale=TRUE ,
            validation ="CV")
validationplot(pcr.fit ,val.type="MSEP")

pcr.pred=predict (pcr.fit ,x[test ,],ncomp =7)
mean((pcr.pred -y.test)^2)

pcr.fit=pcr(y~x,scale=TRUE ,ncomp=7)
summary (pcr.fit)
#pls

set.seed(1)
pls.fit=plsr(Salary~., data=Hitters , subset=train , scale=TRUE ,
               validation ="CV")
summary (pls.fit)

pls.pred=predict (pls.fit ,x[test ,],ncomp =2)
mean((pls.pred -y.test)^2)

pls.fit=plsr(Salary~., data=Hitters , scale=TRUE , ncomp=2)
summary (pls.fit)
