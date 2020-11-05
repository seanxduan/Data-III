#Data 3 HW 6
library(ISLR)
library(e1071)
library(ROCR)

attach(OJ)
data(OJ)
View(OJ)

#figure out which category (minute maid or citrus hill) based on vars in our data
#split training/test 
set.seed(1)
train=sample(1070,800)

#A
tune.out=tune(svm ,Purchase~.,data=OJ[train,] ,kernel ="linear", ranges=list(cost=c(0.01, 0.05, 0.1, 0.5, 1, 5, 10)))
summary(tune.out)
#best cost para 0.1
bestmod=tune.out$best.model
summary(bestmod)
#training error
ypred=predict(bestmod ,OJ[train,])
table(predict=ypred , truth=OJ[train,]$Purchase )
1-mean(ypred==OJ[train,]$Purchase)
#16.5% error

#test error
ypred=predict(bestmod ,OJ[-train,])
table(predict=ypred , truth=OJ[-train,]$Purchase )
1-mean(ypred==OJ[-train,]$Purchase)
# wow even lower test err, 16.3%!

#B
tune.out=tune(svm ,Purchase~.,data=OJ[train,] ,kernel ="polynomial", degree=2, ranges=list(cost=c(0.01, 0.05, 0.1, 0.5, 1, 5, 10)))
summary(tune.out)
#best cost para 10
bestmod=tune.out$best.model
summary(bestmod)
#training error
ypred=predict(bestmod ,OJ[train,])
table(predict=ypred , truth=OJ[train,]$Purchase )
1-mean(ypred==OJ[train,]$Purchase)
#15% error

#test error
ypred=predict(bestmod ,OJ[-train,])
table(predict=ypred , truth=OJ[-train,]$Purchase )
1-mean(ypred==OJ[-train,]$Purchase)
#18.9% err

#maybe add plots?

#C
tune.out=tune(svm ,Purchase~.,data=OJ[train,] ,kernel ="radial",
              ranges=list(cost=c(0.01, 0.05, 0.1, 0.5, 1, 5, 10),
                          gamma=c(0.001, 0.01, 0.05, 0.1, 0.5, 1, 5, 10, 100)))
summary(tune.out)

#best cost para 5, gamma .001
bestmod=tune.out$best.model
summary(bestmod)
#training error
ypred=predict(bestmod ,OJ[train,])
table(predict=ypred , truth=OJ[train,]$Purchase )
1-mean(ypred==OJ[train,]$Purchase)
#17.4% error

#test error
ypred=predict(bestmod ,OJ[-train,])
table(predict=ypred , truth=OJ[-train,]$Purchase )
1-mean(ypred==OJ[-train,]$Purchase)
#18.1% err

#D
tune.out=tune(svm ,Purchase~.,data=OJ[train,] ,kernel ="sigmoid",
              ranges=list(cost=c(0.01, 0.05, 0.1, 0.5, 1, 5, 10),
                          gamma=c(0.001, 0.01, 0.05, 0.1, 0.5, 1, 5, 10, 100)))
summary(tune.out)
#why is this fitting so weirdly???
#best cost para 10, gamma .001
bestmod=tune.out$best.model
summary(bestmod)
#training error
ypred=predict(bestmod ,OJ[train,])
table(predict=ypred , truth=OJ[train,]$Purchase )
1-mean(ypred==OJ[train,]$Purchase)
#17.5% error

#test error
ypred=predict(bestmod ,OJ[-train,])
table(predict=ypred , truth=OJ[-train,]$Purchase )
1-mean(ypred==OJ[-train,]$Purchase)
#17.8% err

#E
glm1<-glm(Purchase~., data = OJ[train,], family = binomial)
summary(glm1)

#training err
glm.probs=predict(glm1 ,OJ[train,], type = "response")
glm.pred=rep("CH",800)
glm.pred[glm.probs >.5]="MM"
table(predict=glm.pred , truth=OJ[train,]$Purchase )
1-mean(glm.pred==OJ[train,]$Purchase)
#17.3% err

#test err
glm.probs=predict(glm1 ,OJ[-train,], type = "response")
glm.pred=rep("CH",270)
glm.pred[glm.probs >.5]="MM"
table(predict=glm.pred , truth=OJ[-train,]$Purchase )
1-mean(glm.pred==OJ[-train,]$Purchase)
#15.6% err

#F
rocplot =function (pred , truth , ...){
  predob = prediction (pred , truth)
  perf = performance (predob , "tpr", "fpr")
  plot(perf ,...)}

par(mfrow=c(1,4))

## ROC for training data
#m1
svmfit.opt=svm(Purchase~., data=OJ[train,], kernel ="linear",gamma=2, cost=0.1, decision.values =T)
fitted =attributes (predict (svmfit.opt ,OJ[train ,], decision.values=TRUE))$decision.values
rocplot (fitted ,OJ[train ,"Purchase"], main="Training Data")
#m2
svmfit.2=svm(Purchase~., data=OJ[train,], kernel ="polynomial",degree=2, cost=10, decision.values =T)
fitted =attributes (predict (svmfit.2 ,OJ[train ,], decision.values=TRUE))$decision.values
rocplot (fitted ,OJ[train ,"Purchase"] ,add=T,col="red ")
#m3
svmfit.3=svm(Purchase~., data=OJ[train,], kernel ="radial",gamma=5, cost=0.001, decision.values =T)
fitted =attributes (predict (svmfit.3 ,OJ[train ,], decision.values=TRUE))$decision.values
rocplot (fitted ,OJ[train ,"Purchase"] ,add=T,col="blue")
#m4
svmfit.4=svm(Purchase~., data=OJ[train,], kernel ="sigmoid",gamma=0.001, cost=10, decision.values =T)
fitted =attributes (predict (svmfit.4 ,OJ[train ,], decision.values=TRUE))$decision.values
rocplot (fitted ,OJ[train ,"Purchase"] ,add=T,col="green")
#m5
library(pROC)
test_prob = predict(glm1, newdata = OJ[train,], type = "response")
test_roc = roc(OJ[train,]$Purchase ~ test_prob, plot = TRUE, print.auc = TRUE)


##ROC for test data
#m1
svmfit.opt=svm(Purchase~., data=OJ[-train,], kernel ="linear",gamma=2, cost=0.1, decision.values =T)
fitted =attributes (predict (svmfit.opt ,OJ[train ,], decision.values=TRUE))$decision.values
rocplot (fitted ,OJ[train ,"Purchase"], main="Testing data")
#m2
svmfit.2=svm(Purchase~., data=OJ[-train,], kernel ="polynomial",degree=2, cost=10, decision.values =T)
fitted =attributes (predict (svmfit.2 ,OJ[train ,], decision.values=TRUE))$decision.values
rocplot (fitted ,OJ[train ,"Purchase"] ,add=T,col="red ")
#m3
svmfit.3=svm(Purchase~., data=OJ[-train,], kernel ="radial",gamma=5, cost=0.001, decision.values =T)
fitted =attributes (predict (svmfit.3 ,OJ[train ,], decision.values=TRUE))$decision.values
rocplot (fitted ,OJ[train ,"Purchase"] ,add=T,col="blue")
#m4
svmfit.4=svm(Purchase~., data=OJ[-train,], kernel ="sigmoid",gamma=0.001, cost=10, decision.values =T)
fitted =attributes (predict (svmfit.4 ,OJ[train ,], decision.values=TRUE))$decision.values
rocplot (fitted ,OJ[train ,"Purchase"] ,add=T,col="green")
#m5
library(pROC)
test_prob = predict(glm1, newdata = OJ[-train,], type = "response")
test_roc = roc(OJ[-train,]$Purchase ~ test_prob, plot = TRUE, print.auc = TRUE)

#looking at AUC it seems like sigmoid kernal or logistic work best for the test data

#G
library(glmnet)
###ridge regression
x=model.matrix(Salary~.,Hitters )[,-1]
y=Hitters$Salary
grid=10^seq(10,-2, length =100)
length(grid)
ridge.mod=glmnet (x,y,alpha=0, lambda=grid, family = "binomial")
#finding best lambda
set.seed(1)
cv.out=cv.glmnet(x[train ,],y[ train],alpha=0, family = "binomial")
plot(cv.out)
bestlam =cv.out$lambda.min
bestlam
#bestlam is 0.0311
#training data
ridge.mod=glmnet(x[train ,],y[ train],alpha=0, lambda=0.0311,thresh =1e-12, family = "binomial")
ridge.pred=predict(ridge.mod ,s=4, newx=x[train,])
ridge.pred2=rep("CH",800)
ridge.pred2[ridge.pred >0]="MM"
table(predict=ridge.pred2 , truth=OJ[train,]$Purchase )
1-mean(ridge.pred2==OJ[train,]$Purchase)
#17.2% err w/ RR
ridge.pred=predict(ridge.mod ,s=4, newx=x[-train,])
ridge.pred2=rep("CH",270)
ridge.pred2[ridge.pred >0]="MM"
table(predict=ridge.pred2 , truth=OJ[-train,]$Purchase )
1-mean(ridge.pred2==OJ[-train,]$Purchase)
#17.4 err w/ RR

###LASSO
#finding best lambda
set.seed(1)
cv.out=cv.glmnet(x[train ,],y[ train],alpha=1, family = "binomial")
plot(cv.out)
bestlam =cv.out$lambda.min
bestlam
#bestlam is 0.00392
#training data
ridge.mod=glmnet(x[train ,],y[ train],alpha=1, lambda=0.00392,thresh =1e-12, family = "binomial")
ridge.pred=predict(ridge.mod ,s=4, newx=x[train,])
ridge.pred2=rep("CH",800)
ridge.pred2[ridge.pred >0]="MM"
table(predict=ridge.pred2 , truth=OJ[train,]$Purchase )
1-mean(ridge.pred2==OJ[train,]$Purchase)
#16.6% err w/ RR
ridge.pred=predict(ridge.mod ,s=4, newx=x[-train,])
ridge.pred2=rep("CH",270)
ridge.pred2[ridge.pred >0]="MM"
table(predict=ridge.pred2 , truth=OJ[-train,]$Purchase )
1-mean(ridge.pred2==OJ[-train,]$Purchase)
#17% err w/ RR
