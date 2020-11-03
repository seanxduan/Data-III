#Data 3 HW 6
library(ISLR)
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
#why is this fitting so weirdly???
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
