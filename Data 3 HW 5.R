#Data 3 HW 5
library(tree)
library(ISLR)
library(randomForest)
library(gbm)
attach(Carseats)
data(Carseats)
set.seed(1)
#split data into training and test set
train = sample (1:nrow(Carseats), nrow(Carseats)/2)

#1
##A
#fitting a regression tree
carseat_t1=tree(Sales~.,Carseats , subset=train)
summary(carseat_t1)
#only 6 vars are used, shelveloc,price,age,advertiseing,comprice,and us
#SSE is 2.16
plot(carseat_t1)
text(carseat_t1, pretty=0)
#get test/train mse
Carseats_test=Carseats [-train ,"Sales"]
yhat=predict(carseat_t1,newdata=Carseats[-train,])
plot(yhat ,Carseats_test)
abline (0,1)
mean((yhat -Carseats_test)^2)
#MSE is 4.922

##B
#pruning
cv_carseat_t1=cv.tree(carseat_t1)
plot(cv_carseat_t1$size ,cv_carseat_t1$dev ,type="b")
#no point in pruning our tree, although getting it to 5 wouldn't be bad if we wanted to simplify it
#lets try w/ prune @ 6
carseat_prune<-prune.tree(carseat_t1, best =6)

yhat=predict(carseat_prune,newdata=Carseats[-train,])
plot(yhat ,Carseats_test)
abline (0,1)
mean((yhat -Carseats_test)^2)
#worse MSE 5.318

##C
#lets do bagging!
carseat_bag=randomForest(Sales~.,data=Carseats , subset=train ,mtry=10,importance =TRUE)
carseat_bag
#checking test MSE
yhat.bag = predict (carseat_bag , newdata=Carseats[-train ,])
plot(yhat.bag , Carseats_test)
abline (0,1)
mean((yhat.bag -Carseats_test)^2)
#test MSE is 2.622! even bestter
#we can check importance of each variable by using the importance() fxn
importance(carseat_bag)
#we can plot these using this code
varImpPlot(carseat_bag)
#price and shelveloc most important, comp price not bad either

##D
#boosting!
set.seed(1)
carseat_boost=gbm(Sales~.,data=Carseats[train ,], distribution="gaussian",n.trees=5000, interaction.depth=4)
#choose gaussian b/c i want sq err loss, not anything else
yhat.boost=predict (carseat_boost ,newdata =Carseats[-train ,],n.trees=5000)
mean((yhat.boost - Carseats_test)^2)

#lets do a for loop to find which n.trees and depth is best
yhat.boost<-list(NA)
carseat_boost<-list(NA)
testn<-seq(from=300, to=7000, length.out = 20)
test_mse<-list(NA)

for (i in 1:20){
carseat_boost[[i]]=gbm(Sales~.,data=Carseats[train,], distribution="gaussian",n.trees=testn[[i]], interaction.depth=1)
yhat.boost[[i]]=predict(carseat_boost[[i]] ,newdata =Carseats[-train ,],n.trees=testn[[i]])
test_mse[[i]]<-mean((yhat.boost[[i]] - Carseats_test)^2)
}
test_mse
which.min(test_mse)

#trying first w/ a stump, then trying w/ another depth to see if better
for (i in 1:20){
  carseat_boost[[i]]=gbm(Sales~.,data=Carseats[train,], distribution="gaussian",n.trees=testn[[i]], interaction.depth=2)
  yhat.boost[[i]]=predict(carseat_boost[[i]] ,newdata =Carseats[-train ,],n.trees=testn[[i]])
  test_mse[[i]]<-mean((yhat.boost[[i]] - Carseats_test)^2)}

test_mse
which.min(test_mse)

#looking at the code, it seems like the best ntrees is 653, which is guarding against overfitting w/ larger sizes 
#tried to fit w/ a stump first, but could not get better results w/ a larger interaction depth.

##E
#Random Forest

#we start w/ a loop to find the best m value!
mvec<-seq(from=1, to=10)
yhat.bag<-list(NA)
carseat_rf<-list(NA)
test_mse<-list(NA)

for(i in 1:10){
carseat_rf[[i]]=randomForest(Sales~.,data=Carseats , subset=train ,mtry=mvec[i],importance =TRUE)
yhat.bag[[i]]<-predict(carseat_rf[[i]] , newdata=Carseats[-train ,])
test_mse[[i]]<-mean((yhat.bag[[i]] -Carseats_test)^2)
}
which.min(test_mse)
#setting m to 9 gets us the lowest test MSE

importance(carseat_rf[[which.min(test_mse)]])
varImpPlot(carseat_rf[[which.min(test_mse)]])
#price shelvloc and compprice are most important

##f
#mars!~
#used tutorial from
#http://uc-r.github.io/mars

install.packages("earth")
library(earth)
carseat_mars<-earth(Sales~.,data=Carseats, subset = train)
summary(carseat_mars)
print(carseat_mars)

#model selection code for mars
plot(carseat_mars, which=1)
#mars models scale invariant

#test mse code
yhat=predict(carseat_mars,newdata=Carseats[-train,])
mean((yhat -Carseats_test)^2)
#WOWOWOW lowest test MSE yet!
#2

