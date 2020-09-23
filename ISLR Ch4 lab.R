#ISLR Chr 4 Exercises
install.packages("ISLR")
library(ISLR)

Smarket<-Smarket
glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume ,
               data=Smarket ,family=binomial )
summary (glm.fits)
glm.probs=predict (glm.fits,type="response")
summary(glm.probs)
View(glm.probs)
attach(Smarket)

#generating vector that lets us know predicted up or down
glm.pred=rep("Down" ,1250)
glm.pred[glm.probs >.5]=" Up"
table(glm.pred,Smarket$Direction)

#creates a holdout to train and test against
train=(Year <2005)
Smarket.2005 = Smarket [!train ,]
dim(Smarket.2005)
Direction.2005=Direction[!train]

glm.fits=glm(Direction∼Lag1+Lag2+Lag3+Lag4+Lag5+Volume ,data=Smarket ,family=binomial ,subset=train)
     
glm.probs=predict (glm.fits,Smarket.2005, type="response")

glm.pred=rep("Down", 252)
glm.pred[glm.probs >.5]=" Up"
table(glm.pred,Direction.2005)

#now we try to do LDA
library(MASS)
attach(Smarket)
lda.fit<-lda(Direction ~ Lag1+Lag2,data = Smarket, subset = train)
lda.fit

lda.pred=predict (lda.fit , Smarket.2005)
names(lda.pred)

lda.class=lda.pred$class
table(lda.class ,Direction.2005)
mean(lda.class==Direction.2005)

sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)
lda.pred$class

#the code above predicts the guess that our lda pred shoots out!
lda.pred$posterior [1:20,1]
lda.class [1:20]

min(lda.pred$posterior[,1])

#QDA work
qda.fit=qda(Direction∼Lag1+Lag2 ,data=Smarket ,subset=train)
qda.fit

qda.class=predict (qda.fit ,Smarket.2005) $class
table(qda.class ,Direction.2005)

mean(qda.class==Direction.2005)

#KNN work
library(class)
#so we have p = 2 here?
train.X=cbind(Lag1 ,Lag2)[train ,]
test.X=cbind(Lag1 ,Lag2)[!train ,]
#This code is the code for the 'outcome' which we use to train our KNN?
train.Direction =Direction [train]
set.seed(1)
?knn
knn.pred=knn(train.X,test.X,train.Direction ,k=1)
table(knn.pred, Direction.2005)
mean(knn.pred==Direction.2005)

knn.pred=knn(train.X,test.X,train.Direction ,k=3)
table(knn.pred, Direction.2005)
mean(knn.pred==Direction.2005)


#caravan example
library(ISLR)
attach(Caravan)
standardized.X=scale(Caravan [,-86])

test=1:1000
train.X= standardized.X[-test ,]
test.X= standardized.X[test ,]
train.Y=Purchase [-test]
test.Y=Purchase [test]
set.seed(1)

knn.pred=knn(train.X,test.X,train.Y,k=1)
table(knn.pred, test.Y)
mean(knn.pred==test.Y)

mean(test.Y!=knn.pred)
