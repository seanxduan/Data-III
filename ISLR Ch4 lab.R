#ISLR Chr 4 Exercises
install.packages("ISLR")
library(ISLR)

Smarket<-Smarket
glm.fits=glm(Direction∼Lag1+Lag2+Lag3+Lag4+Lag5+Volume ,
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
lda.fit<-lda(Direction ~ Lag1+Lag2,data = Smarket, subset = train)
lda.fit

lda.pred=predict (lda.fit , Smarket.2005)
names(lda.pred)

lda.class=lda.pred$class
table(lda.class ,Direction.2005)
mean(lda.class==Direction.2005)
