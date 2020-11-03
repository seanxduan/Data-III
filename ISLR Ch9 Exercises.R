#ISLR ch9 exercises#
library(ISLR)
library(e1071)

#9.6.1 Support vector classifier
#svm fits a svc when kernal is set to linear
# cost argument specifies cost of violation, small c = large margins, large c = few supp vectors on margin

#generate fake data for our examples
set.seed(1)
x=matrix(rnorm (20*2), ncol=2)
y=c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 1
plot(x, col=(3-y))

#note that we need our response as a factor so we can do classification, instead of regression
dat=data.frame(x=x, y=as.factor(y))

#code to fit the svm
svmfit=svm(y~., data=dat , kernel ="linear", cost=10, scale=FALSE)
#note scale false means no scaling done to data, we can do this if we set scale to true

plot(svmfit , dat)
#note that supp vectors are plotted as crosses, remaining obs are circles
#we can find the supp vectors below
svmfit$index

#get some basic info by running summary
summary(svmfit)
#how many svecs are in each class!

#try with a different cost value
svmfit=svm(y~., data=dat , kernel ="linear", cost=.1, scale=FALSE)
plot(svmfit , dat)
svmfit$index
#smaller cost fxn, means MORE supp vectors because margins are now WIDER!

#we can do built in cv w/ the tune fxn defaults to 10fold cv
#compare svms w/ a linear kernal using a range of values in cost para
set.seed(1)
tune.out=tune(svm ,y~.,data=dat ,kernel ="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100) ))
summary(tune.out)
#the tune fxn stores the best model as well, we can call it down below
bestmod=tune.out$best.model
summary(bestmod)

#we can use the predict fxn to predict class label on a set of obs for any given value
#of the cost para, we start by making some fake data
xtest=matrix(rnorm (20*2) , ncol=2)
ytest=sample (c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]= xtest[ytest==1,] + 1
testdat=data.frame(x=xtest , y=as.factor(ytest))

#lets predict the class labels for these obs?
ypred=predict(bestmod ,testdat)
table(predict=ypred , truth=testdat$y )

#not bad, but what if our cost was .01 instead of .1?
svmfit=svm(y~., data=dat , kernel ="linear", cost =.01,scale=FALSE)
ypred=predict (svmfit ,testdat )
table(predict =ypred , truth=testdat$y )
#not as good!

#What happens if we truly have linearly separable observations?
#fake data that is linear seps
x[y==1,]=x[y==1,]+0.5
plot(x, col=(y+5)/2, pch =19)

#lets try to fit it, note cost is large so we have no misclassed obs!
dat=data.frame(x=x,y=as.factor(y))
svmfit=svm(y~., data=dat , kernel ="linear", cost=1e5)
summary(svmfit)
plot(svmfit , dat)

#we can see that the margin is VERY narrow, since obs close to our line are NOT in our margin!
#probably going to perform like garbage on actual test data

#lets try w/ a smaller cost fxn
svmfit=svm(y~., data=dat , kernel ="linear", cost=1)
summary(svmfit)
plot(svmfit ,dat)

#9.6.2 support vector machines
#can fit svm's with nlins by setting kernal values to polynomial or radial
#polynomial takes degree argument, radial takes gamma argument
#make fake data for nlinear class boundary
set.seed(1)
x=matrix(rnorm (200*2) , ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150 ,]=x[101:150,]-2
y=c(rep(1,150) ,rep(2,50))
dat=data.frame(x=x,y=as.factor(y))
plot(x,col=y)

#split our data into training and test
train=sample (200,100)
svmfit=svm(y~., data=dat[train ,], kernel ="radial", gamma=1,cost=1)
plot(svmfit , dat[train ,])
#some info about our fit
summary(svmfit)

#while there are some fitting errors, that we could reduce w/ a larger cost fxn
#however... this creates highly irregular fits and might lead to overfitting on the data
svmfit=svm(y~., data=dat[train ,], kernel ="radial",gamma=1,cost=1e5)
plot(svmfit ,dat[train ,])

#can perform cv using tune to select best gamma and cost for our radial fxn
set.seed(1)
tune.out=tune(svm , y~., data=dat[train ,], kernel ="radial",ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4) ))
summary(tune.out)

#we can view our test set predictions by applying the predict fxn
#we subset our data by using [-train] as an index set
table(true=dat[-train ,"y"], pred=predict(tune.out$best.model ,newdata =dat[-train ,]))
#12% misclass

#9.6.3 ROC curves
install.packages("ROCR")
library(ROCR)
#short fxn to plot ROC curve given a vector containing numerical score for each obs
#a pred, and a vector containing the class label for each obs, truth.
rocplot =function (pred , truth , ...){
  predob = prediction (pred , truth)
  perf = performance (predob , "tpr", "fpr")
  plot(perf ,...)}

#svms and svcs output class labels for each obs
#but... can also find fitted values for each obs
#which are numerical scores used to obtain the class labels!
#we can find these by setting decision.values = TRUE using svm, then predict spits out fitted values
svmfit.opt=svm(y~., data=dat[train ,], kernel ="radial",gamma=2, cost=1, decision.values =T)
fitted =attributes (predict (svmfit.opt ,dat[train ,], decision.values=TRUE))$decision.values

#fitting the ROC plot now w/ the addtl. info
par(mfrow=c(1,2))
rocplot (fitted ,dat[train ,"y"], main="Training Data")

#svm looks alright on the ROC!
#we can try w/ larger gamma to have more flexible fit and ostensiably better accuracy
svmfit.flex=svm(y~., data=dat[train ,], kernel ="radial",gamma=50, cost=1, decision.values =T)
fitted=attributes (predict (svmfit.flex ,dat[train ,], decision.values=T))$decision.values
rocplot (fitted ,dat[train ,"y"],add=T,col="red ")

#these ROCs are all on the training data... how does it look on the test data?
fitted =attributes (predict (svmfit.opt ,dat[-train ,], decision.values=T))$decision.values
rocplot (fitted ,dat[-train ,"y"], main="Test Data")
fitted=attributes (predict (svmfit.flex ,dat[- train ,], decision.values=T))$decision.values
rocplot (fitted ,dat[-train ,"y"],add=T,col="red")

#on our test data, gamma = 2 has a better result!

#9.6.4 SVM with multiple classes?
#svm defaults to one vs one approach w/ multiple classes
#fake dat w/ 3rd class
set.seed(1)
x=rbind(x, matrix(rnorm (50*2) , ncol=2))
y=c(y, rep(0,50))
x[y==0,2]= x[y==0 ,2]+2
dat=data.frame(x=x, y=as.factor(y))
par(mfrow=c(1,1))
plot(x,col=(y+1))

#svm to fit to the data
svmfit=svm(y~., data=dat , kernel ="radial", cost=10, gamma =1)
plot(svmfit , dat)
#note that the e1071 library can be used to do support vector regression as well
#if the response vec passed into svm is numerical vector instead of a factor

#9.6.5 Application to gene expression data
#looking at Khan data, tissue samples in 4 diff classes
#has 2308 genes, 63 obs in train, 20 obs in test
library(ISLR)
library(e1071)
#since P > N we use a linear kernal, bc addtl flexibilty from polynomial/radial is unecessary
dat=data.frame(x=Khan$xtrain , y=as.factor(Khan$ytrain ))
out=svm(y~., data=dat , kernel ="linear",cost=10)
summary (out)
table(out$fitted , dat$y)
#no training errors??? this is b/c if we have a large P it is ez to find a hyperplane that seps all 
#our observations, how does it do on the test data???
dat.te=data.frame(x=Khan$xtest , y=as.factor(Khan$ytest ))
pred.te=predict (out , newdata =dat.te)
table(pred.te, dat.te$y)

#with cost set to 10, we only get 2 errors! really good!