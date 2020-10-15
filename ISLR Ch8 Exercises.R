#ISLR ch8 exercises#
library(ISLR)
install.packages("tree")
library(tree)
attach(Carseats)
data(Carseats)
High=ifelse(Sales <=8,"No","Yes ")
#set up our df
Carseats =data.frame(Carseats ,High)

#classification trees first!
#make our first tree using tree command
tree.carseats =tree(High~.-Sales , Carseats )
summary(tree.carseats)

#code for plotting the tree
plot(tree.carseats )
text(tree.carseats ,pretty =0)

#just output the tree object  for more data
tree.carseats

#code to check training vs test set validation
set.seed(2)
train=sample (1: nrow(Carseats ), 200)
Carseats.test=Carseats [-train ,]
High.test=High[-train]
tree.carseats =tree(High~.-Sales , Carseats ,subset=train)
tree.pred=predict(tree.carseats ,Carseats.test ,type="class")
table(tree.pred ,High.test)
mean(tree.pred==High.test)

#code to see if pruning the tree leads to better results
#note fun = prune.misclass lets us use misclassification rate as our pruning critera
#instead of the default, which is deviance
set.seed(3)
cv.carseats =cv.tree(tree.carseats ,FUN=prune.misclass )
names(cv.carseats )
cv.carseats$size
# number of terminal nodes on each tree we're looking at
cv.carseats$dev
#crossvalidation error rate for our trees
cv.carseats$k
#note that the tree w/ 21 or 8 termial nodes has the lowest cv error rate


#plotting error rate as a fxn of size and k
par(mfrow=c(1,2))
plot(cv.carseats$size ,cv.carseats$dev ,type="b")
plot(cv.carseats$k ,cv.carseats$dev ,type="b")

#use prune.misclas fxn to get the best tree w/ our sizes
prune.carseats =prune.misclass (tree.carseats ,best=21)
plot(prune.carseats )
text(prune.carseats ,pretty =0)

#checking our results of pruned tree on the test/train dataset
tree.pred=predict(prune.carseats ,Carseats.test , type="class")
table(tree.pred ,High.test)
mean(tree.pred==High.test)
#slightly better than our full tree!

## Regession Trees
library(MASS)
set.seed(1)
train = sample (1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~.,Boston , subset=train)
summary(tree.boston)
#only 4 of our vars are used in tree construction!
#deviance here is the SSE
#plotting our tree
plot(tree.boston)
text(tree.boston, pretty=0)

#using cv.tree to see if pruning improves performance
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size ,cv.boston$dev ,type="b")
#our most complex tree is the best here

#we can try to prune regardless tho just to see
prune.boston=prune.tree(tree.boston ,best=5)
plot(prune.boston)
text(prune.boston , pretty =0)

#let use use our unpruned tree to make predictions test/train
yhat=predict (tree.boston ,newdata=Boston[-train ,])
boston.test=Boston[-train ,"medv"]
plot(yhat ,boston.test)
abline (0,1)
mean((yhat -boston.test)^2)

#bagging and RF
#note that we can do bagging in the RF packages by setting m=p
install.packages("randomForest")
library(randomForest)
set.seed(1)
bag.boston= randomForest( medv~.,data=Boston , subset=train ,mtry=13,importance =TRUE)
bag.boston

#checking to see if this prediction works well using cv on train/test
yhat.bag = predict (bag.boston , newdata=Boston[-train ,])
plot(yhat.bag , boston.test)
abline (0,1)
mean((yhat.bag -boston.test)^2)
#test MSE is 23.59
#way better than a single unpruned tree

#can change B by setting ntree argument to diff values
bag.boston= randomForest( medv~.,data=Boston , subset=train ,mtry=13,ntree=25)
yhat.bag = predict (bag.boston , newdata=Boston[-train ,])
mean((yhat.bag -boston.test)^2)
#worse w/ a smaller # of trees here tho

#setting m by changing mtry value
set.seed(1)
rf.boston= randomForest(medv~.,data=Boston , subset=train ,mtry=6, importance =TRUE)
yhat.rf = predict(rf.boston ,newdata=Boston[-train ,])
mean((yhat.rf-boston.test)^2)
#wow even better performance! using RF!!

#we can check importance of each variable by using the importance() fxn
importance(rf.boston)
#we can plot these using this code
varImpPlot(rf.boston)

#boosting!
install.packages("gbm")
library(gbm)

set.seed(1)
boost.boston=gbm(medv~.,data=Boston[train ,], distribution="gaussian",n.trees=5000, interaction.depth=4)
#code that presents relative infl plot and rel infl stats
summary(boost.boston)

#producing partial dependence plots as well
par(mfrow=c(1,2))
plot(boost.boston ,i="rm")
plot(boost.boston ,i="lstat")
#the marginal fx of selected var after integrating out other vars
#house prises incr w/ rm, and decrease w/ lstat

#use boosted model to predict medv on our test set
yhat.boost=predict (boost.boston ,newdata =Boston[-train ,],n.trees=5000)
mean((yhat.boost - boston.test)^2)
#test MSE is even better than our rf tree!

#lets try it w/ some different parameters!
boost.boston=gbm(medv~.,data=Boston[train ,], distribution="gaussian",n.trees=5000, interaction.depth=4,
                 shrinkage=.2,verbose=F)
yhat.boost=predict (boost.boston ,newdata =Boston[-train ,],n.trees=5000)
mean((yhat.boost - boston.test)^2)
#even better w/ smaller shrinkage factor?
