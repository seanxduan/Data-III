#neural net lab
#10/27/20
install.packages("ANN2")

library(ANN2)

#read in our data
bhouse <- read.table("housingdata")
X = bhouse[,1:13]
Y = bhouse[,14]

## make training and test samples 85% training, 15% test
set.seed(1)
trainInds <- sample(1:nrow(X), round(nrow(X)*0.85))
valInds <- (1:nrow(X))[-trainInds]
Xtrain <- X[trainInds, ]
Ytrain <- Y[trainInds]
Xtest <- X[valInds, ]
Ytest <- Y[valInds]
# find mean and sd column-wise of training data (not surprisingily called
# a "z-transform" in the machine learning world)
XtrainMean <- apply(Xtrain,2,mean)
XtrainSd <- apply(Xtrain,2,sd)
Xtrain_stand <- sweep(sweep(Xtrain, 2L, XtrainMean), 2, XtrainSd, "/")

#see if scale does it faster?
scaled_x<-scale(Xtrain)
scaled_x<-as.data.frame(scaled_x)
#it does the xact thing!

# check means and standard deviations
colMeans(Xtrain_stand)
apply(Xtrain_stand, 2, sd)

colMeans(scaled_x)
apply(scaled_x, 2, sd)
#why is the scaled one not the same?? j/w

# now, standardize the test data using the training means/sd; as
# expected, these don't have exactly mean 0 and sd 1
Xtest_stand <- sweep(sweep(Xtest, 2L, XtrainMean), 2, XtrainSd, "/")
colMeans(Xtest_stand)
apply(Xtest_stand, 2, sd)
####ahh, we do it this way so we can scale our test data by our training data!

# standardize the Y's
Ytrain_stand <- (Ytrain - mean(Ytrain))/sd(Ytrain)
Ytest_stand <- (Ytest - mean(Ytrain))/sd(Ytrain)

# Now, train the deep NN. Make sure you look at the help for the ``neuralnetwork''
# function to see the various options. For regression, there are several loss
# functions you can choose, but the standard is squared error (of course). I just
# choose an architecture here (number of hidden layers, units, optimizer,
# activation function, etc.). You will definitely want to play with these. In
# this application, it is VERY fast, so easy to do.
?neuralnetwork
NNbhouse <- neuralnetwork(X = Xtrain_stand, y = Ytrain_stand,regression = TRUE,
                          standardize = FALSE, loss.type = "squared",
                          hidden.layers =c(64,32,16),
                          optim.type = 'adam',n.epochs = 15,activ.functions = "relu",
                          learn.rates = 0.01, val.prop = 0.2)

NNbhouse2 <- neuralnetwork(X = Xtrain_stand, y = Ytrain_stand,regression = TRUE,
                          standardize = FALSE, loss.type = "squared",
                          hidden.layers =c(64,32,16),
                          optim.type = 'adam',n.epochs = 15,activ.functions = "relu",
                          learn.rates = 0.1, val.prop = 0.2)
#much worse!

NNbhouse3 <- neuralnetwork(X = Xtrain_stand, y = Ytrain_stand,regression = TRUE,
                          standardize = FALSE, loss.type = "squared",
                          hidden.layers =c(64,32,16),
                          optim.type = 'adam',n.epochs = 15,activ.functions = "relu",
                          learn.rates = 0.001, val.prop = 0.2)
#slightly worse??

NNbhouse4 <- neuralnetwork(X = Xtrain_stand, y = Ytrain_stand,regression = TRUE,
                          standardize = FALSE, loss.type = "squared",
                          hidden.layers =c(64,32,16),
                          optim.type = 'adam',n.epochs = 10,activ.functions = "relu",
                          learn.rates = 0.01, val.prop = 0.2)
#even more slightly worse?

NNbhouse5 <- neuralnetwork(X = Xtrain_stand, y = Ytrain_stand,regression = TRUE,
                          standardize = FALSE, loss.type = "squared",
                          hidden.layers =c(64,32,16),
                          optim.type = 'adam',n.epochs = 30,activ.functions = "relu",
                          learn.rates = 0.01, val.prop = 0.2)
#the best yet!?

# Plot the loss during training (also, the validation sample if val.prop ne 0)
plot(NNbhouse)
plot(NNbhouse2)
plot(NNbhouse3)
plot(NNbhouse4)
plot(NNbhouse5)

# summarize the NN
print(NNbhouse)
#predictions
y_pred <- predict(NNbhouse, newdata = Xtest_stand)
# MSE
MSE <- mean((Ytest_stand - y_pred$predictions)^2)
MSE
#how does this MSE compare to multiple regression, or lasso, or ridge, etc.?
#we can shoot for mreg
#make testdata
testdat<-cbind(Ytrain_stand, Xtrain_stand)
tstlm<-lm(Ytrain_stand~.,data=testdat)
y_pred2<-predict(tstlm, newdata = Xtest_stand)
MSE2 <- mean((Ytest_stand - y_pred2)^2)
MSE2
#oh much better than lm!

#buzz dataset
load("BuzzRDataFiles/buzz_X1.RData")
load("BuzzRDataFiles/buzz_X2.RData")
load("BuzzRDataFiles/buzz_X3.RData")
load("BuzzRDataFiles/buzz_X4.RData")
load("BuzzRDataFiles/buzz_X5.RData")
load("BuzzRDataFiles/buzz_X6.RData")
load("BuzzRDataFiles/buzz_Y.RData")

X <- rbind(X1, X2, X3, X4, X5, X6)
str(X)

str(Y)

unique(Y)

# select training (90%) and validation samples (10%) randomly
set.seed(1)
trainInds <- sample(1:nrow(X), round(nrow(X)*0.9))
valInds <- (1:nrow(X))[-trainInds]
Xtrain <- X[trainInds, ]
Ytrain_classes <- Y[trainInds]
Xtest <- X[valInds, ]
Ytest_classes <- Y[valInds]
# rescale function to scale training X's between 0-1 (alternatively, you
# could standardize as we usually do by dividing by the standard deviation);
# Here is a little function to scale between 0-1; note, we have to apply the same 
#transformation that was 

rescaleCols <- function(rowX, colMins, colMaxs)
{
  r <- (rowX - colMins)/(colMaxs - colMins)
  r[is.nan(r)] <- 0
  return(r)
}
colMinsX <- apply(Xtrain, 2, min)
colMaxsX <- apply(Xtrain, 2, max)

Xtrain_scaled <- t(apply(Xtrain, 1, rescaleCols, colMinsX, colMaxsX))
Xtest_scaled <- t(apply(Xtest, 1, rescaleCols, colMinsX, colMaxsX))
# it is good to look to see how the scaling worked - especially on
# the test set
range(Xtrain_scaled)

# convert the responses to factor variables
Ytrain <- as.factor(Ytrain_classes)
Ytest <- as.factor(Ytest_classes)


# Now, train the deep NN. Make sure you look at the help for the ``neuralnetwork''
# function to see the various options. For classification, it uses the ``log''
# loss, which is cross-entropy. Note, in this problem, it can take a LONG time
# to train if you have a lot of hidden layers or hidden units. Also, letting the
# proportion of training data used for tracing the loss on the validation set,
# val.prop be > 0 adds quite a bit more time, but it is useful to do to see
# the convergence better (in my opinion). But, if you want to play with options,
# probably best to set it to 0.

NN <- neuralnetwork(X = Xtrain_scaled, y = Ytrain, hidden.layers = c(64,32,32),
                    optim.type = 'adam',n.epochs = 10,activ.functions = "relu",
                    learn.rates = 0.01, val.prop = 0.05)
# Plot the loss during training (also, the validation sample if val.prop ne 0)
plot(NN)
# Make predictions
y_pred <- predict(NN, newdata = Xtest_scaled)
# Confusion matrix
tabl_pred <- table(truth = Ytest, fitted = y_pred$predictions)
tabl_pred
# Accuracy
sum(diag(tabl_pred))/length(Ytest)

#lets futz with it some more!


NN <- neuralnetwork(X = Xtrain_scaled, y = Ytrain, hidden.layers = c(64,32,32),
                    optim.type = 'adam',n.epochs = 30,activ.functions = "relu",
                    learn.rates = 0.01, val.prop = 0.05)
# Plot the loss during training (also, the validation sample if val.prop ne 0)
plot(NN)
# Make predictions
y_pred <- predict(NN, newdata = Xtest_scaled)
# Confusion matrix
tabl_pred <- table(truth = Ytest, fitted = y_pred$predictions)
tabl_pred
# Accuracy
sum(diag(tabl_pred))/length(Ytest)
# gained a whole PERCENT of accuracy???


NN <- neuralnetwork(X = Xtrain_scaled, y = Ytrain, hidden.layers = c(64,32,32,16),
                    optim.type = 'adam',n.epochs = 10,activ.functions = "relu",
                    learn.rates = 0.01, val.prop = 0.05)
# Plot the loss during training (also, the validation sample if val.prop ne 0)
plot(NN)
# Make predictions
y_pred <- predict(NN, newdata = Xtest_scaled)
# Confusion matrix
tabl_pred <- table(truth = Ytest, fitted = y_pred$predictions)
tabl_pred
# Accuracy
sum(diag(tabl_pred))/length(Ytest)
# even better!

NN <- neuralnetwork(X = Xtrain_scaled, y = Ytrain, hidden.layers = c(64,64,32,16),
                    optim.type = 'adam',n.epochs = 30,activ.functions = "relu",
                    learn.rates = 0.01, val.prop = 0.05)
# Plot the loss during training (also, the validation sample if val.prop ne 0)
plot(NN)
# Make predictions
y_pred <- predict(NN, newdata = Xtest_scaled)
# Confusion matrix
tabl_pred <- table(truth = Ytest, fitted = y_pred$predictions)
tabl_pred
# Accuracy
sum(diag(tabl_pred))/length(Ytest)
#woW! 3% increase in accuracy!!!!!!

NN <- neuralnetwork(X = Xtrain_scaled, y = Ytrain, hidden.layers = c(64,64,32,16),
                    optim.type = 'adam',n.epochs = 30,activ.functions = "relu",
                    learn.rates = 0.01, val.prop = 0.05)
# Plot the loss during training (also, the validation sample if val.prop ne 0)
plot(NN)
# Make predictions
y_pred <- predict(NN, newdata = Xtest_scaled)
# Confusion matrix
tabl_pred <- table(truth = Ytest, fitted = y_pred$predictions)
tabl_pred
# Accuracy
sum(diag(tabl_pred))/length(Ytest)

#alt test
NN <- neuralnetwork(X = Xtrain_scaled, y = Ytrain, hidden.layers = c(64,32,32),
                    optim.type = 'adam',n.epochs = 10,activ.functions = "relu",
                    learn.rates = 0.01, val.prop = 0.1)
# Plot the loss during training (also, the validation sample if val.prop ne 0)
plot(NN)
# Make predictions
y_pred <- predict(NN, newdata = Xtest_scaled)
# Confusion matrix
tabl_pred <- table(truth = Ytest, fitted = y_pred$predictions)
tabl_pred
# Accuracy
sum(diag(tabl_pred))/length(Ytest)
