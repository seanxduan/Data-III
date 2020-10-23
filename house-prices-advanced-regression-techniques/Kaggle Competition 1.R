houses<-read.csv("house-prices-advanced-regression-techniques/train.csv")
houses_test<-read.csv("house-prices-advanced-regression-techniques/test.csv")

#lets see how good our model is, looking at best subset r
library(leaps)
regfit.full=regsubsets(SalePrice~.,data=houses, nvmax=20)
reg.summary<-summary(regfit.full)
reg.summary$bic

regfit.fwd=regsubsets(SalePrice~.,data=houses , nvmax=50,method ="forward ")
summary (regfit.fwd)

#hmm gotta see if we can drop NA columns first
library(dplyr)
houses_nna<-houses %>%select_if(~ !any(is.na(.)))
#still unsure how xactly this works... but it's fine?

#too big, see if we can check corr and reduce
regfit.full=regsubsets(SalePrice~.,data=houses_clean, nvmax=61)
reg.summary<-summary(regfit.full)
reg.summary$bic

regfit.fwd=regsubsets(SalePrice~.,data=houses , nvmax=50,method ="forward ")
summary (regfit.fwd)

#try basic linear reg
lm1<-lm(SalePrice~., data=houses_nna)
summary(lm1)
library(car)
vif(lm1)
#checking cor
alias(lm1)
#lets remove corr columns
#removed the alias'd columns in our lm1, removed the 'subsets' instead of the larger category (could do vice/versa)
houses_clean<-subset(houses_nna, select=-c(X1stFlrSF, X2ndFlrSF, LowQualFinSF, BsmtFinSF1, BsmtFinSF2, BsmtUnfSF, Exterior2nd))
#ok, now that we've cleand data, lets see a lm again
lm2<-lm(SalePrice~., data=houses_clean)
summary(lm2)
vif(lm2)
#terms to look @ kicking out
#sale type, sale condition, gr liv area, foundation, exterior 1st, exter qual, bldgtype, housestyle, neighborhood, 
#mssubclass, mszoning



##lets try a fresh stab using other methods... perhaps regression tree methods?
library(tree)
library(ISLR)
library(MASS)
#ensure that we have factors instead of characters
library(tidyverse)
houses_clean <- houses_clean %>% mutate_if(is.character,as.factor)
#perhaps push this code up earlier so we dont have 2 do it here?
tr1<-tree(SalePrice~., data=houses_clean)
summary(tr1)
#only 5 variables used here, overall qual, neighborhood, Grlivarea, total basementsf, year remmodel add
plot(tr1)
text(tr1, pretty=0)
#try to prune or basic tree
cv_tr1<-cv.tree(tr1)
plot(cv_tr1$size ,cv_tr1$dev ,type="b")
#looks like our largest tree is the best, but perhaps a 5 node tree is comparable

## bagging?
library(randomForest)
tr2<-randomForest(SalePrice~., data=houses_clean, mtry = 54, importance = TRUE)
tr2
summary(tr2)
importance(tr2)
varImpPlot(tr2)
#overall qual best for node purity
#mse red is gr liv area, neighborhood, overall qual, totalbsmt SF