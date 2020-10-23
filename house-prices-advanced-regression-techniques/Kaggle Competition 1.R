houses<-read.csv("house-prices-advanced-regression-techniques/train.csv")
houses_test<-read.csv("house-prices-advanced-regression-techniques/test.csv")
houses$MSSubClass<-as.factor(houses$MSSubClass)
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
L<-vif(lm2)
?vif
#terms to look @ kicking out
#sale type, sale condition, gr liv area, foundation, exterior 1st, exter qual, bldgtype, housestyle, neighborhood, 
#mssubclass, mszoning

#plot to look @ shit
for(i in 2:54){
  plot(y=houses_clean$SalePrice, x=houses_clean[,i], xlab=colnames(houses_clean)[i])
}
#consider feature engineering (collapsing variables mostly?)
#which and where do i choose to do this
#collapse then not collapse, compare against each other.


#yrsold doesn't matter?


#here we write a cv loop to eliminate tings
which(L[,1] > 10)
max_vif<-100000
vars_remove<-c(NA)
while(max_vif > 10){
  if(sum(!is.na(vars_remove))==0){ mod<-lm(SalePrice~.,data=houses_clean)
  }else{mod<-lm(SalePrice~.,data=houses_clean[,-vars_remove])}
  L<-vif(mod)
  n<-which.max(L[,1])
  if(L[n,1]>10){vars_remove<-c(vars_remove,names(L[n,1]))}
  max_vif<-L[n,1]
  print(max_vif)
}
#code doesn't work, ethan is a scrub
#perhaps ignore multicollinarity
#code for our own k-fold cv
K=5
folds = sample(1:K,nrow(houses_clean),replace=T)
houses_lm<-list(NA)
pred_lm<-list(NA)
test_mse<-list(NA)
for(k in 1:K){
  
  CV.train = houses_clean[folds != k,]
  CV.test = houses_clean[folds == k,]
  CV.ts_y = CV.test$SalePrice
  houses_lm[[k]]=lm(SalePrice~.,data=CV.train)
  pred_lm[[k]]=predict(object=houses_lm[[k]], newdata=CV.test)
  test_mse[[k]]<-mean((pred_lm[[k]] - CV.ts_y)^2)
}

houses_clean %>% group_by(Condition2) %>% summarise(Prop=n()/nrow(houses_clean))





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


#boosting and RF, lm, but also genearllized Lm (gamma distri), perhaps a MLM?