library(dplyr)
library(leaps)
library(tidyverse)
library(forcats)
library(tree)
library(ISLR)
library(MASS)
library(car)
library(randomForest)
library(gbm)


#data loading, prep, and cleaning step.
houses<-read.csv("house-prices-advanced-regression-techniques/train.csv")
houses_test<-read.csv("house-prices-advanced-regression-techniques/test.csv")
houses$MSSubClass<-as.factor(houses$MSSubClass)
houses_nna<-houses %>%select_if(~ !any(is.na(.)))
houses_clean<-subset(houses_nna, select=-c(X1stFlrSF, X2ndFlrSF, LowQualFinSF, BsmtFinSF1, BsmtFinSF2, BsmtUnfSF, Exterior2nd, Utilities, Id))


#ok, can't do k-fold b/c we have lack of uniqueness in railroad
#time for feature engineering - collapse the railroad categories
#and positive benefit categories
houses_clean$Condition2<-fct_recode(houses_clean$Condition2, AF = "Artery", AF = "Feedr", RR = "RRAe", RR = "RRAn", RR = "RRNn", RR = "RRNe", Pos = "PosA", Pos = "PosN", NULL = "H")
houses_clean$Condition1<-fct_recode(houses_clean$Condition1, AF = "Artery", AF = "Feedr", RR = "RRAe", RR = "RRAn", RR = "RRNn", RR = "RRNe", Pos = "PosA", Pos = "PosN", NULL = "H")
#mesh roof type to composite/ not composite
houses_clean$RoofMatl<-fct_recode(houses_clean$RoofMatl, Oth = "ClyTile", Oth = "Membran", Oth = "Metal", Oth = "Roll", Oth = "Tar&Grv", Oth = "WdShake", Oth = "WdShngl", NULL = "H")
#collapse shingles, bricks/block/sstone, imstucco w/ stucco, 
houses_clean$Exterior1st<-fct_recode(houses_clean$Exterior1st, Shng = "AbShng",Shng = "AsphShn",Shng = "WdShing", Brx = "BrkComm",
                                     Brx = "BrkFace", Brx = "CBlock",Brx = "Stone",Stuc = "Stucco",Stuc = "ImStucc", NULL = "H")
#collapse poor and fair
houses_clean$ExterCond<-fct_recode(houses_clean$ExterCond , PF = "Po", PF = "Fa", NULL = "H")
#collapse heating, not gas
houses_clean$Heating<-fct_recode(houses_clean$Heating , Oth = "Floor", Oth = "Grav", Oth = "OthW", Oth = "Wall", NULL = "H")
#collapse poor and fair
houses_clean$HeatingQC<-fct_recode(houses_clean$HeatingQC , PF = "Po", PF = "Fa", NULL = "H")

#collapse contract terms, warranties deeds
houses_clean$SaleType<-fct_recode(houses_clean$SaleType ,Cont = "Oth", Cont = "Con", Cont = "ConLD", Cont = "ConLI", Cont = "ConLw", WTD = "CWD", WTD = "WD", NULL = "H")

#collapse bluestem to NAmes(most similar)
houses_clean$Neighborhood<-fct_recode(houses_clean$Neighborhood , NAmesP = "Blueste", NAmesP = "NAmes", NULL = "H")

#collapse roof type shd w/ flat
houses_clean$RoofStyle<-fct_recode(houses_clean$RoofStyle , FS = "Flat", FS = "Shed", NULL = "H")

#collapse stone w/ wood for 'othr' foundatain
houses_clean$Foundation<-fct_recode(houses_clean$Foundation , Oth = "Stone", Oth = "Wood", NULL = "H")

#collapse major 1-2 and severe
houses_clean$Functional<-fct_recode(houses_clean$Functional , MSev = "Maj1", MSev = "Maj2", MSev = "Sev", NULL = "H")

#collapse frontage
houses_clean$LotConfig<-fct_recode(houses_clean$LotConfig , FR = "FR2", FR = "FR3", NULL = "H")

#collapse mssubclass 40 and 45
houses_clean$MSSubClass<-fct_recode(houses_clean$MSSubClass , "42.5" = "40", "42.5" = "45", NULL = "H")


#quick loop to see #'s of all tables
for (i  in 1:53) {
  print(colnames(houses_clean)[i])
  print(table(houses_clean[,i]))
  
}


#make sure all our characters are factors
houses_clean <- houses_clean %>% mutate_if(is.character,as.factor)


vif(lm1)
#checking cor
alias(lm1)
#lets remove corr columns
#removed the alias'd columns in our lm1, removed the 'subsets' instead of the larger category (could do vice/versa)
#ok, now that we've cleand data, lets see a lm again
lm2<-lm(SalePrice~., data=houses_clean)
summary(lm2)
L<-vif(lm2)
predict_lm<-predict(lm2, newdata = houses_clean_tst)
View(predict_lm)

#plot to look @ shit
for(i in 1:53){
  plot(y=houses_clean$SalePrice, x=houses_clean[,i], xlab=colnames(houses_clean)[i])
}
#consider feature engineering (collapsing variables mostly?)
#which and where do i choose to do this
#collapse then not collapse, compare against each other.
#terms to look @ kicking out
#sale type, sale condition, gr liv area, foundation, exterior 1st, exter qual, bldgtype, housestyle, neighborhood, 
#mssubclass, mszoning
#yrsold doesn't matter?



#code for our own k-fold cv
set.seed(3)
K=2
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
test_mse
mean(c(test_mse[[1]], test_mse[[2]]))



##lets try a fresh stab using other methods... perhaps regression tree methods?
#ensure that we have factors instead of characters
tr1<-tree(SalePrice~., data=houses_clean)
summary(tr1)
#only 5 variables used here, overall qual, neighborhood, Grlivarea, total basementsf, year remmodel add
plot(tr1)
text(tr1, pretty=0)
#try to prune or basic tree
cv_tr1<-cv.tree(tr1)
plot(cv_tr1$size ,cv_tr1$dev ,type="b")
#looks like our largest tree is the best, but perhaps a 8 node tree is comparable
#going to have to test predicted mse tho

K=2
folds = sample(1:K,nrow(houses_clean),replace=T)
houses_tr<-list(NA)
pred_tr<-list(NA)
test_mse<-list(NA)
for(k in 1:K){
  CV.train = houses_clean[folds != k,]
  CV.test = houses_clean[folds == k,]
  CV.ts_y = CV.test$SalePrice
  houses_tr[[k]]=tree(SalePrice~., data = CV.train)
  pred_tr[[k]]=predict(object=houses_tr[[k]], newdata=CV.test)
  test_mse[[k]]<-mean((pred_tr[[k]] - CV.ts_y)^2)
}
test_mse
mean(c(test_mse[[1]], test_mse[[2]]))

predict_tr<-predict(tr1,newdata=houses_clean_tst)

## bagging?
tr2<-randomForest(SalePrice~., data=houses_clean, mtry = 54, importance = TRUE)
tr2
summary(tr2)
importance(tr2)
varImpPlot(tr2)

K=2
folds = sample(1:K,nrow(houses_clean),replace=T)
houses_bag<-list(NA)
pred_bag<-list(NA)
test_mse<-list(NA)
for(k in 1:K){
  CV.train = houses_clean[folds != k,]
  CV.test = houses_clean[folds == k,]
  CV.ts_y = CV.test$SalePrice
  houses_bag[[k]]=randomForest(SalePrice~., data = CV.train, mtry=52, importance = TRUE)
  pred_bag[[k]]=predict(object=houses_bag[[k]], newdata=CV.test)
  test_mse[[k]]<-mean((pred_bag[[k]] - CV.ts_y)^2)
}
test_mse
mean(c(test_mse[[1]], test_mse[[2]]))

#overall qual best for node purity
#mse red is gr liv area, neighborhood, overall qual, totalbsmt SF

#code for testing our boosting
houses_boost<-list(NA)
yhat.boost<-list(NA)
moderror<-list(NA)
bestmodel<-list(NA)
testn<-seq(from=100, to=10000, length.out = 10)
for(k in 1:K){
  CV.train = houses_clean[folds != k,]
  CV.test = houses_clean[folds == k,]
  CV.ts_y = CV.test$SalePrice
  for (i in 1:10){
    houses_boost[[i]]=gbm(SalePrice~.,data=CV.train, distribution="gaussian",n.trees=testn[[i]], interaction.depth=1)
    yhat.boost[[i]]=predict(houses_boost[[i]] ,newdata =CV.test,n.trees=testn[[i]])
    test_mse[[i]]<-mean((yhat.boost[[i]] - CV.ts_y)^2)
  }
  moderror[[k]]<-test_mse[[which.min(test_mse)]]
  bestmodel[[k]]<-which.min(test_mse)}
moderror
bestmodel
#best tree size is 1200
for(k in 1:K){
  CV.train = houses_clean[folds != k,]
  CV.test = houses_clean[folds == k,]
  CV.ts_y = CV.test$SalePrice
  for (i in 1:10){
    houses_boost[[i]]=gbm(SalePrice~.,data=CV.train, distribution="gaussian",n.trees=testn[[i]], interaction.depth=2)
    yhat.boost[[i]]=predict(houses_boost[[i]] ,newdata =CV.test,n.trees=testn[[i]])
    test_mse[[i]]<-mean((yhat.boost[[i]] - CV.ts_y)^2)
  }
  moderror[[k]]<-test_mse[[which.min(test_mse)]]
  bestmodel[[k]]<-which.min(test_mse)}
moderror
bestmodel
#lets try it but w/ diff depths

for(k in 1:K){
  CV.train = houses_clean[folds != k,]
  CV.test = houses_clean[folds == k,]
  CV.ts_y = CV.test$SalePrice
  for (i in 1:5){
    houses_boost[[i]]=gbm(SalePrice~.,data=CV.train, distribution="gaussian",n.trees=1200, interaction.depth=i)
    yhat.boost[[i]]=predict(houses_boost[[i]] ,newdata =CV.test,n.trees=1200)
    test_mse[[i]]<-mean((yhat.boost[[i]] - CV.ts_y)^2)
  }
  moderror[[k]]<-test_mse[[which.min(test_mse)]]
  bestmodel[[k]]<-which.min(test_mse)}
moderror
bestmodel

#our best ntrees is 1200, depth = 2
houses_boost=gbm(SalePrice~.,data=houses_clean, distribution="gaussian",n.trees=1200, interaction.depth=2)
predict_boost=predict(houses_boost, newdata=houses_clean_tst)

#RF code

houses_rf<-list(NA)
yhat.rf<-list(NA)
mvec<-seq(from=3, to=8)

for(k in 1:K){
  CV.train = houses_clean[folds != k,]
  CV.test = houses_clean[folds == k,]
  CV.ts_y = CV.test$SalePrice
  for(i in 1:length(mvec)){
    houses_rf[[i]]=randomForest(SalePrice~.,data=CV.train,mtry=mvec[i],importance =TRUE)
    yhat.rf[[i]]<-predict(houses_rf[[i]] , newdata=CV.test)
    test_mse[[i]]<-mean((yhat.rf[[i]] -CV.ts_y)^2)
  }
  moderror[[k]]<-test_mse[[which.min(test_mse)]]
  bestmodel[[k]]<-which.min(test_mse)}
moderror
bestmodel


#tried various mvecs earlier from 1 to 52, split into smaller sets (in order to prevent our processing from hanging up)
#best mvec found was for 6.
#final mvec for testg
houses_rf=randomForest(SalePrice~.,data=houses_clean,mtry=6,importance =TRUE)
predict_rf=predict(houses_rf, newdata = houses_clean_tst)
predict_rf
#WHY THE FUCK DOES THIS NOT WORK??????
#SERIOUSLY WHY
# I"M SO ANGRY
# I MANUALLY CHECKED EACH VECTOR AND THEY ALL HAVE THE SAME FACTOR LEVELS
#JESUS CHRIST FUCK


#code to prep houses-test
varnames<-names(houses_clean)
houses_clean_tst<-houses_test
houses_clean_tst<-houses_clean_tst[ names(houses_clean_tst)[names(houses_clean_tst) %in% varnames] ]

#time for feature engineering - collapse the railroad categories
#and positive benefit categories
houses_clean_tst$Condition2<-fct_recode(houses_clean_tst$Condition2, AF = "Artery", AF = "Feedr", RR = "RRAe", RR = "RRAn", RR = "RRNn", RR = "RRNe", Pos = "PosA", Pos = "PosN", NULL = "H")
houses_clean_tst$Condition1<-fct_recode(houses_clean_tst$Condition1, AF = "Artery", AF = "Feedr", RR = "RRAe", RR = "RRAn", RR = "RRNn", RR = "RRNe", Pos = "PosA", Pos = "PosN", NULL = "H")
#mesh roof type to composite/ not composite
houses_clean_tst$RoofMatl<-fct_recode(houses_clean_tst$RoofMatl, Oth = "ClyTile", Oth = "Membran", Oth = "Metal", Oth = "Roll", Oth = "Tar&Grv", Oth = "WdShake", Oth = "WdShngl", NULL = "H")
#collapse shingles, bricks/block/sstone, imstucco w/ stucco, 
houses_clean_tst$Exterior1st<-fct_recode(houses_clean_tst$Exterior1st, Shng = "AbShng",Shng = "AsphShn",Shng = "WdShing", Brx = "BrkComm",
                                     Brx = "BrkFace", Brx = "CBlock",Brx = "Stone",Stuc = "Stucco",Stuc = "ImStucc", NULL = "H")
#collapse poor and fair
houses_clean_tst$ExterCond<-fct_recode(houses_clean_tst$ExterCond , PF = "Po", PF = "Fa", NULL = "H")
#collapse heating, not gas
houses_clean_tst$Heating<-fct_recode(houses_clean_tst$Heating , Oth = "Floor", Oth = "Grav", Oth = "OthW", Oth = "Wall", NULL = "H")
#collapse poor and fair
houses_clean_tst$HeatingQC<-fct_recode(houses_clean_tst$HeatingQC , PF = "Po", PF = "Fa", NULL = "H")

#collapse contract terms, warranties deeds
houses_clean_tst$SaleType<-fct_recode(houses_clean_tst$SaleType ,Cont = "Oth", Cont = "Con", Cont = "ConLD", Cont = "ConLI", Cont = "ConLw", WTD = "CWD", WTD = "WD", NULL = "H")

#collapse bluestem to NAmes(most similar)
houses_clean_tst$Neighborhood<-fct_recode(houses_clean_tst$Neighborhood , NAmesP = "Blueste", NAmesP = "NAmes", NULL = "H")

#collapse roof type shd w/ flat
houses_clean_tst$RoofStyle<-fct_recode(houses_clean_tst$RoofStyle , FS = "Flat", FS = "Shed", NULL = "H")

#collapse stone w/ wood for 'othr' foundatain
houses_clean_tst$Foundation<-fct_recode(houses_clean_tst$Foundation , Oth = "Stone", Oth = "Wood", NULL = "H")

#collapse major 1-2 and severe
houses_clean_tst$Functional<-fct_recode(houses_clean_tst$Functional , MSev = "Maj1", MSev = "Maj2", MSev = "Sev", NULL = "H")

#collapse frontage
houses_clean_tst$LotConfig<-fct_recode(houses_clean_tst$LotConfig , FR = "FR2", FR = "FR3", NULL = "H")

#collapse mssubclass 40 and 45
houses_clean_tst$MSSubClass<-as.factor(houses_clean_tst$MSSubClass)
houses_clean_tst$MSSubClass<-fct_recode(houses_clean_tst$MSSubClass , "42.5" = "40", "42.5" = "45", "160" = "150", NULL = "H")


#imputing missing values
houses_clean_tst$MSZoning<-replace_na(houses_clean_tst$MSZoning, "RM")
houses_clean_tst$Exterior1st<-replace_na(houses_clean_tst$Exterior1st, "MetalSd")
houses_clean_tst$TotalBsmtSF<-replace_na(houses_clean_tst$TotalBsmtSF, 0)
houses_clean_tst$BsmtFullBath<-replace_na(houses_clean_tst$BsmtFullBath, 0)
houses_clean_tst$BsmtHalfBath<-replace_na(houses_clean_tst$BsmtHalfBath, 0)
houses_clean_tst$SaleType<-replace_na(houses_clean_tst$SaleType, "New")
houses_clean_tst$GarageCars<-replace_na(houses_clean_tst$GarageCars, 0)
houses_clean_tst$GarageArea<-replace_na(houses_clean_tst$GarageArea, 0)
houses_clean_tst$Functional<-replace_na(houses_clean_tst$Functional, "Typ")
houses_clean_tst$KitchenQual<-replace_na(houses_clean_tst$KitchenQual, "TA")


#make sure all our characters are factors
houses_clean_tst <- houses_clean_tst %>% mutate_if(is.character,as.factor)

#quick loop to see #'s of all tables
for (i  in 1:52) {
  print(colnames(houses_clean)[i])
  print(table(houses_clean[,i]))
  
}
#quick loop to see #'s of all tables
for (i  in 1:52) {
  print(colnames(houses_clean_tst)[i])
  print(table(houses_clean_tst[,i]))
  
}
