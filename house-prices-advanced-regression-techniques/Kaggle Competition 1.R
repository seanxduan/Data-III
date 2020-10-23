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
