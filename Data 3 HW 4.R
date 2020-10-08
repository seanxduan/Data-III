#Data 3 HW 4
#1


#2

library(ISLR)
library(MASS)
library(ggplot2)
data(Boston)
View(Boston)
#A
#polynomail reg
p2_m1<-lm(nox~poly(dis ,3) ,data=Boston)
#coef(summary (fit))
summary(p2_m1)

#code to plot, first make predictions from our range of data
dis_lims =range(Boston$dis)
dis_grid=seq(from=dis_lims [1],to=dis_lims [2])
dis_preds=predict (p2_m1 ,newdata =list(dis=dis_grid),se=TRUE)
dis_se.bands=cbind(dis_preds$fit +2* dis_preds$se.fit ,dis_preds$fit -2* dis_preds$se.fit)
#actual plotting code
plot(Boston$dis ,Boston$nox, xlim=dis_lims ,cex =.5,col=" darkgrey ")
title(" Degree -3 Polynomial ",outer=T)
lines(dis_grid ,dis_preds$fit ,lwd=2,col="blue")
matlines (dis_grid ,dis_se.bands ,lwd=1, col=" blue",lty=3)

#B
#make a loop to fit all the polys
testlist<-list(NA)
for(i in 1:10){
  testlist[[i]]<-lm(nox~poly(dis ,i) ,data=Boston)
}

#print the deviance (same as RSS for linear models) on all polynomial fits
for(i in 1:10){
  print(deviance(testlist[[i]]))
}

#C
#setting up for CV
train=sample (506,253)
test=!train
#fitting the loop
testlist<-list(NA)
for(i in 1:10){
  testlist[[i]]<-lm(nox~poly(dis ,i) ,data=Boston, subset=train)
}

#test w/ p2_m1
for(i in 1:10){
print(mean((Boston$nox-predict(testlist[[i]],Boston))[-train ]^2))
}
#cross validation w/ test /training set chooses 3rd polynomial

#D
library(splines)
#those are my knot values 
#make a loop to fit all the splines w/ different df

testlist<-list(NA)
for(i in 3:6){
  testlist[[i]]<-lm(nox~bs(dis ,df = i) ,data=Boston, subset=train)
}

for(i in 3:6){
  print(mean((Boston$nox-predict(testlist[[i]],Boston))[-train ]^2))
}
#looks like our 3df model is best
#RSS is 0.003891
#code to plot, first make predictions from our range of data
dis_lims =range(Boston$dis)
dis_grid=seq(from=dis_lims [1],to=dis_lims [2])
dis_preds=predict (testlist[[3]] ,newdata =list(dis=dis_grid),se=TRUE)
dis_se.bands=cbind(dis_preds$fit +2* dis_preds$se.fit ,dis_preds$fit -2* dis_preds$se.fit)
#actual plotting code
plot(Boston$dis ,Boston$nox, xlim=dis_lims ,cex =.5,col=" darkgrey ")
title(" 3 degree freedom  ",outer=T)
lines(dis_grid ,dis_preds$fit ,lwd=2,col="blue")
matlines (dis_grid ,dis_se.bands ,lwd=1, col=" blue",lty=3)

#E
testlist<-list(NA)
for(i in 3:6){
  testlist[[i]]<-lm(nox~ns(dis ,df = i) ,data=Boston, subset=train)
}

for(i in 3:6){
  print(mean((Boston$nox-predict(testlist[[i]],Boston))[-train ]^2))
}
#4th df model best
#rss = 0.003822
#code to plot, first make predictions from our range of data
dis_lims =range(Boston$dis)
dis_grid=seq(from=dis_lims [1],to=dis_lims [2])
dis_preds=predict (testlist[[4]] ,newdata =list(dis=dis_grid),se=TRUE)
dis_se.bands=cbind(dis_preds$fit +2* dis_preds$se.fit ,dis_preds$fit -2* dis_preds$se.fit)
#actual plotting code
plot(Boston$dis ,Boston$nox, xlim=dis_lims ,cex =.5,col=" darkgrey ")
title(" 4 degree freedom  ",outer=T)
lines(dis_grid ,dis_preds$fit ,lwd=2,col="blue")
matlines (dis_grid ,dis_se.bands ,lwd=1, col=" blue",lty=3)

#F
#fitting a smoothing spline using the smooth.spline fxn
plot(x=Boston$dis ,y=Boston$nox ,xlim=dis_lims ,cex =.5,col="darkgrey")
title("Smoothing Spline ")
fit=smooth.spline(Boston$dis ,Boston$nox ,df=10)
fit2=smooth.spline(Boston$dis ,Boston$nox ,cv=TRUE)
fit2$df
lines(fit ,col="red",lwd =2)
lines(fit2 ,col="blue",lwd=2)
legend ("topright",legend=c("10 DF" ,"15.42 DF"),col=c("red","blue"),lty=1,lwd=2, cex =.8)
#we can specify DF in smooth.spline, but we can also let CV auto determine it w/ CV = TRUE command
length(fit$x)
fit2$lambda
#what does this weighting mean??
fit2$pen.crit
sum((fit2$y - Boston$nox[1:412])^2)
#??? lets loook @ this later
#toy example from stackexchange
x = seq(1:18)
y = c(1:3,5,4,7:3,2*(2:5),rep(10,4))
splineres <- function(spar){
  res <- rep(0, length(x))
  for (i in 1:length(x)){
    mod <- smooth.spline(x[-i], y[-i], spar = spar)
    res[i] <- predict(mod, x[i])$y - y[i]
  }
  return(sum(res^2))
}

spars <- seq(0, 1.5, by = 0.001)
ss <- rep(0, length(spars))
for (i in 1:length(spars)){
  ss[i] <- splineres(spars[i])
}
plot(spars, ss, 'l', xlab = 'spar', ylab = 'Cross Validation Residual Sum of Squares' , main = 'CV RSS vs Spar')
spars[which.min(ss)]
R > spars[which.min(ss)]
[1] 0.381
## this code fits spar directly using spar fxn and cross validation?

#G loess code
#performing local regression w/ Loess fxn
## code
rangespan<-seq(from=0.1, to=5, length.out = 20)
check<-list(NA)
testlist<-list(NA)
#clde to c lone
for(i in 1:length(rangespan)){
  testlist[[i]]<-loess(nox ~ dis, span = rangespan[[i]], data = Boston, subset = train)
  check[[i]]<-testlist[[i]]$s
  }

testlist[[1]]

which.min(check)
#lower values of RSE indicate a better fit.

##3
load("lakes_DA3.Rdata")
lakes<-lakes_DA3
lakes$lsecchi<-log(lakes$secchi)
lakes2<-lakes[,-c(4,25)]

for(i in 1:23){
  plot(y=lakes$lsecchi, x=lakes2[,i], xlab=colnames(lakes2)[i])
}
  
#chla, mean depth,tn,tp, mean annual temp,spring temp,summer temp,
#fall temp, fall precip, iws_wetland
#it looks like w/ these plots we only want to look @ these variables

#univariate filter w/ our goal of interest
vars <- c("chla","mean_depth", "tn", "tp", "mean_annual_temp", "mean_spring_temp", "mean_summer_temp",
          "mean_fall_temp", "mean_fall_precip", "iws_wetland")
comb.vars <- expand.grid(vars, vars, stringsAsFactors = FALSE)
comb.vars <- comb.vars[comb.vars[,1] != comb.vars[,2],]
i.vars <- apply(comb.vars, 1, paste, collapse = "+")
View(i.vars)
#code to make 2 items combos of our 10 variables
library(boot)
cv_error<-list(NA)
modelfits<-list(NA)
for(i in 1:length(i.vars)) {
  modelformula <- paste("lsecchi ~", i.vars[i])
  modelfits[[i]] <- glm(as.formula(modelformula), family = "gaussian", data = lakes)
  cv_error[[i]]=cv.glm(lakes,modelfits[[i]] ,K=5)$delta[1]
}
cv_error
which.min(cv_error)
#.2861
best_model<-modelfits[[8]]
summary(best_model)


#now try to run the above loop using SPLINES???

#lets fit bs splines
library(splines)
#CREATE two vectors of unique combos
var_list<-comb.vars[!duplicated(t(apply(comb.vars, 1, sort))), ]

#jesus this is some ugly code
i<-1
cv_error<-list(NA)
modelfits<-list(NA)

for(i in 1:nrow(var_list)) {
  modelformula<-paste("lsecchi~","bs(",var_list[[i,1]],",df=3)+bs(",var_list[[i,2]],",df=3)")
  modelfits[[i]]<-glm(as.formula(modelformula), family = "gaussian",data=lakes)
  cv_error[[i]]<-cv.glm(lakes, modelfits[[i]], K=5)$delta[1]
}
cv_error
which.min(cv_error)

#.179335
best_model<-modelfits[[18]]
summary(best_model)

#fit using ns splines
#jesus this is some ugly code
i<-1
cv_error<-list(NA)
modelfits<-list(NA)

for(i in 1:nrow(var_list)) {
  modelformula<-paste("lsecchi~","ns(",var_list[[i,1]],",df=3)+ns(",var_list[[i,2]],",df=3)")
  modelfits[[i]]<-glm(as.formula(modelformula), family = "gaussian",data=lakes)
  cv_error[[i]]<-cv.glm(lakes, modelfits[[i]], K=5)$delta[1]
}
cv_error
which.min(cv_error)
#.1359
best_model<-modelfits[[18]]
summary(best_model)

#test w/ wikle
i<-1
cv_error<-list(NA)
modelfits<-list(NA)

for(i in 1:nrow(var_list)) {
  modelfits[[i]]<-glm(lsecchi~ ns(var_list[i,1], df = 3) + ns(var_list[i,2], df=3), family = "gaussian",data=lakes)
  cv_error[[i]]<-cv.glm(lakes, modelfits[[i]], K=5)$delta[1]
}
# doesn't work, can't directly call the item off var_list?

#time to go wild with gam? using the gamclass fxn as a stand in for cross validation
library(gam)
errorlist<-list(NA)
bestmodel<-list(NA)
i=1
#code for our own k-fold cv
K=5
folds = sample(1:K,nrow(lakes),replace=T)
modelfits<-list(NA)
errorlist<-list(NA)
bestmodel<-list(NA)
moderror<-list(NA)
for(k in 1:K){
  CV.train = lakes[folds != k,]
  CV.test = lakes[folds == k,]
  CV.ts_y = CV.test$lsecchi
  for(i in 1:nrow(var_list)) {
    modelformula<-paste("lsecchi~","s(",var_list[[i,1]],",4)+s(",var_list[[i,2]],",4)")
    modelfits[[i]]<-gam(as.formula(modelformula), data=CV.train)
    errorlist[[i]]<-sum((CV.ts_y-predict(modelfits[[i]], newdata=CV.test))^2)
      }
  moderror[[k]]<-errorlist[[which.min(errorlist)]]/nrow(CV.test)
  bestmodel[[k]]<-which.min(errorlist)
}
bestmodel
moderror
#is this code even ... good? YES~!
#0.159,0.1357,0.1667
#2 of our methods agreed that our best model was the 2 var model 2 tn and CHLA, compare our smooth spline vs our bs

#our best model was fitting using natural splines, model 18 mean fall temp and depth

levee<-read.delim("mmr_levee.txt", sep = "", header = FALSE)
colnames(levee)<-c("failure","year","river_mile","sediment","borrow_pit","meander_loc","channel_width","flood_width","constriction_fact","land_type","vege_buffer","channel_sinu","dredge_intense","bank_revetment")
levee$failure<-as.factor(levee$failure)
levee$meander_loc<-as.factor(levee$meander_loc)
levee$land_type<-as.factor(levee$land_type)
levee$borrow_pit<-as.factor(levee$borrow_pit)

for(i in 2:14){
  plot(y=levee$failure, x=levee[,i], xlab=colnames(levee)[i])
}
#checking overlaid plots
plot(y=levee$failure, x=jitter(levee$bank_revetment))

levels(levee$failure)
#relvaent lvls?
#land type matters, meander location
#looking at the plot only these two things matter
plot(y=levee$land_type, x=levee$meander_loc)
#perhaps slight correlation b/w these factors but not serious
train=sample(70,35)
levee_test<-levee[-train,]
#consider collapsing categories (collapse 1-3 and 2-4 in meander
#collapse 1 vs 2-3-4 in land_type)

#fit using glm
library(boot)
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)
modelfits<- glm(failure ~ meander_loc + land_type, family = "binomial", data = levee, subset=train)
glm.probs=predict(modelfits,newdata=levee_test,type="response")
glm.pred=rep(0 ,35)
glm.pred[glm.probs >.5]=1

table(glm.pred,levee_test$failure)
mean(glm.pred==levee_test$failure)
summary(modelfits)
library(lme4)
BIC(modelfits)
#fit using splines
library(splines)
modelfits<- glm(failure ~ bs(meander_loc, df=3) + bs(land_type, df=3), family = "binomial", data = levee)
cv_error=cv.glm(levee,modelfits,K=5, cost=cost)$delta[1]
cv_error
# unsure if this is valid

#prep for LDA
library(MASS)

lda.fit<-lda(failure~meander_loc + land_type, data = levee, subset = train)

lda.pred=predict(lda.fit , levee_test, type = "response")
lda.class=lda.pred$class
table(lda.class ,levee_test$failure)
mean(lda.class==levee_test$failure)

#for QDA
#cant fit w/ anything other than just meander_loc
qda.fit<-qda(failure~meander_loc, data = levee, subset = train)
qda.pred=predict(qda.fit , levee_test, type = "response")
qda.class=qda.pred$class
table(qda.class ,levee_test$failure)
mean(qda.class==levee_test$failure)

#lets try fwd stepwise
library(leaps)
regfit.bwd=regsubsets(failure~.,data=levee, nvmax=13,method ="backward")
summary(regfit.bwd)
regfit.fwd=regsubsets(failure~.,data=levee, nvmax=13,method ="forward")
summary(regfit.fwd)

regfit.full=regsubsets(failure~.,data=levee, nvmax=13)
reg.summary<-summary(regfit.full)
reg.summary$rsq
reg.summary$adjr2
reg.summary$bic
#7 variable model looking at best subset reg
#just sediment has best BIC
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)
modelfits<- glm(failure ~ sediment, family = "binomial", data = levee, subset=train)
glm.probs=predict(modelfits,newdata=levee_test,type="response")
glm.pred=rep(0 ,35)
glm.pred[glm.probs >.5]=1
table(glm.pred,levee_test$failure)
mean(glm.pred==levee_test$failure)
#lda w/ sed
library(MASS)
lda.fit<-lda(failure~ sediment, data = levee, subset = train)
lda.pred=predict(lda.fit , levee_test, type = "response")
lda.class=lda.pred$class
table(lda.class ,levee_test$failure)
mean(lda.class==levee_test$failure)

#qda w/ sed
qda.fit<-qda(failure~sediment, data = levee, subset = train)
qda.pred=predict(qda.fit , levee_test, type = "response")
qda.class=qda.pred$class
table(qda.class ,levee_test$failure)
mean(qda.class==levee_test$failure)
