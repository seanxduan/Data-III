#Data 3 - HW 1#

getwd()

#3.
#a.
seeds<-read.csv("seeds.csv")
seeds$Type<-as.factor(seeds$Type)

library(plyr)

levels(seeds$Type)
seeds$Type<-revalue(seeds$Type, c("1" = "Kama", "2" = "Rosa", "3" = "Canadian"))

#b
summary(seeds)

#c.
count(seeds$perimeter > 15)
71/210

#d
which.max(seeds$asymmetry.coefficient)

seeds[204,]

#e
pairs(seeds)

#f
library(ggplot2)
p3_f1<-ggplot(data =seeds, aes(x=Area, y=Type, fill=Type ))
p3_f1 + geom_boxplot() + geom_jitter(color = "black", size = 0.4, alpha = 0.9)+
  ggtitle("Boxplot of Area against Type") + xlab("Area") + ylab("Type")

p3_f2<-ggplot(data =seeds, aes(x=perimeter, y=Type, fill=Type ))
p3_f2 + geom_boxplot() + geom_jitter(color = "black", size = 0.4, alpha = 0.9)+
  ggtitle("Boxplot of Perimeter against Type") + xlab("Perimeter") + ylab("Type")

p3_f3<-ggplot(data =seeds, aes(x=length.of.kernel.groove, y=Type, fill=Type ))
p3_f3 + geom_boxplot() + geom_jitter(color = "black", size = 0.4, alpha = 0.9)+
  ggtitle("Boxplot of Length of Kernel Groove Against Type") + xlab("Length of Kernel Groove") + ylab("Type")

#g
my_cols <- c("#00AFBB", "#E7B800", "#FC4E07")  
pairs(seeds, col = my_cols[seeds$Type])

#4

#5
#A
#each single regression followed by code for plots
library(MASS)
?Boston
Bostons<-Boston
Boston_2<-Bostons^2
Boston_3<-Bostons^3

Boston$chas <- factor(Boston$chas)

b_m1<-lm(crim~zn, data = Boston)
summary(b_m1)
b_m1p<-ggplot(Boston, aes(x = zn, y = crim)) + 
  geom_point() + 
  geom_abline(intercept = coef(b_m1)[1], slope = coef(b_m1)[2], 
              col = "deepskyblue3", 
              size = 1) + 
  geom_smooth(se = F)
b_m1p

b_m2<-lm(crim~indus, data = Boston)
summary(b_m2)
b_m2p<-ggplot(Boston, aes(x = indus, y = crim)) + 
  geom_point() + 
  geom_abline(intercept = coef(b_m2)[1], slope = coef(b_m2)[2], 
              col = "deepskyblue3", 
              size = 1) + 
  geom_smooth(se = F)
b_m2p

b_m3<-lm(crim~chas, data = Boston)
summary(b_m3)
b_m3p<-ggplot(Boston, aes(x = chas, y = crim)) + 
  geom_point() + 
  geom_abline(intercept = coef(b_m3)[1], slope = coef(b_m3)[2], 
              col = "deepskyblue3", 
              size = 1) + 
  geom_smooth(se = F)
b_m3p

b_m4<-lm(crim~nox, data = Boston)
summary(b_m4)
b_m4p<-ggplot(Boston, aes(x = nox, y = crim)) + 
  geom_point() + 
  geom_abline(intercept = coef(b_m4)[1], slope = coef(b_m4)[2], 
              col = "deepskyblue3", 
              size = 1) + 
  geom_smooth(se = F)
b_m4p

b_m5<-lm(crim~rm, data = Boston)
summary(b_m5)
b_m5p<-ggplot(Boston, aes(x = rm, y = crim)) + 
  geom_point() + 
  geom_abline(intercept = coef(b_m5)[1], slope = coef(b_m5)[2], 
              col = "deepskyblue3", 
              size = 1) + 
  geom_smooth(se = F)
b_m5p

b_m6<-lm(crim~age, data = Boston)
summary(b_m6)
b_m6p<-ggplot(Boston, aes(x = age, y = crim)) + 
  geom_point() + 
  geom_abline(intercept = coef(b_m6)[1], slope = coef(b_m6)[2], 
              col = "deepskyblue3", 
              size = 1) + 
  geom_smooth(se = F)
b_m6p

b_m7<-lm(crim~dis, data = Boston)
summary(b_m7)
b_m7p<-ggplot(Boston, aes(x = dis, y = crim)) + 
  geom_point() + 
  geom_abline(intercept = coef(b_m7)[1], slope = coef(b_m7)[2], 
              col = "deepskyblue3", 
              size = 1) + 
  geom_smooth(se = F)
b_m7p

b_m8<-lm(crim~rad, data = Boston)
summary(b_m8)
b_m8p<-ggplot(Boston, aes(x = rad, y = crim)) + 
  geom_point() + 
  geom_abline(intercept = coef(b_m8)[1], slope = coef(b_m8)[2], 
              col = "deepskyblue3", 
              size = 1) + 
  geom_smooth(se = F)
b_m8p

b_m9<-lm(crim~tax, data = Boston)
summary(b_m9)
b_m9p<-ggplot(Boston, aes(x = tax, y = crim)) + 
  geom_point() + 
  geom_abline(intercept = coef(b_m9)[1], slope = coef(b_m9)[2], 
              col = "deepskyblue3", 
              size = 1) + 
  geom_smooth(se = F)
b_m9p

b_m10<-lm(crim~ptratio, data = Boston)
summary(b_m10)
b_m10p<-ggplot(Boston, aes(x = ptratio, y = crim)) + 
  geom_point() + 
  geom_abline(intercept = coef(b_m10)[1], slope = coef(b_m10)[2], 
              col = "deepskyblue3", 
              size = 1) + 
  geom_smooth(se = F)
b_m10p

b_m11<-lm(crim~black, data = Boston)
summary(b_m11)
b_m11p<-ggplot(Boston, aes(x = black, y = crim)) + 
  geom_point() + 
  geom_abline(intercept = coef(b_m11)[1], slope = coef(b_m11)[2], 
              col = "deepskyblue3", 
              size = 1) + 
  geom_smooth(se = F)
b_m11p

b_m12<-lm(crim~lstat, data = Boston)
summary(b_m12)
b_m12p<-ggplot(Boston, aes(x = lstat, y = crim)) + 
  geom_point() + 
  geom_abline(intercept = coef(b_m12)[1], slope = coef(b_m12)[2], 
              col = "deepskyblue3", 
              size = 1) + 
  geom_smooth(se = F)
b_m12p

b_m13<-lm(crim~medv, data = Boston)
summary(b_m13)
b_m13p<-ggplot(Boston, aes(x = medv, y = crim)) + 
  geom_point() + 
  geom_abline(intercept = coef(b_m13)[1], slope = coef(b_m13)[2], 
              col = "deepskyblue3", 
              size = 1) + 
  geom_smooth(se = F)
b_m13p

#B
b_m14<-lm(crim~., data = Boston)
summary(b_m14)

#C
single_reg<-c(coef(b_m1)[[2]], coef(b_m2)[[2]],coef(b_m3)[[2]],coef(b_m4)[[2]],coef(b_m5)[[2]],coef(b_m6)[[2]],coef(b_m7)[[2]],
              coef(b_m8)[[2]],coef(b_m9)[[2]],coef(b_m10)[[2]],coef(b_m11)[[2]],coef(b_m12)[[2]],coef(b_m13)[[2]])
single_reg<-as.data.frame(single_reg)

multiple_reg<-b_m14[[1]]
multiple_reg<-as.data.frame(multiple_reg)
multiple_reg<-multiple_reg[-1,]
multiple_reg<-as.data.frame(multiple_reg)

names<-names((b_m14$coefficients))
names<-as.data.frame(names)
names<-names[-1,]
names<-as.data.frame(names)

regression_plot<-cbind(single_reg, multiple_reg,names)


ggplot(regression_plot, aes(x = single_reg, y = multiple_reg, col = names)) + 
  geom_jitter() + 
  labs(title = "Simple vs Multiple Regression Coefficients", 
       subtitle = "For linear models predicting 'crim'", 
       x = "Simple Coefficient", 
       y = "Multiple Coefficient")  

#D
library(tidyverse)
P_value_x <- c()
P_value_x2 <- c()
P_value_x3 <- c()
R2 <- c()
y <- Boston$crim

for (i in 1:ncol(Boston)) {
  x <- Boston[ ,i]
  if (is.numeric(x)) { 
    model <- lm(y ~ x + I(x^2) + I(x^3))
    if (!identical(y, x)) {
      P_value_x[i] <- summary(model)$coefficients[2, 4]
      P_value_x2[i] <- summary(model)$coefficients[3, 4]
      P_value_x3[i] <- summary(model)$coefficients[4, 4]
      R2[i] <- summary(model)$r.squared 
    }
  } else {
    P_value_x[i] <- NA
    P_value_x2[i] <- NA
    P_value_x3[i] <- NA
    R2[i] <- NA
  }
}

data.frame(varname = names(Boston),
           R2 = round(R2, 5),
           P_value_x = round(P_value_x, 10),
           P_value_x2 = round(P_value_x2, 10), 
           P_value_x3 = round(P_value_x3, 10)) %>%
  filter(!varname %in% c("crim", "chas")) %>%
  arrange(desc(R2)) %>%
  mutate(relationship = case_when(P_value_x3 < 0.05 ~ "Cubic", 
                                  P_value_x2 < 0.05 ~ "Quadratic", 
                                  P_value_x < 0.05 ~ "Linear", 
                                  TRUE ~ "No Relationship"))

#test code for my own try

Boston<-Boston

n<-14
my_lms <- lapply(2:n, function(x) lm(Bostons$crim ~ Bostons[,x] + Boston_2[,x] + Boston_3[,x] ))
#makes a function x, the function is defined afterward. 
summaries <- lapply(my_lms, summary)
print(summaries)

testreg<-lm(crim ~ zn + I(zn^2) + I(zn^3), data = Boston)
summary(testreg)
?I
#ok, this works, note that we can't have CHAS as a factor while doing this b/c it breaks the code, and that we cant
#have our my lms set using I b/c it is not defined b/c of singularities

testlist<-list(NA)
for (i in 2:ncol(Boston)){
  testlist[[i-1]]<-summary(lm(Boston$crim ~ Boston[,i] + Boston_2[,i] + Boston_3[,i] ))
}
print(testlist)

#7
#a
library(plyr)
tumor<-read.csv("tumor.csv")
tumor$Diagnosis<-as.factor(tumor$Diagnosis)
pairs(tumor)

#radius, perimeter, and concave points seem most related to outcome
#features highly related, radius and perimter, radius and area, area highly so
#less so but still strongly perimter,concavity/concave points, and compactness/concavity

#b
set.seed(1)
test=1:512
train_tumor<-tumor[test,]
test_tumor<-tumor[-test,]
nrow(test_tumor)
#c
tumor_logistic<-glm(Diagnosis~Radius+Symmetry+Concave.Points, data = train_tumor, family = binomial)
summary(tumor_logistic)
tumor_logistic_probs<-predict(tumor_logistic, test_tumor,type = "response")
?predict
tumor_logistic_probs
tumor_logistic_pred<-rep("Benign", 57)
tumor_logistic_pred[tumor_logistic_probs >.5]= "Malignant"
table(tumor_logistic_pred, test_tumor$Diagnosis)

mean(tumor_logistic_pred==test_tumor$Diagnosis)
1-mean(tumor_logistic_pred==test_tumor$Diagnosis)
#misclassification rate is 3ish%

tumor_logistic_pred2<-rep("Benign", 57)
tumor_logistic_pred2[tumor_logistic_probs >.25]= "Malignant"
table(tumor_logistic_pred2, test_tumor$Diagnosis)

mean(tumor_logistic_pred2==test_tumor$Diagnosis)
1-mean(tumor_logistic_pred2==test_tumor$Diagnosis)
#none of the errors were it said bening and actually malignant
#thus rate false negative very small, 
#some it called beningn were malignant
#misclass rate is 5ish%

#e
library(MASS)
tumor_lda<-lda(Diagnosis~Radius+Symmetry+Concave.Points, data = train_tumor)
tumor_lda
tumor_lda_pred<-predict(tumor_lda, test_tumor)
tumor_lda_pred
tumor_lda_class<-tumor_lda_pred$class

table(tumor_lda_class, test_tumor$Diagnosis)
1-mean(tumor_lda_class==test_tumor$Diagnosis)
#5%ish error

#f QDA
tumor_qda<-qda(Diagnosis~Radius+Symmetry+Concave.Points, data = train_tumor)

tumor_qda_class<-predict(tumor_qda, test_tumor)$class

table(tumor_qda_class, test_tumor$Diagnosis)
1-mean(tumor_qda_class==test_tumor$Diagnosis)
#3ish% error rate

library(class)
train_tumor_x<-cbind(train_tumor$Radius,train_tumor$Symmetry,train_tumor$Concave.Points)
test_tumor_x<-cbind(test_tumor$Radius,test_tumor$Symmetry,test_tumor$Concave.Points)
train_tumor_direction<-train_tumor$Diagnosis

tumor_knn_1<-knn(train_tumor_x,test_tumor_x,train_tumor_direction ,k=1)
table(tumor_knn_1 , test_tumor$Diagnosis)
1-mean(tumor_knn_1==test_tumor$Diagnosis)

tumor_knn_2<-knn(train_tumor_x,test_tumor_x,train_tumor_direction ,k=2)
table(tumor_knn_2 , test_tumor$Diagnosis)
1-mean(tumor_knn_2==test_tumor$Diagnosis)

tumor_knn_3<-knn(train_tumor_x,test_tumor_x,train_tumor_direction ,k=3)
table(tumor_knn_3 , test_tumor$Diagnosis)
1-mean(tumor_knn_3==test_tumor$Diagnosis)

tumor_knn_4<-knn(train_tumor_x,test_tumor_x,train_tumor_direction ,k=4)
table(tumor_knn_4 , test_tumor$Diagnosis)
1-mean(tumor_knn_4==test_tumor$Diagnosis)

#8
data(Boston)
median(Boston$crim)
#add this as a vector? where either true or false
Boston$direction<-rep("Up", 506)
Boston$direction[Boston$crim <median(Boston$crim)]="Down"
Boston$direction<-as.factor(Boston$direction)
sum(Boston$direction == "Down")
pairs(Boston[,-15])
levels(Boston$direction)
?Boston
#looking at the graphs, rad, ptratio, nox, and dis look most strongly related
#we can look at the full set
#and then the set w/ the fewest models that makes sense? (4 above variables)
#gotta split data into test and training, 90% training 10% test seems good
test2<-(1:455)
Boston_train<-Boston[test2,]
Boston_test<-Boston[-test2,]
#a
#note that random guessing gives you a 50% shot either way, so can we do better?
Boston_logistic_1<-glm(direction~.-crim, data = Boston_train, family = binomial)
summary(Boston_logistic_1)
Boston_logistic_probs_1<-predict(Boston_logistic_1, Boston_test,type = "response")
Boston_logistic_pred_1<-rep("Down", 51)
Boston_logistic_pred_1[Boston_logistic_probs_1 >.5]= "Up"
table(Boston_logistic_pred_1, Boston_test$direction)
1-mean(Boston_logistic_pred_1==Boston_test$direction)

Boston_logistic_2<-glm(direction~ rad + nox + ptratio + dis, data = Boston_train, family = binomial)
summary(Boston_logistic_2)
Boston_logistic_probs_2<-predict(Boston_logistic_2, Boston_test,type = "response")
Boston_logistic_pred_2<-rep("Down", 51)
Boston_logistic_pred_2[Boston_logistic_probs_2 >.5]= "Up"
table(Boston_logistic_pred_2, Boston_test$direction)
1-mean(Boston_logistic_pred_2==Boston_test$direction)

#b
#e
Boston_lda_1<-lda(direction~.-crim, data = Boston_train, family = binomial)
Boston_lda_1
Boston_lda_pred_1<-predict(Boston_lda_1, Boston_test)
Boston_lda_class_1<-Boston_lda_pred_1$class

table(Boston_lda_class_1, Boston_test$direction)
1-mean(Boston_lda_class_1==Boston_test$direction)


Boston_lda_2<-lda(direction~ rad + nox + ptratio + dis, data = Boston_train, family = binomial)
Boston_lda_2
Boston_lda_pred_2<-predict(Boston_lda_2, Boston_test)
Boston_lda_class_2<-Boston_lda_pred_2$class

table(Boston_lda_class_2, Boston_test$direction)
1-mean(Boston_lda_class_2==Boston_test$direction)

#c
library(class)
Boston_train_x_1<-Boston_train[,c(-1,-15)]
Boston_test_x_1<-Boston_test[,c(-1,-15)]
Boston_train_direction<-Boston_train$direction

Boston_knn_1_A<-knn(Boston_train_x_1,Boston_test_x_1,Boston_train_direction,k=1)
table(Boston_knn_1_A , Boston_test$direction)
1-mean(Boston_knn_1_A==Boston_test$direction)

Boston_knn_1_B<-knn(Boston_train_x_1,Boston_test_x_1,Boston_train_direction,k=2)
table(Boston_knn_1_B , Boston_test$direction)
1-mean(Boston_knn_1_B==Boston_test$direction)

Boston_knn_1_C<-knn(Boston_train_x_1,Boston_test_x_1,Boston_train_direction,k=4)
table(Boston_knn_1_C , Boston_test$direction)
1-mean(Boston_knn_1_C==Boston_test$direction)

Boston_train_x_2<-cbind(Boston_train$rad, Boston_train$nox, Boston_train$ptratio, Boston_train$dis)
Boston_test_x_2<-cbind(Boston_test$rad, Boston_test$nox, Boston_test$ptratio, Boston_test$dis)

Boston_knn_2_A<-knn(Boston_train_x_2,Boston_test_x_2,Boston_train_direction,k=1)
table(Boston_knn_2_A , Boston_test$direction)
1-mean(Boston_knn_2_A==Boston_test$direction)

Boston_knn_2_B<-knn(Boston_train_x_2,Boston_test_x_2,Boston_train_direction,k=2)
table(Boston_knn_2_B , Boston_test$direction)
1-mean(Boston_knn_2_B==Boston_test$direction)

Boston_knn_2_C<-knn(Boston_train_x_2,Boston_test_x_2,Boston_train_direction,k=10)
table(Boston_knn_2_C , Boston_test$direction)
1-mean(Boston_knn_2_C==Boston_test$direction)
