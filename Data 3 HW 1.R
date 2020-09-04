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

seeds[210,]

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
Boston<-Boston
Boston_2<-Boston^2
Boston_3<-Boston^3

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
my_lms <- lapply(2:n, function(x) lm(Boston$crim ~ Boston[,x] + Boston_2[,x] + Boston_3[,x] ))
summaries <- lapply(my_lms, summary)
print(summaries)

testreg<-lm(crim ~ zn + I(zn^2) + I(zn^3), data = Boston)
summary(testreg)
?I
#ok, this works, note that we can't have CHAS as a factor while doing this b/c it breaks the code, and that we cant
#have our my lms set using I b/c it is not defined b/c of singularities