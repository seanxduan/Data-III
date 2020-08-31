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

