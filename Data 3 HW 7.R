#Data 3 HW 7

#reading in our greyscale images from wikle
library(ISLR)
library(readr)
shoes <- read_table('footwear.txt', col_names=F)
shoes <- t(shoes)
rotate <- function(x) t(apply(x,2,rev))
image(rotate(matrix(shoes[1,], nrow=sqrt(ncol(shoes)))))

#for the first shoe above?

image(rotate(matrix(shoes[17300,], nrow=sqrt(ncol(shoes)))))

## 1


## 2
#how to do PCA?

#doing PCA here w/ scaling set to true
pca_shoes=prcomp(shoes)
#A
#the variance explained by each PC is explained by squaring these
pr.var<-((pca_shoes$sdev)^2)
pve<-pr.var/sum(pr.var)
head(pve)
#32%, 13%, 7%, 3%, and 2.5%

#B
biplot(pca_shoes)
#try it without scale = 0?

#C
#figure out how to scramble the x columns effectively?

## 3

## 4