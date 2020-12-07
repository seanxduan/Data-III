#Data 3 HW 7

#reading in our greyscale images from wikle
library(ISLR)
library(readr)
shoes <- read_table('footwear.txt', col_names=F)
shoes <- t(shoes)
rotate <- function(x) t(apply(x,2,rev))
image(rotate(matrix(shoes[1,], nrow=sqrt(ncol(shoes)))))
class(shoes)
#for the first shoe above?

image(rotate(matrix(shoes[17300,], nrow=sqrt(ncol(shoes)))))

## 1
#average across all the rows for all the columns, then you have a vector of 784 elements, then put it in a 28 by 28 matrix
s1<-colMeans(shoes)
s1<-as.matrix(s1)
s1<-t(s1)
class(s1)
image(rotate(matrix(s1[1,], nrow=sqrt(ncol(s1)))))

#do it for sdev as well
s2<-apply(shoes, 2, sd)
s2<-as.matrix(s2)
s2<-t(s2)
class(s2)
image(rotate(matrix(s2[1,], nrow=sqrt(ncol(s2)))))


## 2
#how to do PCA?
#subtract mean value
shoes_1<-sweep(shoes, 2, s1)

#doing PCA here w/ scaling set to true
pca_shoes=prcomp(shoes_1)
#A
#the variance explained by each PC is explained by squaring these
pr.var<-((pca_shoes$sdev)^2)
pve<-pr.var/sum(pr.var)
head(pve)
#32%, 13%, 7%, 3%, and 2.5%


#B
biplot(pca_shoes)
#try it without scale = 0?
#lets try to do it imagewise like the two previous things
View(pca_shoes)
names(pca_shoes)
View(pca_shoes$rotation)
pc_loadings<-pca_shoes$rotation
pc_loadings<-as.data.frame(pc_loadings)
pc1<-pc_loadings$PC1
pc1<-as.matrix(pc1)
pc1<-t(pc1)
class(pc1)
image(rotate(matrix(pc1[1,], nrow=sqrt(ncol(pc1)))))

pc2<-pc_loadings$PC2
pc2<-as.matrix(pc2)
pc2<-t(pc2)
class(pc2)
image(rotate(matrix(pc2[1,], nrow=sqrt(ncol(pc2)))))

#C
#figure out how to scramble the x columns effectively?
#randomly move
shoes_2<-as.data.frame(shoes_1)
class(shoes_2)
z<-as.data.frame(lapply(shoes_2, sample))
#this works and samples w/o replacement <3
#trying to generate our PCA on our randomized col dataset and calc the PCA effect
pca_shoes_random=prcomp(z)

pr.var_r<-((pca_shoes_random$sdev)^2)
pve_r<-pr.var_r/sum(pr.var_r)
head(pve_r)
library(ggplot2)
pca_plot_dat<-cbind(pve,pve_r)
pca_plot_dat<-as.data.frame(pca_plot_dat)
pca_plot_dat$index<-seq(1:nrow(pca_plot_dat))
library(tidyr)
pca_plot_dat_wide<-gather(pca_plot_dat, condition, measurement, pve:pve_r, factor_key = TRUE)

pca_plot<-ggplot(pca_plot_dat_wide, aes(x=index, y=measurement, group = condition, color = condition))+geom_point()+geom_line()
pca_plot


## 3
kmean_loading<-pc_loadings[,1:4]
km.out=kmeans(kmean_loading , 3, nstart =20)
km.out$cluster
plot(kmean_loading, col=(km.out$cluster +1), main="K-Means Clustering Results with K=3",  pch=20, cex=2)

k2<-as.factor(km.out$cluster)
km_plot<-ggplot(kmean_loading, aes(x=PC1, y=PC2, color = k2))+geom_point()
km_plot
#w/ only the graph we care about

## 4
library(kernlab)
#do data preprocessing (split train/test, and feature scaling)
shoes_4<-scale(shoes)
shoes_4[is.na(shoes_4)] <- 0

#really reduce the # size so we have a smaller set!


min=sample(1:nrow(shoes_4), 1000)
shoes_5<-shoes_4[min,]

shoes_5<-as.data.frame(shoes_5)
train=sample(1:nrow(shoes_5), 500)
shoes_k = kpca(~., data = shoes_5[-train,], kernel = 'rbfdot', features = 4)

training_set_pca = as.data.frame(predict(shoes_k, shoes_5[train,]))
head(training_set_pca)
View(training_set_pca)

#scatter plots 
km.out=kmeans(training_set_pca , 3, nstart =20)
km.out$cluster
plot(kmean_loading, col=(km.out$cluster +1), main="K-Means Clustering Results with K=3",  pch=20, cex=2)

k2<-as.factor(km.out$cluster)
km_plot<-ggplot(training_set_pca, aes(x=V1, y=V2, color = k2))+geom_point()
km_plot

#repeat w/ other kernals?
shoes_k = kpca(~., data = shoes_5[-train,], kernel = 'laplacedot', features = 4)
training_set_pca = as.data.frame(predict(shoes_k, shoes_5[train,]))
#scatter plots 
km.out=kmeans(training_set_pca , 3, nstart =20)
km.out$cluster
plot(kmean_loading, col=(km.out$cluster +1), main="K-Means Clustering Results with K=3",  pch=20, cex=2)
k2<-as.factor(km.out$cluster)
km_plot<-ggplot(training_set_pca, aes(x=V1, y=V2, color = k2))+geom_point()
km_plot
#doesn't seem to work w/ other kernals (tried polydot, vanilladot, splinedot)

#repeat w/ other kernals? NOPE
shoes_k = kpca(~., data = shoes_5[-train,], kernel = 'polydot', features = 4)
training_set_pca = as.data.frame(predict(shoes_k, shoes_5[train,]))
#scatter plots 
km.out=kmeans(training_set_pca , 3, nstart =20)
km.out$cluster
plot(kmean_loading, col=(km.out$cluster +1), main="K-Means Clustering Results with K=3",  pch=20, cex=2)
k2<-as.factor(km.out$cluster)
km_plot<-ggplot(training_set_pca, aes(x=V1, y=V2, color = k2))+geom_point()
km_plot

#5.
library(Biobase)
library(NMF)
#performing a 'single run' of the NMF algo
res <- nmf(shoes[train,], 3, method = 'lee')
#note need to specify method in our argument?
??nmf

#fitted(res) pulls out or fitted matrice, the command below autoplots our first 6 images
V.hat <- fitted(res)
class(V.hat)
for (i in 1:6) {
  image(rotate(matrix(V.hat[i,], nrow=sqrt(ncol(V.hat)))))  
}

#6.
library(lle)

k40 <- lle(X=shoes_5, m=2, k=5, reg=2, ss=FALSE, id=TRUE, v=0.9 )
View(k40)
names(k40)
k40$Y

lle_pca<-k40$Y
lle_pca<-as.data.frame(lle_pca)
#scatter plots 
km.out=kmeans(lle_pca , 3, nstart =20)
km.out$cluster
k2<-as.factor(km.out$cluster)
km_plot<-ggplot(lle_pca, aes(x=V1, y=V2, color = k2))+geom_point()
km_plot
