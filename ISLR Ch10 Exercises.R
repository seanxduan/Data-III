#ISLR ch10 exercises#
library(ISLR)

#10.4 lab 1 - Principle components analysis
data("USArrests")
apply(USArrests , 2, mean)
#we see that our col's have vastly different means
#Note that the apply() function allows us to apply a function—in this case,
#the mean() function—to each row or column of the data set. The second
#input here denotes whether we wish to compute the mean of the rows, 1,
#or the columns, 2. We see that there are on average three times as many
#rapes as murders, and more than eight times as many assaults as rapes.
#We can also examine the variances of the four variables using the apply()
#function

apply(USArrests , 2, var)
#also looks like our vars have a LOT of variance.

#so we're going to apply PCA but note, scaling is set to TRUE
#this is important b/c our assault variable has much higher variance

pr.out=prcomp(USArrests, scale = TRUE)

#these are the elements in prcomp that are useful quantities
names(pr.out)
#note that the center and scale components correspond to
#means and std deviations of the variables that were used for scaling
#prior to implementation of PCA
pr.out$center
pr.out$scale

#the rotation matrix gives the PCA loadings themselves
pr.out$rotation

#we can plot the first two pcs using the code below
biplot(pr.out, scale = 0)
#the scale 0 argument ensures that the arrows are scaled to represent the loadings
#other values for scale give slightly diff biplots

#we can also find stdev
pr.out$sdev
#the variance explained by each PC is explained by squaring these
(pr.out$sdev)^2

#proportion of var explained per item is this value divided by sum of values
((pr.out$sdev)^2)/sum((pr.out$sdev)^2)

##code fr plot
pr.var=pr.out$sdev ^2
pve=pr.var/sum(pr.var)

#can plot the PVE explained by each component as below
plot(pve , xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component ", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')


#10.5 lab 2 Clustering
#lets try simulating data where there are actually 2 clusters, just to see how kmeans works
set.seed(2)
x=matrix(rnorm (50*2), ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4
#we try kmeans here w/ k= 2
km.out=kmeans (x,2, nstart =20)

#what are the cluster assignments of our x's here?
km.out$cluster
#it perfectly separated into 2 groups!
#lets plot this result to see what happened
plot(x, col=(km.out$cluster +1), main="K-Means Clustering Results with K=2", xlab="", ylab="", pch=20, cex=2)

#but we don't always know that there's 2 clusters ahead of time... what happens if we set it up so k=3?
set.seed(4)
km.out=kmeans (x,3, nstart =20)
km.out

#we can also try kmeans w/ multiple initial cluster assignments, for testing and checking purposes
#remember, this is the 'random' initial shot when we do k-means, so any # greater than 1 does the job!

#we can check to see if nstart matters below!
set.seed(3)
km.out=kmeans (x,3, nstart =1)
km.out$tot.withinss

km.out=kmeans (x,3, nstart =20)
km.out$tot.withinss
#the #'s should be different... but tot.withinss is the total within cluster sum of sq
#this is what we're trying to minimize!
#we should set nstart to be at least 20, or 50, to make sure we dont get a bad random roll

#10.5.2 Hierarchical Clustering
#we'll use the data from the prev section to try out our hclustering methods

#note that we set our linkage of choice in the fxn
#and that the dist fxn is used to compute the 50x50 inter-obs euclidian distance matrix
hc.complete =hclust(dist(x), method="complete")
#now that we have complete linkage, lets try other methods too!
hc.average =hclust(dist(x), method ="average")
hc.single=hclust(dist(x), method ="single")

#lets plot our dendograms
par(mfrow=c(1,3))
plot(hc.complete ,main="Complete Linkage ", xlab="", sub="",cex=.9)
plot(hc.average , main="Average Linkage", xlab="", sub="",cex=.9)
plot(hc.single , main="Single Linkage ", xlab="", sub="",cex=.9)
#note that the numbers @ the bottom of the plot identify each obs!

#we can also see the cluster labels for each obs @ a given cut of the dendogram
#done using the cutree fxn!
cutree(hc.complete,2)

cutree(hc.average,2)

cutree(hc.single,2)

#huh, single seems to do a poor job @ finding the right group, while the other 2 perform OK!

#we can try looking at it @ a diff cutpoint... but we still ahve those pesky single 2 values and 4 values lurking!
cutree(hc.single , 4)


#we can also scale our obs before we do heirarchical clustering 
xsc=scale(x)
plot(hclust(dist(xsc), method ="complete"), main=" Hierarchical Clustering with Scaled Features ")

#we can also find correlation based distance measures, as plotted below
x=matrix(rnorm (30*3), ncol=3)
dd=as.dist(1-cor(t(x)))
plot(hclust(dd, method ="complete"), main=" Complete Linkage with Correlation -Based Distance ", xlab="", sub ="")


#10.6 - Lab 3 NCI60 Data Example
#gene information that we are using unsupervised methods on
library(ISLR)
nci.labs=NCI60$labs
nci.data=NCI60$data
#note that we do indeed have a "y" here, our cancer types, but we aren't going to use them for our unsupervised methods
#we can see how good we do after trying PCA and clustering tho!

#10.6.1 PCA on the data
pr.out=prcomp(nci.data , scale=TRUE)
#note that we chose to scale it, but we could have not scaled it (b/c all genes measured on same scale)

#lets plot the first few PC's so we can look @ and understand the data

#make a fxn that assigns diff color to each of the 64 cell lines
Cols=function (vec){
   cols=rainbow (length(unique(vec)))
   return(cols[as.numeric (as.factor(vec))])
   }

#with this fxn, we can plot the PCscore vectors
par(mfrow=c(1,2))
plot(pr.out$x[,1:2], col=Cols(nci.labs), pch=19, xlab="Z1",ylab="Z2")
plot(pr.out$x[,c(1,3)], col=Cols(nci.labs), pch=19,xlab="Z1",ylab="Z3")

#lets see how good our proportion of variance explained (PVE) is!
summary(pr.out)
#also can plot it!
plot(pr.out)
#very clearly our first 3 PC's are the most impactful

#we can also plot the cumulative PVE of each principle component
pve =100*pr.out$sdev ^2/sum(pr.out$sdev ^2)
par(mfrow=c(1,2))
plot(pve , type="o", ylab="PVE", xlab=" Principal Component ", col="blue")
plot(cumsum(pve), type="o", ylab="Cumulative PVE", xlab="Principal Component ", col="brown3")

#the elbow of the scree plot here is roughly @ the 7th component

#10.6.2 Clustering on the data!
#we chose to standardize our data here!
sd.data=scale(nci.data)

#now we do heirarchical clustering using complete, avg, and single linkage, elcuidian distance is our dissimilarity measure

par(mfrow=c(1,3))
data.dist=dist(sd.data)
plot(hclust(data.dist), labels=nci.labs , main="Complete Linkage ", xlab="", sub="",ylab="")
plot(hclust(data.dist , method ="average"), labels=nci.labs , main="Average Linkage ", xlab="", sub="",ylab="")
plot(hclust(data.dist , method ="single"), labels=nci.labs , main="Single Linkage ", xlab="", sub="",ylab="")


#lets cut our dendogram to see our chosen # of clusters, here we're looking for k=4
hc.out=hclust(dist(sd.data))
hc.clusters =cutree (hc.out ,4)
table(hc.clusters ,nci.labs)
#we see clearly here that leukemia falls in clsuter 3, for example
#now lets plot the dendograms!
par(mfrow=c(1,1))
plot(hc.out , labels =nci.labs)
abline(h=139, col="red")

hc.out
#we can compare against k-means w/ k preset to 4, the same # of clusters that our dendo is cut at to see if there
#is any significant differences!
set.seed(2)
km.out=kmeans(sd.data , 4, nstart =20)
km.clusters =km.out$cluster
table(km.clusters ,hc.clusters )
#the clusters are indeed rather different!
#there are ofc some similarities... but lots more differences!
#cluster 3 in HC is same as cluster 4 in KM, however, our other clusters differ!

#we can also try HC on the PCA instead of the whole dataset, which can sometimes get superior results!
hc.out=hclust(dist(pr.out$x [,1:5]) )
plot(hc.out , labels =nci.labs , main="Hier. Clust. on First Five Score Vectors ")
table(cutree (hc.out ,4), nci.labs)
