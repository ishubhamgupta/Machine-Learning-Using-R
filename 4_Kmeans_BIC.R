#================================================================================
# 
#================================================================================
# Prepare Data
mydata <- na.omit(mydata) # listwise deletion of missing
mydata <- scale(mydata) # standardize variables

str(mydata)
summary(mydata)

dim(mydata)

#---------------------------------------------------------------------------------
# Determine number of clusters - Elbow Method
#---------------------------------------------------------------------------------
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(mydata, 5) # 5 cluster solution

# get cluster means 
aggregate(mydata,by=list(fit$cluster),FUN=mean)

# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)

#---------------------------------------------------------------------------------
#
#---------------------------------------------------------------------------------
# Model Based Clustering
library(mclust)

fit <- Mclust(mydata)

plot(fit) # plot results 

summary(fit) # display the best model

# K-Means Clustering with 5 clusters
fit <- kmeans(mydata, 5)

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
library(cluster) 
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(mydata, fit$cluster)

#----------------------------------------------------
# VAlidate the clusters
#----------------------------------------------------
# comparing 2 cluster solutions
library(fpc)
cluster.stats(d, fit1$cluster, fit2$cluster)

