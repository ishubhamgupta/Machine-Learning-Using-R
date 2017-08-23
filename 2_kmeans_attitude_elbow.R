#=================================================================================
#
# How to choose the optimal number of clusters "k"???
#
#=================================================================================
#
# K-Means Algorithm:
#           1. Each data point is randomly assigned to a cluster 
#              (number of clusters is given before hand).
#           2. Each cluster's centroid (mean within cluster) is calculated.
#           3. Each data point is assigned to its nearest centroid (iteratively 
#              to minimise the within-cluster variation) until no major differences 
#              are found.
#
#----------------------------------------------------------------------------------
#
# Data: 
#
# The dataset is a survey of clerical employees of a large financial organization. 
# The data are aggregated from questionnaires of approximately 35 employees for 
# each of 30 (randomly selected) departments. The numbers give the percent proportion
# of favourable responses to seven questions in each department. 
#
#----------------------------------------------------------------------------------
#

# Load necessary libraries
library(datasets)

# Inspect data structure
?attitude
str(attitude)

# Summarise data
summary(attitude)

# Subset the attitude data
dat = attitude[,c(3,4)] # Take only 2nd and 3rd attributes - Learning and Privileges

# Plot subset data
plot(dat, main = "% of favourable responses to
     Learning and Privilege", pch = 8) # pch - plot character - 8 = star

# With the data subset and the plot above we can see how each department's 
# score behave across Privilege and Learning compare to each other. In the
# most simplistic sense, we can apply K-Means clustering to this data set 
# and try to assign each department to a specific number of clusters that 
# are "similar"


# Perform K-Means with 2 clusters
set.seed(7)
km1 = kmeans(dat, 2, nstart=100) # nstart - no. of initial configurations, chose the best

# Note: The kmeans() function has an nstart option that attempts multiple initial 
# configurations and reports on the best one. For example, adding nstart=100 will 
# generate 100 initial configurations. This approach is often recommended.

# Plot the clusters
plot(dat, col =(km1$cluster +1) , main="K-Means result with 2 clusters", pch=8, cex=1)

# cex = plot character size

#========================================================================
# Check for the optimal number of clusters given the data - Find best k
#
# One solution often used to identifiy the optimal number of clusters is 
# called the Elbow method and it involves observing a set of possible 
# numbers of clusters relative to how they minimise the within-cluster 
# sum of squares. In other words, the Elbow method examines the within-cluster 
# dissimilarity as a function of the number of clusters. 
#========================================================================

mydata <- dat

# nrow - Return the number of rows
# sum - Returns the sum of all the values of its arguments
# apply(Array of data = mydata, columns =2, Function = var)
# var - Compute the variance of data

wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))

# Compute wss for k=1 to 15 - It helps to find what is the best k in 1 to 15
#
# kmeans returns an object with the following components:3
#
#   1. cluster	- A vector of integers (from 1:k) indicating the cluster to 
#             which each point is allocated.
#   2. centers	- A matrix of cluster centres.
#   3. totss	- The total sum of squares.
#   4. withinss	- Vector of within-cluster sum of squares, one component per cluster.
#   5. tot.withinss	- Total within-cluster sum of squares, i.e. sum(withinss).
#   6. betweenss	- The between-cluster sum of squares, i.e. totss-tot.withinss.
#   7. size	 - The number of points in each cluster.
#   8. iter	 - The number of (outer) iterations.
#   9. ifault	 - integer: indicator of a possible algorithm problem - for experts.

for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)

plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)


# Perform K-Means with the optimal number of clusters identified from the Elbow method
set.seed(7)
km2 = kmeans(dat, 6, nstart=100)

# Examine the result of the clustering algorithm
km2

# Plot results
plot(dat, col =(km2$cluster +1) , main="K-Means result with 6 clusters", pch=20, cex=2)

#=====================================================================================
# Analyze the results
#=====================================================================================

# From the results above we can see that there is a relatively well defined 
# set of groups of departments that are relatively distinct when it comes to 
# answering favourably around Privileges and Learning in the survey
