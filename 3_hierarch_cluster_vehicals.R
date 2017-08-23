#==================================================================================
#
# To illustrate interpretation of the dendogram, we'll look at a cluster analysis 
# performed on a set of cars.
#
#==================================================================================


head(mtcars)

# Find the distance matrix

d <- dist(as.matrix(mtcars))   # find distance matrix 

print(d)

# apply hirarchical clustering - The hclust function in R
# uses the complete linkage method for hierarchical clustering by default.

hc <- hclust(d)

print(hc)

# plot the dendrogram

plot(hc)  # Simple plotting


