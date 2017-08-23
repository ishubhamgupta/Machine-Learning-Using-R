#Installing library
install.packages("jpeg") 
#Loading library into R Session
library("jpeg")

#Read image
dog_img<-readJPEG("cat.jpg")

# Obtain the dimension i.e (Pixels, Color Values)
img_Dm <- dim(dog_img)

# Lets assign RGB channels to a data frame
img_RGB <- data.frame(
  x_axis = rep(1:img_Dm[2], each = img_Dm[1]),
  y_axis = rep(img_Dm[1]:1, img_Dm[2]),
  Red = as.vector(dog_img[,,1]),
  Green = as.vector(dog_img[,,2]),
  Blue = as.vector(dog_img[,,3])
)

library(ggplot2)

# Plot the image
ggplot(data = img_RGB, aes(x = x_axis, y = y_axis)) +
  geom_point(colour = rgb(img_RGB[c("Red", "Green", "Blue")])) +
  labs(title = "Original Image") +
  xlab("x-axis") +
  ylab("y-axis")

#Show time - Let's start the clustering

#Running the WSSPLOT function again
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(img_RGB[c(3,4,5)],25)

#running the k-means algorithm

k_cluster <- 5
k_img_clstr <- kmeans(img_RGB[, c("Red", "Green", "Blue")],
                      centers = k_cluster)
k_img_colors <- rgb(k_img_clstr$centers[k_img_clstr$cluster,])

#plotting the compressed image
ggplot(data = img_RGB, aes(x = x_axis, y = y_axis)) +
  geom_point(colour = k_img_colors) +
  labs(title = paste("k-Means Clustering of", k_cluster, "Colours")) +
  xlab("x") +
  ylab("y")