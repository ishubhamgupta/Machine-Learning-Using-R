#==========================================================================
# Problem: Identify five groups/Clusters that share common interests such 
# as sports, religion, or music.
#--------------------------------------------------------------------------
# Data: Text of teenagers' Social Networking Service (SNS) pages
#
# 30,000 U.S. high school students (snsdata.csv)
# The data was sampled evenly across four high school graduation years 
# (2006 through 2009) representing four classes:
#         - the senior (2009)
#         - junior (2008)
#         - sophomore (2007)
#         - freshman (2006)
# The following data is collected from the profiles for each teen:
#         - gender
#         - age
#         - number of SNS friends was recorded.
# Convert the SNS pages into words using text mining tools.

# From the top 500 words appearing across all the pages, 36 words were chosen to 
# represent "five categories/clusters of interests": namely,
#         - extracurricular activities, 
#         - fashion, 
#         - religion, 
#         - romance, and 
#         - antisocial behaviour.
#==========================================================================

# Load the necessary libraries (cluster, stats, lattice should be present by default)
library(ggplot2) # For visulaization
library(lattice) # For visulization
library(stats)   # For Statistical calculations and random number generation  
library(cluster) # For plotting clusters with clusplot()
library(fpc)     # For plotting clusters with plotcluster(), validating clusters

#setwd("C:/Users/Ramesh/Desktop/0_PGDBA-Feb-2016/1_PPTS_Materials/2_ML/1_EXAMPLES/2_3_clustering")
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load Data - Data is in CSV format
teens <- read.csv("snsdata.csv")

# for each person, how many times each word appeared in the person's SNS profile.
# The data include 30,000 teenagers with four variables indicating personal characteristics 
# (gradyear, gender, age and friends) and 36 words indicating interests (basketball, football, soccer, etc).

str(teens) # Structure of an R object - Get all the attributes, type, and other information

teens[1:5,] # Inspect first 5 feature vectors of 5 students
#=================================================================
# 1. Analyse the data and understand it
#=================================================================

#-----------------------------------------------------------------
# check for the presence of missing values in any of four variables
# corresponding to users' private information.
#   Gradyear, gender, age, friends
#-----------------------------------------------------------------

table(teens$gradyear, useNA = "ifany") # Cross tabulation - Count of factors and missing values

table(teens$gender, useNA = "ifany") 

summary(teens$age) # Summaries of a field - min, max, NA's, Mean, Median etc.

summary(teens$friends)

# Note: The summaries show that there are no missing values for the
# gradyear and friends variables. However, we have 2724 users (~9%) 
# with a missing value of their gender and 5086 users (~17%) with missing age.

#-----------------------------------------------------------------
# Check for outliers
#-----------------------------------------------------------------
# Although the majority of users provides the age range of 16 to 18 years, 
# there are also outliers ranging between 3 and 106 years:

boxplot(teens$age, ylab = "Age")
#nr rug(jitter(teens$age), side = 2) # Creates tick marks on side-2 means y-axis
#nr abline(h = mean(teens$age, na.rm = T), lty = 2) # Add verticle, horizontal line to a plot

#=================================================================
# 2. Cleaning and preparing the data
#=================================================================

# A reasonable range of ages for high school students includes those who are at least 13 years old 
# and not yet 20 years old. Any age value falling outside this range will be treated
# the same as missing data because it is not feasible to trust the age provided.

# Consider only age between 13 and 20, treat all others as Missing values - NA
teens$age <- ifelse(teens$age >= 13 & teens$age < 20, teens$age, NA)
summary(teens$age)

# create dummy variables for female and unknown gender.
teens$female <- ifelse(teens$gender == "F" & !is.na(teens$gender), 1, 0)
teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)

#------------------------------------------------------------------
# Imputing missing values - Use average age of each graduation year
#------------------------------------------------------------------

# In order to eliminate 5523 missing values on age, 
# I will use an imputation of mean age values, calculated for each specific 
# graduation year.

#mean(teens$age, na.rm = TRUE) # Remove NAs and compute mean of all the students

# Find out the mean of ages for each graduation year
aggregate(data = teens, age ~ gradyear, mean, na.rm = TRUE)

# Find out the mean for each graduation year
ave_age <- ave(teens$age, 
               teens$gradyear, 
               FUN = function(x) mean(x, na.rm = TRUE))

# Fill all the missing age fields with average age
teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)

#=================================================================
# 3. Train the model
#=================================================================

# Using z-scoe standardization 
# (rescales features such that they have a mean of zero and a standard deviation of one.)
# select only 36 features.

interests <- teens[5:40]  # Interests columns are from 5 to 40 in csv file
head(interests)
interests_z <- as.data.frame(lapply(interests, scale)) # Apply scale function to interests and return as data frame
head(interests_z)
# interests_z

set.seed(12)

#---------------------------------------------------------------
# train the model using k-means clustering, use k as 5
#---------------------------------------------------------------

# kmeans(x, centers, iter.max = 10, nstart = 1,
# algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
#              "MacQueen"), trace=FALSE)

teen_clusters <- kmeans(interests_z, centers=5, iter.max=10)
teen_clusters
teen_clusters$cluster 

# Plot the clusters

clusplot(interests_z,
         teen_clusters$cluster, 
         color=TRUE,
         col.clus=c(1:5)[unique(teen_clusters$cluster)],
         shade=TRUE,
         labels=5, 
         lines=0, 
         main = "Bivariate Cluster Plot")

plotcluster(interests_z, teen_clusters$cluster)

#=================================================================
# Evaluate model performance
#=================================================================

# check how many members are there in each cluster. Usually, if the
# groups are too large or too small, then they are not likely to be very useful.

teen_clusters$size

# the third cluster contains 21386 users, which is quite a lot compared to other clusters.

# examine the coordinates of the cluster centroids and print out the list of 
# all the clusters with associated interests. Negative values here are below
# the overall mean for all students and positive values are above the mean.

teen_clusters$centers

#=-----------------------------------------------------------
# Understand the model results and get insights
#=-----------------------------------------------------------
teens$cluster <- teen_clusters$cluster

# Find out the cluster numbers of first 10 teens

teens[1:10, c("cluster", "gender", "age", "friends")]

# Average age of each cluster
aggregate(data = teens, age ~ cluster, mean)

# % female in each cluster 
aggregate(data = teens, female ~ cluster, mean)

# Avg friends for each cluster
aggregate(data = teens, friends ~ cluster, mean)

#====================================================================
# Summary of Analytics/Insights (Sample)
#====================================================================
# Overall about 74 percent of the SNS users are female. 
# Clusters 2 and 5 are around 83 percent female, 
# while Cluster 3 is only 69 procent female. 

# The same cluster 3 has the smallest amount of friends on average 
# - about 27 percent, while clusters 2 and 5 have the highest average 
# number of friends.

# The association among group membership, gender, and number of 
# friends suggests that the clusters can be useful predictors for 
# preparing e.g. marketing activities based on these insights.