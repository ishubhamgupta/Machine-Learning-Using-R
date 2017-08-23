#==================================================================================

# Build a predictive model which will identify customers who are more likely to 
# respond to term deposit cross sell campaign.

#----------------------------------------------------------------------------------
# Data Set: https://archive.ics.uci.edu/ml/machine-learning-databases/00222/

# Input dataset has 20 independent variables and a target variable. 
# The target variable y is binary (Yes or No).
#----------------------------------------------------------------------------------
# A marketing department of a bank runs various marketing campaigns for cross-selling 
# products, improving customer retention and customer services.

# In this example, the bank wanted to cross-sell term deposit product to its customers.
# Contacting all customers is costly and does not create good customer experience.
#==================================================================================

library(randomForest)

## Read data
termCrosssell<-read.csv(file="bank-5000.csv",header = T, sep = ";")

## Explore data frame

names(termCrosssell)

##  [1] "age"            "job"            "marital"        "education"     
##  [5] "default"        "housing"        "loan"           "contact"       
##  [9] "month"          "day_of_week"    "duration"       "campaign"      
## [13] "pdays"          "previous"       "poutcome"       "emp.var.rate"  
## [17] "cons.price.idx" "cons.conf.idx"  "euribor3m"      "nr.employed"   
## [21] "y"

# How much % of target variable has "yes"...
table(termCrosssell$y)/nrow(termCrosssell)

## 
##        no       yes 
## 0.8873458 0.1126542

# termCrosssell$y

#---------------------------------------------------------------
# split the data sample into development(train) and validation(test) samples.
#---------------------------------------------------------------
sample.ind <- sample(2, 
                     nrow(termCrosssell),
                     replace = T)
#                     prob = c(0.6,0.4))

cross.sell.dev <- termCrosssell[sample.ind==1,] # 1st sample for train
cross.sell.val <- termCrosssell[sample.ind==2,] # 2nd sample for test/validation 


#---------------------------------------------------------------
# Check whether Both development and validation samples have similar target variable distribution.
#---------------------------------------------------------------
table(cross.sell.dev$y)/nrow(cross.sell.dev)
## 
##        no       yes 
## 0.8859289 0.1140711


table(cross.sell.val$y)/nrow(cross.sell.val)
## 
## no yes 
## 0.8894524 0.1105476

#---------------------------------------------------------------
# What is the type of the target variable
# if it is factor means it's a classification target
#---------------------------------------------------------------
class(cross.sell.dev$y)

## [1] "factor"

#---------------------------------------------------------------
# Make formaula of all the features
#---------------------------------------------------------------
varNames <- names(cross.sell.dev)
# Exclude ID or Response variable
varNames <- varNames[!varNames %in% c("y")]

# add + sign between exploratory variables
varNames1 <- paste(varNames, collapse = "+")

# Add response variable and convert to a formula object
rf.form <- as.formula(paste("y", varNames1, sep = " ~ "))

#---------------------------------------------------------------
# Building Random Forest using R
#---------------------------------------------------------------

# 500 decision trees or a forest has been built using the Random 
# Forest algorithm based learning. We can plot the error rate across decision trees. 
# The plot seems to indicate that after 100 decision trees, there is not a 
# significant reduction in error rate.


cross.sell.rf <- randomForest(rf.form,
                              cross.sell.dev,
                              ntree=500,
                              importance=T)

plot(cross.sell.rf)

#---------------------------------------------------------------
# Find out the important features
#---------------------------------------------------------------

# Variable importance plot is also a useful tool and can be plotted 
# using varImpPlot function. Top 5 variables are selected and plotted
# based on Model Accuracy and Gini value. We can also get a table with 
# decreasing order of importance based on a measure (1 for model accuracy 
# and 2 node impurity)

# Variable Importance Plot
varImpPlot(cross.sell.rf,
           sort = T,
           main="Variable Importance",
           n.var=5)

# Variable Importance Table
var.imp <- data.frame(importance(cross.sell.rf,type=2))
# make row names as columns
var.imp$Variables <- row.names(var.imp)
var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),]

# Based on Random Forest variable importance, the variables 
# could be selected for any other predictive modelling techniques or machine learning.

#---------------------------------------------------------------
# Measuer the accuracy of the RF model
#---------------------------------------------------------------

# Predicting response variable
cross.sell.dev$predicted.response <- predict(cross.sell.rf ,cross.sell.dev)


# Create confusion matrix
library(e1071)
library(caret)

## Loading required package: lattice
## Loading required package: ggplot2
# Create Confusion Matrix
confusionMatrix(data=cross.sell.dev$predicted.response,
                reference=cross.sell.dev$y,
                positive='yes')

# Predicting response variable
cross.sell.val$predicted.response <- predict(cross.sell.rf ,cross.sell.val)

# Create Confusion Matrix
confusionMatrix(data=cross.sell.val$predicted.response,
                reference=cross.sell.val$y,
                positive='yes')

