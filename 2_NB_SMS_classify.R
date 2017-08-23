#===================================================
# SMS Spam filter
#===================================================
library(tm)
#library(SnowballC)
# library(wordcloud)
# library(e1071)

# We need to transform our data into Bag of Words, which ignores word order and
# simply provides a variable indicating whether the word appears at all.

#setwd("/Users/Ramesh/Desktop/0_PGDBA-Feb-2016/1_PPTS_Materials/2_ML/1_EXAMPLES/4_Classification")

#---------------------------------------------------
# 1. Explore and prepare the data
#---------------------------------------------------
sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = FALSE)


# see that the sms_raw data frame includes 5,559 total
# SMS messages with two features: type and text.
str(sms_raw)

# The type element is currently a character vector. 
# Since this is a categorical variable, it would be better to convert it into a factor,

sms_raw$type <- factor(sms_raw$type)

str(sms_raw$type)

table(sms_raw$type)


#--------------------------------------------------------
# 2. Data preparation - cleaning and standardizing text data
#--------------------------------------------------------

# SMS messages are strings of text composed of words, spaces, numbers, and
# punctuation. Handling this type of complex data takes a lot of thought and
# effort. One needs to consider how to remove numbers and punctuation; handle
# uninteresting words such as and, but, and or; and how to break apart sentences into
# individual words. Thankfully, this functionality has been provided by the members
# of the R community in a text mining package titled tm.

# (a) Create a text corpus, from the collection of SMS messages.

sms_corpus <- VCorpus(VectorSource(sms_raw$text))

print(sms_corpus)

# Inspect the first and second SMS messages in the corpus:

inspect(sms_corpus[1:2])

# How to view the original SMS??
as.character(sms_corpus[[2]])

# How to view multiple SMSs? Use lapply().
lapply(sms_corpus[1:2], as.character)

# (b) Divide the SMSs into words. 


# (c) Standardize or clean the SMSs: removing
# punctuation and other characters that cluster the result. For example, we would like
# the strings Hello!, HELLO, and hello to be counted as instances of the same word

# standardize the messages to use only lowercase characters.
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))

# Check whether tolower() worked or not.
as.character(sms_corpus[[1]])

# Remove numbers from SMSs, bcoz they wont give any useful information
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)

# Remove stop words - a, and, an, the etc. (There are 571 words in tm package)
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords())

# eliminate any punctuation from the text messages
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)

#-------
# Test
# strips punctuation characters from the
# text blindly, which can lead to unintended consequences
#-------
removePunctuation("hello...world")
# Word hello and world becomes a single word "helloworld"
# But, we are ignoring this in this problem currently.
# We can create our own custom function:

# replacePunctuation <- function(x) {
#     gsub("[[:punct:]]+", " ", x)

#------

# (d) Do stemming: Reduce the words to roor form: 
#     learning, learns, learned will become learn.

library(SnowballC)

# Stemming example
# wordStem(c("learn", "learned", "learning", "learns"))

# Use stemDocument() method to apply stemming to entire corpus
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)

# Remove white spaces
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)

# SMS messages before cleaning
#as.character(sms_corpus[1:3])

# SMS messages after cleaning
#as.character(sms_corpus_clean[1:3])

# Splitting text document into words
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

# Split with preprocessing also*****

sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(tolower = TRUE,removeNumbers = TRUE,
                                                          stopwords = TRUE,
                                                          removePunctuation = TRUE,
                                                          stemming = TRUE
))
  
sms_dtm

sms_dtm2  #It uses slightly a different stopwords method, so result may vary

# =======================================================

# 3. Create train and test data sets

# data into two portions: 75 percent for training and 25 percent for testing

# =======================================================

sms_dtm_train <- sms_dtm[1:4169, ]

sms_dtm_test <- sms_dtm[4170:5559, ]

# Labels are not stored in DTM, so,  save a pair of vectors with labels for 
# train and test data

sms_train_labels <- sms_raw[1:4169, ]$type
sms_test_labels <- sms_raw[4170:5559, ]$type

# compare the proportion of spam in the training and test data frames:

prop.table(table(sms_train_labels))

prop.table(table(sms_test_labels))

#========================================================

# 3(b). Visualizing text data - word clouds

#========================================================

library(wordcloud)

# random.order = FALSE, the cloud will be arranged in a nonrandom order with higher
# frequency words placed closer to the center. 
wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)

# Compare the clouds for ham and spam

spam <- subset(sms_raw, type == "spam")
ham <- subset(sms_raw, type == "ham")

wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))


#========================================================

# 3(c). creating indicator features for
#       frequent words

#========================================================

#  the words appearing at least five times in the sms_dtm_train matrix
findFreqTerms(sms_dtm_train, 5)

sms_freq_words <- findFreqTerms(sms_dtm_train, 5)

# Get only the words which are present in sms_freq_words
# The training and test datasets now include 1,136 features, which correspond to
# words appearing in at least five messages.
sms_dtm_freq_train<- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]

# NB Classifier can work only on categorical values, so, change the numerical
# values to categorical values

#  The ifelse(x > 0, "Yes", "No") statement transforms
# the values in x, so that if the value is greater than 0, then it will be replaced by "Yes",
# otherwise it will be replaced by a "No" string. Lastly, the newly transformed x vector
#is returned.

convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

sms_train <- apply(sms_dtm_freq_train, MARGIN = 2,
                   convert_counts)


sms_test <- apply(sms_dtm_freq_test, MARGIN = 2,
                  convert_counts)

#=============================================================
# 4. Train the model
#=============================================================

library(e1071)

sms_classifier <- naiveBayes(sms_train, sms_train_labels)

#=============================================================
# 5. Evaluate/TEst the model
#=============================================================

sms_test_pred <- predict(sms_classifier, sms_test)

library(gmodels)

CrossTable(sms_test_pred, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))

# Results: Looking at the table, we can see that a total of only 6 + 30 = 36 of the 1,390 SMS
# messages were incorrectly classified (2.6 percent). Among the errors were 6 out
# of 1,207 ham messages that were misidentified as spam, and 30 of the 183 spam
# messages were incorrectly labeled as ham

#=============================================================
# 6. Improving the model
#=============================================================

# You may have noticed that we didn't set a value for the Laplace estimator while
# training our model. This allows words that appeared in zero spam or zero ham
# messages to have an indisputable say in the classification process. Just because the
# word "ringtone" only appeared in the spam messages in the training data, it does
#not mean that every message with this word should be classified as spam

sms_classifier2 <- naiveBayes(sms_train, sms_train_labels,laplace = 1)

sms_test_pred2 <- predict(sms_classifier2, sms_test)

CrossTable(sms_test_pred2, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

# Result: Adding the Laplace estimator reduced the number of false positives (ham messages
# erroneously classified as spam) from six to five and the number of false negatives
# from 30 to 28. 


