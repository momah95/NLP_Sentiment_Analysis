#load library
library(ggplot2)
library(wordcloud)
library(e1071)
library(caret)
library(tidyverse)
library(tm)
library(SnowballC)

#load the data
tweets_new <- read.csv("tweets_new.csv")

#select text and sentiment column
tweets <- tweets_new %>% 
  select(text, sentiment)

#change structure of variables from chr to factor (appropriate structure for sentiment analysis)
tweets$text <- as.factor(tweets$text)
tweets$sentiment <- as.factor((tweets$sentiment))

#check proportion of sentiments
round(prop.table(table(tweets$sentiment)),2)

#Data Cleaning
corpus <- VCorpus(VectorSource(tweets$text))

# a snap shot of the first text stored in the corpus
as.character(corpus[[1]])

#Make text lower case, remove numbers, remove punctuation, stopwords and so on.
corpus <-  tm_map(corpus, content_transformer(tolower))
corpus <-  tm_map(corpus, removeNumbers)
corpus <-  tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <-  tm_map(corpus, stemDocument)
corpus <-  tm_map(corpus, stripWhitespace)

#Creating the Document Term Matrix for the model
#(A document-term matrix is a mathematical matrix that describes the frequency of terms 
# that occur in a collection of documents)
dtm <-  DocumentTermMatrix(corpus)
dtm
dim(dtm)

# The document-term matrix presently has 22,382 words extracted from 27,481 tweets. 
# These words are what we will use to decide if a tweet is positive or negative.
# The sparcity of the dtm is 100% which means no words is left out the matrix.

dtm <- removeSparseTerms(dtm, 0.999)

#Inspecting the the first 10 tweets and the first 15 words in the dataset
inspect(dtm[0:10, 1:15])

#identifying terms that appears more than 60 times
freq<- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
findFreqTerms(dtm, lowfreq=60)
wf<- data.frame(word=names(freq), freq=freq)


#creating wordclouds
positive <- subset(tweets,sentiment=="positive")
head(positive)
wordcloud(positive$text, max.words = 100, scale = c(3,0.5))


negative <- subset(tweets,sentiment=="negative")
head(negative)
wordcloud(negative$text, max.words = 100, scale = c(3,0.5))


neutral <- subset(tweets,sentiment=="neutral")
head(neutral)
wordcloud(neutral$text, max.words = 100, scale = c(3,0.5))


set.seed(1234)
wordcloud(words = wf$word, freq = wf$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Naive Bayes trains on categorical data, the numerical data is converted to categorical data.
# We will convert the numeric features by creating a function that converts any non-zero 
# positive value to "Yes" and all zero values to "No" to state whether a specific term is 
# present in the document.
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}


# Apply the convert_count function to get final training and testing DTMs
datasetNB <- apply(dtm, 2, convert_count)

dataset <-  as.data.frame(as.matrix(datasetNB))

# #The text corpus is now stored as a dataframe and hence, we will need to merge that with the class variable.
dataset$Class <- tweets$sentiment
str(dataset$Class)


head(dataset)
dim(dataset)


#Data Splitting
set.seed(222)
split = sample(2,nrow(dataset),prob = c(0.75,0.25),replace = TRUE)
train_set = dataset[split == 1,]
test_set = dataset[split == 2,] 

prop.table(table(train_set$Class))
prop.table(table(test_set$Class))


#Model Training
#Naive Bayes

control <- trainControl(method="repeatedcv", number=10, repeats=3)
system.time( classifier_nb <- naiveBayes(train_set, train_set$Class, laplace = 1,
                                         trControl = control,tuneLength = 7) )

#Model Evaluation for Naive Bayes
nb_pred <-  predict(classifier_nb, type = 'class', newdata = test_set)

confusionMatrix(nb_pred,test_set$Class)

f1_naive<-F1_Score(test_set$Class, nb_pred)


# Naive Bayes works on the assumption that the features of the dataset are independent of each other,
# hence called Naive.It works well for bag-of-words models a.k.a text documents since words in a text document are
# independent of each other; the location of one word doesn't depend on another word.Hence, it satisfys the independence assumption of the Naive Bayes model.
# It is therefore the most commonly used model for text classification, sentiment analysis, spam filtering & recommendation systems.



# Save Output Object
modelOutput_list <- vector(mode = "list")

## Save model output
modelOutput_list$classifier_nb <- classifier_nb

# Serializing and saving 'modelOutput_list' for later/future retrieval
saveRDS(object =modelOutput_list, file ="naive.RDS")
