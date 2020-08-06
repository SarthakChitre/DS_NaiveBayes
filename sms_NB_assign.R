sms=read.csv(file.choose())
View(sms)
class(sms)
str(sms)
sms$type=factor(sms$type)
#Text Mining
library(tm)
sms_corpus=Corpus(VectorSource(sms$text))

#Cleaning Data
corpus_clean<-tm_map(sms_corpus,tolower)
corpus_clean<-tm_map(corpus_clean, removeNumbers)
corpus_clean<-tm_map(corpus_clean,removeWords, stopwords())
corpus_clean<-tm_map(corpus_clean,removePunctuation)
corpus_clean<-tm_map(corpus_clean,stripWhitespace)
class(corpus_clean)

inspect(corpus_clean)

#Creating document term matrix
sms_dtm=DocumentTermMatrix(corpus_clean)
inspect(sms_dtm)

# creating training and test datasets
sms_raw_train <- sms[1:4169, ]
sms_raw_test  <- sms[4170:5559, ]

sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test  <- sms_dtm[4170:5559, ]

sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test  <- corpus_clean[4170:5559]

# check that the proportion of spam is similar
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

#wordcloud visualization
library(wordcloud)
wordcloud(sms_corpus,min.freq = 20)

spam=subset(sms_raw_train,type=="spam")
ham=subset(sms_raw_test,type=="ham")

wordcloud(spam$text,max.words=30,scale=c(3,.2),colors = "Blue")
wordcloud(ham$text,max.words=100,scale=c(3,.2),colors = "Violet")

#indicator features for frequent words
sms_dict=findFreqTerms(sms_dtm_train,5)

sms_train=DocumentTermMatrix(sms_corpus_train,list(dictionary=sms_dict))
sms_test=DocumentTermMatrix(sms_corpus_test,list(dictionary=sms_dict))
sms_dict

# convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

# apply() convert_counts() to columns of train/test data
sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_test, MARGIN = 2, convert_counts)
View(sms_train)
View(sms_test)

##  Training a model on the data ----
library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)
sms_classifier

sms_test_pred=predict(sms_classifier,sms_test)
sms_test_pred

##  Evaluating model performance ----

library(gmodels)
CrossTable(sms_test_pred, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual')) #0.974


sms_classifier2 <- naiveBayes(sms_train, sms_raw_train$type, laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
# Accuracy 
mean(sms_test_pred2==sms_raw_test$type)
