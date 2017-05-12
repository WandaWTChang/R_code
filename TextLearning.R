##### Text learning--------------------
rm(list=ls())
Sys.getlocale(category = "LC_ALL")
setwd('C:/Users/user/Desktop/MachineLearningWithR/MLwR-master/Machine Learning with R (2nd Ed.)/Chapter 04')
getwd()

sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
n.sms_raw <- length(sms_raw$type)

# Text Cleaning----------------------------------------------------------------
library(tm)
sms_corpus <- VCorpus(VectorSource(sms_raw$text))
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers) # remove numbers
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords()) # remove stop words
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation) # remove punctuation
# tip: create a custom function to replace (rather than remove) punctuation
removePunctuation("hello...world")
replacePunctuation <- function(x) { gsub("[[:punct:]]+", " ", x) }
replacePunctuation("hello...world")

# illustration of word stemming
library(SnowballC)
wordStem(c("learn", "learned", "learning", "learns"))

sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)

sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace) # eliminate unneeded whitespace

inspect(sms_corpus_clean)

tdm <- TermDocumentMatrix(sms_corpus_clean)
tdm

#inspect frequent words
freq.terms <- findFreqTerms(tdm, lowfreq=100)
term.freq <- sort(rowSums(as.matrix(tdm)), decreasing = TRUE)
df <- data.frame(word = names(term.freq), freq = term.freq)
head(df, 20)

if(!require(ggplot))  install.packages("ggplot")
if(!require(ggplot2))  install.packages("ggplot2")
ggplot(df, aes(x=word, y=freq))+
  geom_bar(stat = "identity")+
  xlab("Words") + ylab("Count") +coord_flip()

#source("http://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")
#library(Rgraphviz)
#plot(tdm, term = freq.terms, corThreshold = 0.12, weighting = T)

# Word Cloud--------------------------------------------------------------
library(wordcloud)
wordcloud(words = df$word, freq = df$freq, min.freq = 3,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Set2"))

install.packages("htmltools")
install.packages("jsonlite")
install.packages("yaml")
library(wordcloud2)
wordcloud2(df, size = 2, color = "random-light")

# Clustering---------------------------------------------------------------
# remove sparse terms
tdm2 <- removeSparseTerms(tdm, sparse = 0.95)

# Topic Model---------------------------------------------------------------

