#load packages
library(tm)
library(XML)
library(Matrix)
library(slam)
library(SnowballC)

#prepare the text documents
text = dat[,"review"]
text = iconv(text, to = "utf-8") #some conversion on SMILE needed
corpus = Corpus(VectorSource(text))

#convert to term-document matrix
dtm.control <- list(tolower = TRUE, removePunctuation = TRUE, removeNumbers = TRUE,
 removestopWords = TRUE, stemming = TRUE, wordLengths = c(3, 15), bounds = list(global = c(2, Inf)))
dtm = DocumentTermMatrix(corpus, control = dtm.control)

#the term-document matrix
dtm.mat = as.matrix(dtm)

#Append the sentiment (a 0-1 vector) and the index tag
sentiment.bool = dat$rating > 3
document.tag = 1:nrow(dtm.mat)
dtm.mat = cbind(dtm.mat, sentiment.bool, document.tag)