library(tm)
library(XML)
library(Matrix)
library(slam)
library(SnowballC)
library(akmeans)
library(lsa)

load("Wikipedia.RData")
text = dat[,"text"]
text = iconv(text, to = "utf-8") #some conversion on SMILE needed
corpus = Corpus(VectorSource(text))
dtm.control.raw <- list(tolower = TRUE, removePunctuation = TRUE, removeNumbers = TRUE,
                    removestopWords = TRUE, stemming = TRUE, wordLengths = c(3, 15), bounds = list(global = c(2, Inf)))

dtm.raw = DocumentTermMatrix(corpus, control = dtm.control.raw)
dtm.mat.raw = as.matrix(dtm.raw) # our term-document matrix