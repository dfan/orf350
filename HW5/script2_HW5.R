dtm.control <- list(tolower = TRUE, removePunctuation = TRUE, removeNumbers = TRUE,
                    removestopWords = TRUE, stemming = TRUE, wordLengths = c(3, 15), bounds = list(global = c(2, Inf)),
                    weighting = function(x){weightTfIdf(x, normalize = FALSE)})

dtm = DocumentTermMatrix(corpus, control = dtm.control)
dtm.mat = as.matrix(dtm)