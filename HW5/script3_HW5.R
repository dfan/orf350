dtm.mat.indicator = dtm.mat.raw
dtm.mat.indicator[dtm.mat.indicator!=0] = 1

word.presence = apply(dtm.mat.indicator, 2, sum)
idx = which(word.presence >= quantile(word.presence, prob = 0.99))
dtm.mat.raw = dtm.mat.raw[,-idx]

common.words = read.csv("google-10000-english.txt", header = F)
idx = which(colnames(dtm.mat.raw) %in% common.words[1:300,1])
dtm.mat.raw = dtm.mat.raw[,-idx]