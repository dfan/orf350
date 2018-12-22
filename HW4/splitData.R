set.seed(10)
idx = sample(1:nrow(dtm.mat), floor(nrow(dtm.mat)*.9))
train = dtm.mat[idx,]
test = dtm.mat[-idx,]

idx = which(colnames(dtm.mat) == "sentiment.bool")
train.x = train[,-idx]
train.y = train[,idx]

test.x = test[,-idx]
test.y = test[,idx]

idx = which(colnames(train.x) == "document.tag")
train.tag = train.x[,idx]
train.x = train.x[,-idx]

test.tag = test.x[,idx]
test.x = test.x[,-idx]