load_mnist <- function() {
  load_image_file <- function(filename) {
    ret = list()
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    ret$n = readBin(f,'integer',n=1,size=4,endian='big')
    nrow = readBin(f,'integer',n=1,size=4,endian='big')
    ncol = readBin(f,'integer',n=1,size=4,endian='big')
    x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F)
    ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
    close(f)
    ret
  }
  load_label_file <- function(filename) {
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    n = readBin(f,'integer',n=1,size=4,endian='big')
    y = readBin(f,'integer',n=n,size=1,signed=F)
    close(f)
    y
  }
  train <- load_image_file('train-images-idx3-ubyte')
  test <- load_image_file('t10k-images-idx3-ubyte')
  
  train$y <- load_label_file('train-labels-idx1-ubyte')
  test$y <- load_label_file('t10k-labels-idx1-ubyte') 
}

show_digit <- function(arr784, col=gray(12:1/12),...) {
  image(matrix(arr784, nrow=28)[,28:1], col=col,...)
}

show_digitsmall <- function(arr196, col=gray(12:1/12), ...) {
  image(matrix(arr196, nrow=14)[,14:1], col=col, ...)
}


compressImg <- function(full){
  compressFour <- function(j){
    pixelvec = rep(NA,4)
    pixelvec[1] = full[2*j-1+floor((j-1)/14)*28];
    pixelvec[2] = full[2*j+floor((j-1)/14)*28];
    pixelvec[3] = full[2*j-1+28+floor((j-1)/14)*28];
    pixelvec[4] = full[2*j+28+floor((j-1)/14)*28];
    return(mean(pixelvec))
  }
  
  compress = unlist(lapply(1:196,compressFour))
  return(compress)
}

# ---- reading in train and test and selecting only 0,1,2,3,4 ----
setwd("/Users/dfan/Dropbox/School/Sophomore\ Year/Spring\ 2017/ORF\ 350/Assignments/HW5")
load_mnist()
train.images = train$x[which(train$y == 0 |train$y == 1 | train$y == 2 | train$y == 3 | train$y == 4),]
train.labels = train$y[which(train$y == 0 |train$y == 1 | train$y == 2 | train$y == 3 | train$y == 4)]
train.num = length(which(train$y == 0 |train$y == 1 | train$y == 2 | train$y == 3 | train$y == 4))

# ---- actual compressing (takes a long time) -----
compress.train.images = matrix(NA,nrow=dim(train.images)[1],ncol=14*14)
compress.train.images[1:30596,] = t(apply(train.images[1:30596,],1,compressImg))
save(compress.train.images,file="compress.train.images.RData")

# ---- plot table / plot a ----
plotTable <- function(numCol,vec.labels,mat.images){
  vec.uniq = unique(vec.labels)
  par(mfrow=c(length(vec.uniq),numCol),pty="s",mar = c(0.1,0.1,0.1,0.1))
  for(i in 1:length(vec.uniq)){
    tmpidx = which(vec.labels==vec.uniq[i])
    for(j in 1:numCol){
      show_digitsmall(mat.images[tmpidx[j],],asp=TRUE)
    }
  }
}
# plotTable(12, gamma.new, compress.train.images)

# ---- plot b ----
for (j in 1:5) {
  show_digitsmall(mu.min[j,])
}

# ---- plot c ----
linMap <- function(x, low, high) {
  return((x - min(x)) / max(x - min(x)) * (high - low) + low) 
}
for (j in 1:5) {
  jthcluster = compress.train.images[which(gamma.new==j),]
  variance = apply(jthcluster, 2, var)
  pixels = linMap(variance,0,255)
  pixels = 255- pixels
  show_digitsmall(pixels)
}

# ---- table 1 ----
table = matrix(0,5,5)
for (i in 1:5) {
  ithcluster = which(gamma.new==i)
  for (j in 0:4) {
    count = 0
    for (k in 1:length(ithcluster)) {
      if (train.labels[ithcluster[k]] == j)
        count = count + 1
    }
    table[i,j+1] = count
  }
}



