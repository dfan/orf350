df = read.csv("scotus.csv")

library(caTools)
set.seed(3000)
spl = sample.split(df$Reverse, SplitRatio = 0.7)
df.train = subset(df, spl==TRUE)
df.test = subset(df, spl==FALSE)

df.train$Reverse = as.factor(df.train$Reverse)
df.test$Reverse = as.factor(df.test$Reverse)

# Install rpart library
# install.packages("rpart")
library(rpart)
# install.packages("rpart.plot")
library(rpart.plot)

# CART model
cart.model = rpart(Reverse ~ Circuit + Issue + Petitioner + 
	Respondent + LowerCourt + Unconst, data = df.train, method="class") # Add something as required by the problem.

prp() # Complete this line

cart.prediction= predict(cart.model, newdata = , type = "class") # Complete this line!
error=sum(cart.prediction!=df.test$Reverse)/(dim(df.test)[1])

# Random Forest Model

install.packages("randomForest")
library(randomForest)

set.seed(350)

# Figure out the meaning of k and fill in the value of k here!
k=

N=dim(df.train)[1] # Total sample size
n=floor(N/k) # Split sample size
error.all=rep(0, 40) # error.all records the cross validation error with different nodesizes. 
for (j in (1:40)){
	ind=sample(N) # Random permutation of all the samples. 
	error=rep(0, k) # error records the misclassification error for each split as the testing data within cross validation. 
	for (i in 1:k){
		test=ind[(((i-1)*n+1):(min(i*n, N)))] # Choose the ith split as the testing data
		rf.model = randomForest(Reverse ~ Circuit + Issue + 
		Petitioner + Respondent + LowerCourt + Unconst, data = , ntree=10, nodesize=j+10) # Complete this line!
		rf.prediction = predict(rf.model, newdata = , type = "class") # Complete this line!
		error[i]=sum()/length(test)	# Complete this line!
		cat("=======", j, ",", i, "=======", "\n")
	}
	error.all[j]=mean(error)
}
nodesize.opt=10+which(error.all==min(error.all))[1] # In case there are multiple matches, we take the first one.

# The following code is for calculating the misclassification error on the real testing data using the best tuned nodesize.
rf.model <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = , ntree=10, nodesize=nodesize.opt) # Complete this line
rf.prediction= predict(rf.model, newdata=, type="class") # Complete this line
error= # Complete this line		


