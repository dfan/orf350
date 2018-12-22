
N=3000000		# number of rating records
Nu=135359		# maximum of UserID
Np=220970		# maximum of ProfileID
user.rat=rep(0,Nu)		# user.rat[i] denotes the sum of ratings given by user i
user.num=rep(0,Nu)		# user.num[i] denotes the number of ratings given by user i
profile.rat=rep(0,Np)		# profile.rat[i] denotes the sum of ratings given to profile i
profile.num=rep(0,Np)		# user.rat[i] denotes the number of ratings given to profile i
for (i in 1:N){	# In each iteration, we update the four arrays, i.e. user.rat, user.num, profile.rat, profile.num, using one rating record.
	user.rat[X[i,'UserID']]=user.rat[X[i,'UserID']]+X[i,'Rating'] # The matrix X here comes from the file 'ratings.dat'
	user.num[X[i,'UserID']]=user.num[X[i,'UserID']]+1
	profile.rat[X[i,'ProfileID']]=profile.rat[X[i,'ProfileID']]+X[i,'Rating']
	profile.num[X[i,'ProfileID']]=profile.num[X[i,'ProfileID']]+1
	if (i %% 10000==0) print(i/10000)
}
user.ave=user.rat/user.num
profile.ave=profile.rat/profile.num
X1=big.matrix(nrow=nrow(X), ncol=ncol(X), type= "double", dimnames=list(NULL, c('UsrAveRat','PrfAveRat','Rat')))
X1[,'Rat']=X[,'Rating']
X1[,'UsrAveRat']=user.ave[X[,'UserID']]
X1[,'PrfAveRat']=profile.ave[X[,'ProfileID']]		# X1 is the new data matrix we will work with in regression.
