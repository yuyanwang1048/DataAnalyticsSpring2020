# Group3 Lab4
??kyphosis
library(e1071)
set.seed (1)
# generating the observations, which belong to two classes.
x=matrix(rnorm(20*2), ncol=2)
y=c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 1
x
y
# checking whether the classes are linearly separable.
plot(x, col=(3-y))
# They are not. Next, we fit the support vector classifier. 
dat <- data.frame(x = x,y  = as.factor(y))
svmfit <- svm(y ~., data=dat, kernel="linear", cost=10,scale=FALSE)
# plot the support vector classifier obtained:
plot(svmfit , dat)
# The boudary is linear since kernel="linear"
# The support vectors are plotted as crosses and the remaining observations are plotted as circles; we see here that there are seven support vectors. 
# We can determine the identities of those support vectors by:
svmfit$index
summary(svmfit)
# This tells us, for instance, that a linear kernel was used with cost=10, and that there were seven support vectors, four in one class and three in the other. 

# Then we use a smaller value. cost=0.1
svmfit <- svm(y ~., data=dat, kernel="linear", cost = 0.1, scale=FALSE)
plot(svmfit , dat)
svmfit$index
# smaller cost means wider margin, so there is more support factors. 

# Compares SVM with a linear kernal
set.seed (1)
tune.out <- tune(svm, y ~.,data=dat,kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)
# We see that cost=0.1 results in the lowest cross-validation error rate.
# The tune() function stores the best model obtained, which can be accessed as follows:
bestmod=tune.out$best.model 
summary(bestmod)
# generates test dataset
xtest=matrix(rnorm(20*2), ncol=2)
ytest=sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,] + 1
testdat=data.frame(x=xtest, y=as.factor(ytest))
# prediction
ypred <-predict(bestmod ,testdat)
table(predict=ypred, truth=testdat$y)

# when cost= 0.01
svmfit <- svm(y~., data=dat, kernel="linear", cost=.01, scale=FALSE)
ypred=predict(svmfit ,testdat)
table(predict=ypred, truth=testdat$y)

x[y==1,]=x[y==1,]+0.5
plot(x, col=(y+5)/2, pch=19)

# observations are linearly separable.
# We fit the support vector classifier and plot the resulting hyperplane, using a very large value of cost so that no observations are misclassified.
dat=data.frame(x=x,y=as.factor(y))
svmfit <-svm(y~., data=dat, kernel="linear", cost=1e5)
summary(svmfit)
plot(svmfit,dat)

# we can see from the figure that the margin is very narrow (because the observations that are not support vectors, indicated as circles, are very close to the decision boundary). It seems likely that this model will perform poorly on test data. 
# We now try a smaller value of cost:

svmfit <- svm(y~., data=dat, kernel="linear", cost=1)
summary(svmfit)
plot(svmfit ,dat)

# It seems likely that this model will perform better on test data than the model with cost=1e5.

# We now examine the Khan data set, which consists of a number of tissue samples 
library(e1071)
library(ISLR)
names(Khan)
dim(Khan$xtrain )
dim(Khan$xtest )

length(Khan$ytrain )
length(Khan$ytest )
table(Khan$ytrain )
table(Khan$ytest )

dat <- data.frame(x=Khan$xtrain , y = as.factor(Khan$ytrain ))
out <- svm(y ~., data=dat, kernel="linear",cost=10)
summary(out)


dat.te=data.frame(x=Khan$xtest , y = as.factor(Khan$ytest ))
pred.te=predict(out, newdata=dat.te)
table(pred.te, dat.te$y)
# We see that using cost=10 yields two test set errors on this data.


