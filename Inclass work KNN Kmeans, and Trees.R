# abalone dataset from UCI repository
# reading the dataset from UCI repository URL
abalone <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"), header = FALSE, sep = ",")
# Column names
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight', 'rings' )
# summary on abalone
summary(abalone)
# structure of the abalone data
str(abalone)
# summary of the abalone rings column
summary(abalone$rings)

# As shown above, the “rings” variable has a range between 1-29. 
# This is the variable that we want to predict, and predicting this many levels 
# might not give us the insight we’re looking for.
# For now, we’ll break the rings variable 
# into 3 levels" “young” for abalones less than 8, “adult” for abalones between 8-11, 
# and “old” for abalones older than 11.
abalone$rings <- as.numeric(abalone$rings)
abalone$rings <- cut(abalone$rings, br=c(-1,8,11,35), labels = c("young", 'adult', 'old'))
abalone$rings <- as.factor(abalone$rings)
summary(abalone$rings)
# remove the "sex" varialbe in abalone, because KNN requires all numeric variables for prediction
# z <- abalone
aba <- abalone
aba$sex <- NULL

#  normalize the data using min max normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
aba[1:7] <- as.data.frame(lapply(aba[1:7], normalize))
summary(aba$shucked_wieght)
# After Normalization, each variable has a min of 0 and a max of 1.
# in otherwords, values are in the range from 0 to 1.
# We’ll now split the data into training and testing sets.
ind <- sample(2, nrow(aba), replace=TRUE, prob=c(0.7, 0.3))
KNNtrain <- aba[ind==1,]

KNNtest <- aba[ind==2,]
sqrt(2918)
# make k equal to the square root of 2918, the number of observations in the training set.
# sqrt(2918) ~= 54.01852  round it to 55 and use k = 55 # We usually take an Odd number for k value, 
# knn model 
# knn() is in the "class" library.Make sure to install it first on your RStudio.
library(class)
help("knn") # Read the knn documentation on RStudio. 
KNNpred <- knn(train = KNNtrain[1:7], test = KNNtest[1:7], cl = KNNtrain$rings, k = 55)
KNNpred
table(KNNpred)

# iris dataset is from UCI ML repository.
# kmeans clustering example from Rpubs by RStudio: https://rpubs.com/AnanyaDu/361293
library(ggplot2) # we will use ggplot2 to visualize the data.
head(iris) # first 6 rows of the 
str(iris) # take a look at the structure of the iris data using str() function in R.
# dataset has 150 observations equally distributed observations among 
# the three species: Setosa, Versicolor and Verginica.
summary(iris) # summary statistics of all the 4 variables Sepal.Length,Sepal.Width,
# Petal.Length and Petal.Width 
help("sapply")
sapply(iris[,-5], var)
summary(iris)

# plot Sepal.Length Vs Sepal.Width using ggplot 
ggplot(iris,aes(x = Sepal.Length, y = Sepal.Width, col= Species)) + geom_point()
# plot Petal.Length Vs Sepal.Width using ggplot 
ggplot(iris,aes(x = Petal.Length, y = Petal.Width, col= Species)) + geom_point()
# kmeans clustering
# Read the documentation for kmeans() function
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/kmeans.html
set.seed(300)
k.max <- 12
# tot.withinss = Total within-cluster sum of square 
# iter.max = the maximum number of iterations allowed
# nstart = if centers is a number, how many random sets should be chosen.
wss<- sapply(1:k.max,function(k){kmeans(iris[,3:4],k,nstart = 20,iter.max = 20)$tot.withinss})
wss # within sum of squares.
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")
icluster <- kmeans(iris[,3:4],3,nstart = 20)
table(icluster$cluster,iris$Species)
# In the table we can see that most of the observations have been clustered correctly 
# however, 2 of the versicolor have been put in the cluster with all the virginica 
# and 4 of the verginica have been put in cluster 3 which mostly has versicolor.

#  Classification ctrees
# iris data set
# Install the following libararies/packages 
library(rpart)
library(rpart.plot)
# we will be using the iris dataset
iris
dim(iris) # check the dimensions of the iris dataset

# creating a sample from the iris dataset
s_iris <- sample(150,100) 
s_iris

# creat testing and training sets
iris_train <-iris[s_iris,]
iris_test <-iris[-s_iris,]
dim(iris_test)
dim(iris_train)

# generate the decision tree model
dectionTreeModel <- rpart(Species~., iris_train, method = "class")
dectionTreeModel

#plotting the decision tree model using rpart.plot() function
rpart.plot(dectionTreeModel)

