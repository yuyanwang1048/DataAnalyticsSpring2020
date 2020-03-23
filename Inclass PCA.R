# Yuyan Wang
# Inclass work 
# Decision trees,  Introduction to Dimension Reduction (DR) and Principal Component Analysis (PCA)

# Random Forest
require(randomForest)
fitKF <- randomForest(Kyphosis ~ Age + Number + Start,   data=kyphosis)
print(fitKF) 	# view results
randomForest(formula = Kyphosis ~ Age + Number + Start, data = kyphosis) 

# Use USAArrest data that available on RStudio
data("USArrests")

states=row.names(USArrests) 
states

# The columns of the data set contain the four variables.
names(USArrests )
# We first briefly examine the data. We notice that the variables have vastly different means.
# Note that the apply() function allows us to apply a function—in this case, the mean() function to each row or column of the data set.
# The second input here denotes whether we wish to compute the mean of the rows, 1, or the columns, 2.
# We see that there are on average three times as many rapes as murders, and more than eight times as many assaults as rapes.
apply(USArrests , 2, mean)

# We can also examine the variances of the four variables using the apply() function.
apply(USArrests , 2, var)

# We now perform principal components analysis using the prcomp() function, which is one of several functions in R that perform PCA.
# By default, the prcomp() function centers the variables to have mean zero. By using the option scale=TRUE, 
# The output from prcomp() contains a number of useful quantities.
pr.out=prcomp(USArrests, scale=TRUE)
names(pr.out)

# The center and scale components correspond to the means and standard deviations of the variables that were used for scaling prior to implementing PCA.
pr.out$center
pr.out$scale
# We see that there are four distinct principal components.
pr.out$rotation

dim(pr.out$x)

# We can plot the first two principal components as follows:
biplot(pr.out, scale=0)

pr.out$sdev
pr.var = pr.out$sdev^2
pr.var

pve = pr.var/sum(pr.var)
pve
# PCA with iris dataset
data("iris")
head(iris)
# creating another dataset from iris dataset that contains the columns from 1 to 4 
irisdata1 <- iris[,1:4]
irisdata1

head(irisdata1)
# Read the documentation for the princomp() function in RStudio.
help("princomp")
principal_components <- princomp(irisdata1, cor = TRUE, score = TRUE)
summary(principal_components)
# in the summary you can see that it has four Principal Components it is because the input data has four different features.
# using the plot() function, we can plot the principal components.
plot(principal_components)
# plotting the principal_components using the a line in plot() functions 
plot(principal_components, type = "l")
# using rhw biplot() function we can plot the components
help("biplot")
biplot(principal_components)
install.packages('MASS')
data(Boston, package="MASS")
# Read the documentation of Boston dataset in RStudio to understand the dataset
help(Boston)
# Principal Component Analysis
# the prcomp() fucntion computes the principal components and we have turned on scalling
# Read the documentation for prcompt() function in RStudio
help(prcomp)
pca_out <- prcomp(Boston,scale. = T)
# pca_out shows the loadings that used.
pca_out
plot(pca_out)

# plotting using the biplot()
# Read the documentation for biplot() function in RStudio
help(biplot)

biplot(pca_out, scale = 0)
boston_pc <- pca_out$x
boston_pc
# boston_pc has the Princial Components having the same number of rows in the original dataset
head(boston_pc)
summary(boston_pc)


