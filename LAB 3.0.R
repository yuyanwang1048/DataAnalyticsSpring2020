# Lab3
# wine data
wine_data <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep = ",")
head(wine_data)
# there is no variable names and we need to add them
nrow(wine_data) # there are 178 rows
dim(wine_data) 
colnames(wine_data) <- c("Cvs", "Alcohol", 
                         "Malic_Acid", "Ash", "Alkalinity_of_Ash", 
                         "Magnesium", "Total_Phenols", "Flavanoids", "NonFlavanoid_Phenols",
                         "Proanthocyanins", "Color_Intensity", "Hue", "OD280/OD315_of_Diluted_Wine", 
                         "Proline")
head(wine_data) # Now you can see the header names.
# Using heatmap to check the correlation
?heatmap
heatmap(cor(wine_data),Rowv = NA, Colv = NA) 
help(factor)
# declaring the cultivar_classes using the factor() function each cultivar Cv1,Cv2 and Cv3.
cultivar_classes <- factor(wine_data$Cvs) 
cultivar_classes
# PCA
wine_data_PCA <- prcomp(scale(wine_data[,-1]))
summary(wine_data_PCA)


# data titanic
library(titanic)
titanic_clean_train<-na.omit(titanic_train)
titanic_clean_train<- titanic_clean_train[, -c(1,4,9,11)]

library(dummies)
titanic_dum<-dummy.data.frame(titanic_clean_train,sep='.')
titanic_dum
# rpart
require(rpart)
titanic_rpart <- rpart( Survived ~ ., data = titanic_clean_train)
plot(titanic_rpart) 
text(titanic_rpart) 
# ctree
require(party)
head(titanic_dum)
titanic_ctree<-ctree( Survived ~ ., data = titanic_dum)
plot(titanic_ctree)

# hclust
?hclust
d<-dist(titanic_dum)
titanic_hclust<-hclust(d)
plot(titanic_hclust)

#random forest
library(randomForest)
titanic_rf<-randomForest(Survived ~ ., data = titanic_dum)
plot(titanic_rf)

