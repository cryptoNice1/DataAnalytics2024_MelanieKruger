rm(list=ls())
setwd("C:\\Users\\Melanie\\OneDrive - Rensselaer Polytechnic Institute\\Desktop\\School\\24_Spring\\Data Analytics\\Lab3")

#### Exercise 1 -----------------
set.seed(420)
par(mar = rep(0.2,4))
datamatrix <- matrix(rnorm(400),nrow=40)

#plot out the values
image(1:10, 1:40, t(datamatrix)[,nrow(datamatrix):1]) #regular heatmap
heatmap(datamatrix) #heatmap but with clustering

#add in a pattern to hte left-hand side 
for (i in 1:40){
  coinflip <- rbinom(1,size=1, prob=0.5)
  if (coinflip){
    datamatrix[i,] <- datamatrix[i,] + rep(c(0,3),each=5)
  }
}

#replot out values
heatmap(datamatrix)
image(1:10, 1:40, t(datamatrix)[,nrow(datamatrix):1]) #regular heatmap

#plotting the averages
hh <- hclust(dist(datamatrix))
datamatrix_ordered <- datamatrix[hh$order,]
par(mfrow = c(1,3))
image(t(datamatrixordered)[, nrow(datamatrixordered):1])
plot(rowMeans(datamatrixordered), 40:1, xlab="row means", ylab="row",pch=19)
plot(colMeans(datamatrixordered), 40:1, xlab="column", ylab="column means",pch=19)

#### Exercise 2 ---------------
abalone <- read.csv("abalone.csv")

#split the ring size into three categories
abalone$Rings
abalone$Age <- cut(abalone$Rings, br=c(-1,8,11,35),labels=c("young","adult","old"))
abalone$Age <- as.factor(abalone$Age)

# make a new data frame excluding sex (not numeric) and Rings
aba <- abalone
aba$Sex <- NULL
aba$Rings <- NULL
head(aba)

#normalize the data
normalize <- function(x){
  return ((x - min(x)/(max(x) - min(x))))
}
n <- dim(aba)[2] - 1 #number of columns to normalize
aba[1:n] <- as.data.frame(lapply(aba[1:n],normalize))

#split the data into testing and training
ind <- sample(2, nrow(aba), replace=T, prob=c(0.7,0.3))
KNN.train <- aba[ind == 1,]
KNN.test <- aba[ind == 2,]

#do KNN
library(class)
KNN.k = sqrt(dim(abalone)[1]) #get the number of clusters
KNN.pred <- knn(train = KNN.train[1:n], test= KNN.test[1:n], cl = KNN.train$Age, k = KNN.k)

#results
table(KNN.pred)

#### Exercise 3 - Clustering
data("iris")
str(iris)

#remove the fifth column
iris.X <- iris[1:4]
summary(iris.X)

#kmeans clustering
set.seed(420)
k.max <- 12 #number of clusters
wss <- sapply(1:k.max, function(k){kmeans(iris[,3:4],k,
                                   nstart=20,
                                   iter.max = 1000
                                   )$tot.withinss})
par(mfrow = c(1,1))
plot(1:k.max, wss, #plotting the within sum of squares 
     type="b",
     xlab = "Number of clusters",
     ylab = "Within cluster sum of squares")

clusters <- kmeans(iris.X, 3, nstart=20)
table(clusters$cluster, iris$Species)
