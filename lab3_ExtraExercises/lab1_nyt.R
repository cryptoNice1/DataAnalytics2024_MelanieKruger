library(kknn)

nyt1<-read.csv("nyt1.csv")

#get only the articles which people have actually interacted with
nyt1<-nyt1[which(nyt1$Impressions>0 & nyt1$Clicks>0 & nyt1$Age>0),]

#get the number of rows in the data
nnyt1<-dim(nyt1)[1]		# shrink it down!

sampling.rate <- 0.9
num.test.set.labels <- nnyt1*(1.-sampling.rate)

#get training and testing data using just age and impressions
training <-sample(1:nnyt1,sampling.rate*nnyt1, replace=FALSE)

train<-subset(nyt1[training,],select=c(Age,Impressions))
testing<-setdiff(1:nnyt1,training)
test<-subset(nyt1[testing,],select=c(Age,Impressions))

#these are the labels we're interested in
cg<-nyt1$Gender[training]
true.labels<-nyt1$Gender[testing]

#running the knn
classif<-kknn(cg~., 
              train = data.frame(train), 
              test = data.frame(test),
              k=5) 
classif
attributes(.Last.value) 

