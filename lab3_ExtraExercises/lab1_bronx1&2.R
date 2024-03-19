#open up the file ... use cow
rm(list=ls())

library(readxl)
setwd("C:\\Users\\Melanie\\OneDrive - Rensselaer Polytechnic Institute\\Desktop\\School\\24_Spring\\Data Analytics\\Lab3")
bronx1 <- read_excel("rollingsales_bronx.xls",skip=4)
str(bronx1)

#change some of the names that are formatted weird
colnames(bronx1)
names(bronx1)[names(bronx1) == "SALE\nPRICE"] <- 'SALE.PRICE'
names(bronx1)[names(bronx1) == "GROSS SQUARE FEET"] <- 'GROSS.SQUARE.FEET'
names(bronx1)[names(bronx1) == "LAND SQUARE FEET"] <- 'LAND.SQUARE.FEET'
names(bronx1)[names(bronx1) == "BUILDING CLASS CATEGORY"] <- 'BCC'
colnames(bronx1) <- gsub("[\n\r]",".",colnames(bronx1))
colnames(bronx1) <- gsub(" ",".",colnames(bronx1))
colnames(bronx1)

#### Bronx1 -------------
#plot to see if there's a relationship between size of the apartment and the price
plot(log(bronx1$GROSS.SQUARE.FEET), log(bronx1$SALE.PRICE))

#get rid of zeros (and getting rid of the variables we don't care about)
bronx1_subset <- bronx1[,c("SALE.PRICE","GROSS.SQUARE.FEET","LAND.SQUARE.FEET","NEIGHBORHOOD","BCC")]
bronx1_subset[bronx1_subset==0] <- NA
bronx1_subset <- na.omit(bronx1_subset)

#run regression
library(plm)
m1 <- lm(log(SALE.PRICE)~log(GROSS.SQUARE.FEET), data = bronx1_subset)
summary(m1)
abline(m1,col="red",lwd=2)

plot(resid(m1))

# Model 2
eq.2 <- log(SALE.PRICE)~log(GROSS.SQUARE.FEET)+log(LAND.SQUARE.FEET)+factor(NEIGHBORHOOD)
m2 <- lm(eq.2, bronx1_subset)
summary(m2)
plot(resid(m2))

# Model 2 without intercept
eq.2a <- log(SALE.PRICE)~log(GROSS.SQUARE.FEET)+log(LAND.SQUARE.FEET)+factor(NEIGHBORHOOD)-1
m2a<-lm(eq.2a, bronx1_subset)
summary(m2a)

plot(resid(m2a))

# Model 3
eq3 <- log(SALE.PRICE)~0+log(GROSS.SQUARE.FEET)+log(LAND.SQUARE.FEET)+factor(NEIGHBORHOOD)+factor(BCC)
m3<-lm(eq3, bronx1_subset)
summary(m3)
plot(resid(m3))

# Model 4
eq4 <- log(SALE.PRICE)~0+log(GROSS.SQUARE.FEET)+log(LAND.SQUARE.FEET)+factor(NEIGHBORHOOD)*factor(BCC)
m4<-lm(eq4, bronx1_subset)
summary(m4)
plot(resid(m4))


#### Bronx2 ----------------
bronx1$SALE.PRICE<-sub("\\$","",bronx1$SALE.PRICE) 
bronx1$SALE.PRICE<-as.numeric(gsub(",","", bronx1$SALE.PRICE)) 
bronx1$GROSS.SQUARE.FEET<-as.numeric(gsub(",","", bronx1$GROSS.SQUARE.FEET)) 
bronx1$LAND.SQUARE.FEET<-as.numeric(gsub(",","", bronx1$LAND.SQUARE.FEET)) 
bronx1$SALE.DATE<- as.Date(gsub("[^]:digit:]]","",bronx1$SALE.DATE)) 
bronx1$YEAR.BUILT<- as.numeric(gsub("[^]:digit:]]","",bronx1$YEAR.BUILT)) 
bronx1$ZIP.CODE<- as.character(gsub("[^]:digit:]]","",bronx1$ZIP.CODE)) 

minprice<-10000
bronx1<-bronx1[which(bronx1$SALE.PRICE>=minprice),]
nval<-dim(bronx1)[1]


#### stuff i couldn't figure out -----------
##change up the address
#first, get rid of the spaces in the address 
library(gdata)
bronx1$ADDRESSONLY <- gsub("[,][[:print:]]*", "", gsub("[ ]+", "", trim(bronx1$ADDRESS)))

#put unique addresses and zip codes a new dataframe and set column names
bronxadd <- unique(data.frame(bronx1$ADDRESSONLY, bronx1$ZIP.CODE, stringsAsFactors=FALSE))
names(bronxadd) <- c("ADDRESSONLY", "ZIP.CODE")

#determines which lines contain an address that is repeated is in the dataset
duplicates <- duplicated(bronx1$ADDRESSONLY)

#get all the rows that contain duplicate addresses
for(i in 1:2) { #2345
  if(duplicates[i]==FALSE){ #if there is not a duplicate
    #add the ...???
    print(bronxadd[duplicates[i],1])
  }
}



nsample=450 #set the sample size
addsample<-bronxadd[sample.int(dim(bronxadd)[1],size=nsample),]#I use nval here 
# may need to install this package
library(ggmap)
addrlist<-paste(addsample$ADDRESSONLY, "NY", addsample$ZIP.CODE, "US", sep=" ") 
querylist<-geocode(addrlist) #This is cool. Take a break.

matched<-(querylist$lat!=0 &&querylist$lon!=0) addsample<-cbind(addsample,querylist$lat,querylist$lon) 
names(addsample)<-c("ADDRESSONLY","ZIPCODE","Latitude","Longitude")# correct the column na adduse<-merge(bronx1,addsample)

adduse<-adduse[!is.na(adduse$Latitude),]
mapcoord<-adduse[,c(2,3,24,25)]

table(mapcoord$NEIGHBORHOOD)

mapcoord$NEIGHBORHOOD <- as.factor(mapcoord$NEIGHBORHOOD)
map <- get_map(location = 'Bronx', zoom = 12)#Zoom 11 or 12
ggmap(map) + geom_point(aes(x = mapcoord$Longitude, y = mapcoord$Latitude, size =1, color=mapcoord$NEIGHBORHOOD), data = mapcoord) +theme(legend.position = "none") 

#It would be perfect if I can decrease the size of points 
mapmeans<-cbind(adduse,as.numeric(mapcoord$NEIGHBORHOOD))
colnames(mapmeans)[26] <- "NEIGHBORHOOD" #This is the right way of renaming.

keeps <- c("ZIP.CODE","NEIGHBORHOOD","TOTAL.UNITS","LAND.SQUARE.FEET","GROSS.SQUARE.FEET","SALE.PRICE","Latitude","Longitude") 
mapmeans<-mapmeans[keeps]#Dropping others
mapmeans$NEIGHBORHOOD<-as.numeric(mapcoord$NEIGHBORHOOD) 

for(i in 1:8){
  mapmeans[,i]=as.numeric(mapmeans[,i]) 
}#Now done for conversion to numeric

#Classification
mapcoord$class<as.numeric(mapcoord$NEIGHBORHOOD)
nclass<-dim(mapcoord)[1]
split<-0.8
trainid<-sample.int(nclass,floor(split*nclass))
testid<-(1:nclass)[-trainid]

##mappred<-mapcoord[testid,] # What would you use this for?
##mappred$class<as.numeric(mappred$NEIGHBORHOOD) 

kmax<-10
knnpred<-matrix(NA,ncol=kmax,nrow=length(testid))
knntesterr<-rep(NA,times=kmax)
for (i in 1:kmax){		# loop over k
  knnpred[,i]<-knn(mapcoord[trainid,3:4],mapcoord[testid,3:4],cl=mapcoord[trainid,2],k=i)
  knntesterr[i]<-sum(knnpred[,i]!=mapcoord[testid,2])/length(testid)
} 
knntesterr

#Clustering
mapobj<-kmeans(mapmeans,5, iter.max=10, nstart=5, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
fitted(mapobj,method=c("centers","classes"))
mapobj$centers
#
library(cluster)
clusplot(mapmeans, mapobj$cluster, color=TRUE, shade=TRUE, labels=2, lines=0) 
#
library(fpc)#May need to install.packages("fpc")
plotcluster(mapmeans, mapobj$cluster)
#
mapmeans1<-mapmeans[,-c(1,3,4)]
mapobjnew<-kmeans(mapmeans1,5, iter.max=10, nstart=5, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
fitted(mapobjnew,method=c("centers","classes"))
clusplot(mapmeans1, mapobjnew$cluster, color=TRUE, shade=TRUE, labels=2, lines=0) 
plotcluster(mapmeans1, mapobjnew$cluster)
ggmap(map) + geom_point(aes(x = mapcoord$Longitude, y = mapcoord$Latitude, size =1, color=mapobjnew$cluster), data = mapcoord)#How to change colors?
