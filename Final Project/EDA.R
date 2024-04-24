rm(list=ls())
library(readxl)
library(plm)

dir.str <- "C:\\Users\\Melanie\\OneDrive - Rensselaer Polytechnic Institute\\Desktop\\School\\24_Spring\\Data Analytics\\DA_Project\\New Datasets"
setwd(dir.str)
main <- read.csv("main.csv")
str(main)

#Changing things ----------------
main$Medium.Household.Gas.Prices..gJ. <- as.numeric(main$Medium.Household.Gas.Prices..gJ.)
main$Country <- factor(main$Country)
main$Year <- factor(main$Year)
main$X <- NULL

#changing some of the names
colnames(main)[3] <- "fossil_dep"
colnames(main)[4] <- "oil_dep"
colnames(main)[5] <- "gas_dep"
colnames(main)[25] <- "elec_price"
colnames(main)[26] <- "gas_price"

str(main)

# doing some exploratory data analysis -----------------
library(ggplot2)
library(ggcorrplot)

numeric_only <- data.table::copy(main)
numeric_only$Country <- as.numeric(factor(main$Country,levels=unique(main$Country)))
numeric_only$Year <- as.numeric(main$Year)

for (i in colnames(numeric_only)){
  print(paste(i,sd(na.omit(numeric_only[[i]]))))
}

#looking at a plot for all of them to determine the standard deviation
histogram_plot <- function(name){
  column <- numeric_only[[name]]
  col.sd <- round(sd(na.omit(numeric_only[[name]])),digits=4)
  png(filename=paste(dir.str,"\\Each_Histogram\\",name,".png",sep=""))
  hist(column,
       main = paste(name," sd: ",col.sd,sep=""),
       xlab = name)
  dev.off()
}

all_names <- colnames(numeric_only)
#plotting each of the columns
for (i in colnames(numeric_only)){
  histogram_plot(i)
}

#getting rid of the columns that don't make sense
numeric_only$Biogas.Inv <- NULL
numeric_only$Coal.and.peat.Cap <- NULL
numeric_only$Coal.and.peat.Inv <- NULL
numeric_only$Concentrated.solar.power.Inv <- NULL
numeric_only$Fossil.fuels.n.e.s..Inv <- NULL
numeric_only$Geothermal.energy.Inv <- NULL
numeric_only$Liquid.biofuels.Inv <- NULL
numeric_only$Marine.energy.Cap <- NULL
numeric_only$Marine.energy.Inv <- NULL
numeric_only$Natural.gas.Cap <- NULL
numeric_only$Natural.gas.Inv <- NULL
numeric_only$Non.renewable.municipal.waste.Inv <- NULL
numeric_only$Off.grid.Solar.photovoltaic.Inv<- NULL
numeric_only$Oil.Inv <- NULL
numeric_only$Pumped.storage.Inv <- NULL
numeric_only$Renewable.municipal.waste.Inv <- NULL
numeric_only$Solar.thermal.energy.Inv <- NULL
numeric_only$Oil.Cap <- NULL
numeric_only <- na.omit(numeric_only)

plot.size <- 1000
png(filename=paste(dir.str,"\\CorrelationMatrix.png",sep=""),
    width = plot.size, height = plot.size)
ggcorrplot(cor(numeric_only),
           type = "upper",
           show.diag = F,
           lab=T,
           lab_size=3)
dev.off()

## Doing factor analysis --------------
#get the eigenvalues from the correlation matrix
eigenvalues <- eigen(cor(numeric_only))$values

#plotting the eigenvalues
barplot(eigenvalues, ylab = "Eigenvalue", xlab="Eigenvalue Number",
        main="Correlation Matrix Eigenvalues",
        names.arg = 1:length(eigenvalues))
abline(h = 1, col = "red")

#the result from this is that we want 9 factors
factanal(x = na.omit(numeric_only), factors = 9, lower=0.05)

##Doing PCA ---------------
library(stats)
library(factoextra)
library("corrplot")
ggcorrplot(cor(scale(numeric_only[-1])))
components <- prcomp(scale(numeric_only[-1]))
summary(components)

#since 99% is pretty good for the amount of explained variance, we find what 
# value reaches that threshold. 
var <- get_pca_var(components)
head(var$cos2)

#make a plot that shows the correlation between the variables to the components,
# which can further prove how many dimensions to use in the end
corrplot(var$cos2, is.corr=FALSE)


linear.fit <- lm(eq.all, X_train)

## Doing some OLS regressions ----------
#print out all the variables
all_variables <- ""
for (i in colnames(numeric_only)){
  all_variables <- paste(all_variables," + ",i,sep="")
}
all_variables


set.seed(420)
n <- dim(numeric_only)[1]
test.ind <- sample(n, n/3)
X_test <- numeric_only[test.ind,]
X_train <- numeric_only[-test.ind,]

#set the variables to everyone
eq.all <- gas_price ~ fossil_dep + oil_dep + gas_dep + Biogas.Cap +
  Fossil.fuels.n.e.s..Cap + Geothermal.energy.Cap + Liquid.biofuels.Cap +
  Mixed.Hydro.Plants.Cap + Nuclear.Cap + Offshore.wind.energy.Cap + 
  Onshore.wind.energy.Cap + Other.non.renewable.energy.Cap + 
  Pumped.storage.Cap + Renewable.hydropower.Cap + Renewable.municipal.waste.Cap + 
  Solar.photovoltaic.Cap + Solar.thermal.energy.Cap + Solid.biofuels.Cap + 
  Population + Multiple.renewables..Inv + Offshore.wind.energy.Inv + 
  On.grid.Solar.photovoltaic.Inv + Onshore.wind.energy.Inv + 
  Other.primary.solid.biofuels.n.e.s..Inv + Renewable.hydropower.Inv +
  invren + invrnonre
  #get rid of elec_price

linear.fit <- lm(eq.all, X_train)
summary(linear.fit)

## Doing LASSO --------------------------
library(glmnet)

#separating the data into X and Y
y <- X_train$gas_price
x <- data.matrix(X_train[,-22])

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

#plot the model
plot(cv_model)

#find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

#predicting using that model
y_predicted <- predict(best_model, s = best_lambda, newx = data.matrix(X_test[,-22]))

plot(y_predicted, X_test$gas_price)
abline(a=0,b=1, col = "darkgreen")

