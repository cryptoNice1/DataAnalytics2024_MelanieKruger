##Linear Regression -----------------

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
eq.all <- gas_price ~ Biogas.CHP + Coal.and.peat.CHP + Liquid.biofuels.CHP +
  Natural.gas.CHP + Nuclear.CHP + Oil.CHP + Other.non.renewable.energy.CHP + 
  Renewable.municipal.waste.CHP + Solid.biofuels.CHP + Biogas.comm + 
  Coal.and.peat.comm + Geothermal.energy.comm + Liquid.biofuels.comm + 
  Natural.gas.comm + Oil.comm + Other.non.renewable.energy.comm + 
  Other.renewable.energy.comm + Renewable.municipal.waste.comm +
  Solar.thermal.energy.comm + Solid.biofuels.comm + fossil_dep + oil_dep +
  gas_dep + Biogas.Cap + Fossil.fuels.n.e.s..Cap + Geothermal.energy.Cap +
  Liquid.biofuels.Cap + Mixed.Hydro.Plants.Cap + Nuclear.Cap + 
  Offshore.wind.energy.Cap + Onshore.wind.energy.Cap + 
  Other.non.renewable.energy.Cap + Pumped.storage.Cap + 
  Renewable.hydropower.Cap + Renewable.municipal.waste.Cap + 
  Solar.photovoltaic.Cap + Solid.biofuels.Cap + Population +
  Multiple.renewables..Inv + Onshore.wind.energy.Inv
#get rid of elec_price

linear.fit <- lm(eq.all, X_train)
summary(linear.fit)
y_predicted <- predict(linear.fit, X_test[,-41])
length(y_predicted)
dim(X_test)

plot(y_predicted, X_test$gas_price, 
     main = "Residuals for gas price")
abline(a=0,b=1, col = "darkgreen")


#set the variables to everyone
eq.all <- elec_price ~ Biogas.CHP + Coal.and.peat.CHP + Liquid.biofuels.CHP +
  Natural.gas.CHP + Nuclear.CHP + Oil.CHP + Other.non.renewable.energy.CHP + 
  Renewable.municipal.waste.CHP + Solid.biofuels.CHP + Biogas.comm + 
  Coal.and.peat.comm + Geothermal.energy.comm + Liquid.biofuels.comm + 
  Natural.gas.comm + Oil.comm + Other.non.renewable.energy.comm + 
  Other.renewable.energy.comm + Renewable.municipal.waste.comm +
  Solar.thermal.energy.comm + Solid.biofuels.comm + fossil_dep + oil_dep +
  gas_dep + Biogas.Cap + Fossil.fuels.n.e.s..Cap + Geothermal.energy.Cap +
  Liquid.biofuels.Cap + Mixed.Hydro.Plants.Cap + Nuclear.Cap + 
  Offshore.wind.energy.Cap + Onshore.wind.energy.Cap + 
  Other.non.renewable.energy.Cap + Pumped.storage.Cap + 
  Renewable.hydropower.Cap + Renewable.municipal.waste.Cap + 
  Solar.photovoltaic.Cap + Solid.biofuels.Cap + Population +
  Multiple.renewables..Inv + Onshore.wind.energy.Inv
#get rid of elec_price

linear.fit <- lm(eq.all, X_train)
summary(linear.fit)
y_predicted <- predict(linear.fit, X_test[,-42])
plot(y_predicted, X_test$elec_price, 
     main = "Residuals for gas price")
abline(a=0,b=1, col = "darkgreen")


# Plotting just for one variable ----------------
one_var_reg <- function(col_name){
  ag <- numeric_only[order(numeric_only[[col_name]]),]
  plot(ag$gas_price ~ ag[[col_name]])
  small_model <- lm(ag$gas_price ~ ag[[col_name]])
  abline(small_model, col = "darkgreen")
  conf_interval  <- predict(small_model, newdata = ag, 
                            interval = 'confidence',
                            level=0.95)
  
  lines(ag[[col_name]], conf_interval[, 2], col = "blue", lty = 2)
  lines(ag[[col_name]], conf_interval[, 3], col = "blue", lty = 2)
  
}
one_var_reg("Mixed.Hydro.Plants.Cap")
one_var_reg("Solid.biofuels.comm")

## LASSO ---------------
library(glmnet)

lasso_model <- function(colname, i, colstring){
  #separating the data into X and Y
  y <- X_train[[colname]]
  x <- data.matrix(X_train[,-i])
  
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
  y_predicted <- predict(best_model, s = best_lambda, newx = data.matrix(X_test[,-i]))

  plot(X_test[[colname]], y_predicted,
       main = paste("LASSO Predicted",colstring),
       ylab = "Predicted Value",
       xlab = "Actual Value")
  abline(a=0,b=1, col = "darkgreen")
  
}
lasso_model("gas_price",41,"gas prices")
lasso_model("elec_price",40,"electricity prices")
