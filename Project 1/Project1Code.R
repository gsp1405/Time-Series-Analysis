#Load packages
require(magic)
require(ggplot2)
require(dplyr)
source("Functions.R")

#Load dataset and convert pitstop_lagged variable to boolean
load("DataA1.RData")
data <- AllDat
data$pitstop <- as.numeric(data$pitstop)

#Extraxt each driver into drivers list
n_drivers = max(data[, "driverId"])
n_laps = max(data[, "lap_number"])
drivers <- list()

for (i in 1:n_drivers){
  drivers[[i]] <- filter(data, driverId == i)
}


#Plot laptimes for driver 9
ggplot(drivers[[9]], aes(x = lap_number, y = lap_time)) + geom_line() +
  geom_point(aes(color = as.logical(pitstop))) +
  labs(title = "Lap times of driver 9", x = "Lap number", y = 
         "Lap time(miliseconds)", color = "Pitstop") + 
  theme(plot.title = element_text(hjust = 0.5))


#Include the intercept for each individual driver in the design matrix as well
#as the indicator for the first lap
intercepts <- list()
for (i in 1:n_drivers){
  intercepts[[i]] = as.numeric(data[, "driverId"] == i)
}
intercepts <- bind_cols(intercepts)
data <- cbind(intercepts, data)
names(data)[1:n_drivers] <- paste("alpha_", 1:n_drivers, sep = "")

#Include the indicator for th first lap
first_lap <- as.numeric(data[, "lap_number"] == 1)
data <- cbind(first_lap, data)
names(data)[1] <- "first_lap"

#Split the data into training and testing
data_train <- list()
data_test <- list()
for (i in 1:n_drivers){
  driver <- filter(data, driverId == i)
  data_train[[i]] <- na.omit(driver[1:51, ])
  data_test[[i]] <- na.omit(driver[52:58, ])
}
data_train <- bind_rows(data_train)
data_test <- bind_rows(data_test)

#Obtain indices for driver 9 in training data
idx9_train <- which(data_train[, "alpha_9"] == 1)
idx9_test <- which(data_test[, "alpha_9"] == 1)

#Split datasets into X and Y and remove driverId from X
X_train <- subset(data_train, select = -c(lap_time, driverId))
X_test <- subset(data_test, select = -c(lap_time, driverId))
Y_train <- data_train[, "lap_time"]
Y_test <- data_test[, "lap_time"]

#Construct linear model with an intercept for each individual driver and
#relationship for every lap number and pitstop indicator
lmodel1 <- lm(lap_time ~ . - driverId + 0, data = data_train)

lm1 <- (OLS(Y_train, X_train))

X_test9 <- filter(X_test, alpha_9 == 1)

#Predict lap times for driver 9
Y_hat9 <- GLMPredict(lm1, X_test9)

#Obtain measure of uncertainty of model (standard error)
lm1$std_error



#Extract fitted values for driver 9
driver9_test <- filter(data_test, alpha_9 == 1)
driver9_fit <- lm1$fitted_values[idx9_train]

#Append predicted test data for driver 9
driver9_fit <- c(driver9_fit, Y_hat9[, 1])


#Plot predicted times for driver 9
plot(1:n_laps, drivers[[9]]$lap_time, type = "l", xlab = "Lap number",
     ylab = "Time (miliseconds)", main = "Predicted lap times of driver 9",
     ylim = c(85000, 120000), col = 1, lwd = 1)
lines(driver9_fit, col = 2, lwd = 2)
matlines(52:58, Y_hat9[, 2:3], col = 3, lty = 1, lwd = 2)
legend("topright", legend = 
         c("Observed times", "Predicted times", 
           "95% confidence interval"),
       col = 1:3, lty = 1, lwd = 2, cex = 0.8)

#Plot the prediction errors
lm1_error <- drivers[[9]]$lap_time - driver9_fit
plot(lm1_error, xlab = "Lap number",
     ylab = "Error (miliseconds)", main = "Prediction errors for driver 9")
abline(0, 0)


#Use the attached commands to obtain the covariance matrix Sigma
LapsPerDriver <- c()
for(k in unique(AllDat$driverId)){
  LapsPerDriver <- c(LapsPerDriver,sum(AllDat$driverId==k))
}
LapsPerDriver_train <- pmin(LapsPerDriver,51)
rho <- rep(0.5, 21)
Sigma <- matrix(0,ncol=0,nrow=0)
for(k in seq_along(LapsPerDriver_train)){
  N <- LapsPerDriver_train[k]
  O <- matrix(data = rep(0:(N-1),N),ncol=N)
  pow <- abs(t(O)-O)
  Block <- rho[k]^pow
  Sigma <- adiag(Sigma,Block)
}

#Construct WLS model
lm2 <- WLS(Y_train, X_train, Sigma = Sigma)
lm2$coef
lm2$std_error
Y_hat9_WLS_fitted <- lm2$fitted_values[idx9_train]

#Predict the lap times with the WLS model
Y_hat9_WLS <- GLMPredict(lm2, X_test9)
Y_hat9_WLS_fitted <- c(Y_hat9_WLS_fitted, Y_hat9_WLS[,1])

#Plot the predictions of driver 9 with WLS model
plot(1:n_laps, drivers[[9]]$lap_time, type = "l", xlab = "Lap number",
     ylab = "Time (miliseconds)", main = "Predicted lap times of driver 9",
     ylim = c(85000, 120000), col = 1, lwd = 1)
lines(Y_hat9_WLS_fitted, col = 2, lwd = 2)
matlines(52:58, Y_hat9_WLS[, 2:3], col = 3, lty = 1, lwd = 2)
legend("topright", legend = 
         c("Observed times", "Predicted times", 
           "95% confidence interval"),
       col = 1:3, lty = 1, lwd = 2, cex = 0.8)

#Plot the prediction errors
lm2_error <- drivers[[9]]$lap_time - Y_hat9_WLS_fitted
plot(lm2_error, xlab = "Lap number",
     ylab = "Error (miliseconds)", main = "Prediction errors for driver 9")
abline(0, 0)
hist(lm2_error, breaks = 30)

#Scale the covariance matrix by a parameter gamma
gamma_pit <- 1.5
tmp <- data_train$pitstop
Sigma <- Sigma*(gamma_pit^outer(tmp,tmp,FUN="+"))

tmp <- data_train$first_lap
Sigma <- Sigma*(gamma_pit^outer(tmp,tmp,FUN="+"))

tmp <- data_train$pitstop_lagged
Sigma <- Sigma*(gamma_pit^outer(tmp,tmp,FUN="+"))

#Reconstruct the WLS model with the new covariance matrix
lm3 <- WLS(Y_train, X_train, Sigma = Sigma)
lm3$coef
lm3$std_error
Y_hat9_WLS2_fitted <- lm3$fitted_values[idx9_train]

#Predict the lap times with the updated WLS model
Y_hat9_WLS2 <- GLMPredict(lm3, X_test9)
Y_hat9_WLS2_fitted <- c(Y_hat9_WLS2_fitted, Y_hat9_WLS2[, 1])

#Plot the predictions of driver 9 with updated WLS model
plot(1:n_laps, drivers[[9]]$lap_time, type = "l", xlab = "Lap number",
     ylab = "Time (miliseconds)", main = "Predicted lap times of driver 9",
     ylim = c(80000, 120000), col = 1, lwd = 1)
lines(Y_hat9_WLS2_fitted, col = 2, lwd = 2)
matlines(52:58, Y_hat9_WLS2[, 2:3], col = 3, lty = 1, lwd = 2)
legend("topright", legend = 
         c("Observed times", "Predicted times", 
           "95% confidence interval"),
       col = 1:3, lty = 1, lwd = 2, cex = 0.8)

#Plot the prediction errors
lm3_error <- drivers[[9]]$lap_time - Y_hat9_WLS2_fitted
plot(lm3_error, xlab = "Lap number",
     ylab = "Error (miliseconds)", main = "Prediction errors for driver 9")
abline(0, 0)
