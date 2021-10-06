OLS <- function(y, x){
  x <- as.matrix(x)
  y <- as.matrix(y)
  f = list()
  f$df <- nrow(x) - ncol(x)
  f$coef <- solve(t(x) %*% x) %*% t(x) %*% y
  f$fitted_values <- t(x %*% f$coef)
  f$residuals <- t(y - t(f$fitted_values))
  f$std_error <- sqrt(((f$residuals) %*% t(f$residuals)) / (f$df))
  f$X <- t(x) %*% x
  
  return(f)
}

GLMPredict <- function(md, x, level=0.05){
  x <- as.matrix(x)
  res <- matrix(0, nrow = nrow(x), ncol = 3)
  res[, 1] <- x %*% md$coef
  for (i in 1:nrow(x)){
    res[i, 2] <- res[i, 1] - qt(1 - level / 2, df = md$df) *
      md$std_error * sqrt(1 + t(x[i,]) %*% solve(md$X) %*% x[i,])
    res[i, 3] <- res[i, 1] + qt(1 - level / 2, df = md$df) *
      md$std_error * sqrt(1 + t(x[i,]) %*% solve(md$X) %*% x[i,])
  }
  colnames(res) <- c("fitted", "pred_low", "pred_high")
  return(res)
}

WLS <- function(y, x, Sigma){
  x <- as.matrix(x)
  y <- as.matrix(y)
  f = list()
  f$df <- nrow(x) - ncol(x)
  f$coef <- solve(t(x) %*% solve(Sigma) %*% x) %*% t(x) %*% solve(Sigma) %*% y
  f$fitted_values <- t(x %*% f$coef)
  f$residuals <- t(y - t(f$fitted_values))
  f$std_error <- sqrt(((f$residuals) %*% Sigma %*% t(f$residuals)) / (f$df))
  f$X <- t(x) %*% x
  
  return(f)
}