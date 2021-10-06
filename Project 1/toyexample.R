x <- c(4, 4, 3.5, 4, 2, 2.5, 1.5)
y <- c(1, 0.5, 2, 2, 3.5, 3, 4)
t <- 1:7
x <- cbind(rep(1, 7), x)
lm_toy <- OLS(y, x)

pred <- OLSPredict(lm_toy, t(c(1, 0.5)), 0.1)
