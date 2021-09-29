#Install package ggplot
require(ggplot2)

#Load files
X <- c(4, 4, 3.5, 4, 2, 2.5, 1.5)
Y <- c(1, 0.5, 2, 2, 3.5, 3, 4)

df = data.frame(t = 1:length(X), X, Y)

#Visualize dataset
colors <- c("X" = "blue", "Y" = "red")
ggplot(df, aes(x = t)) + geom_line(aes(y = X, color = "X")) +
  geom_line(aes(y = Y, color = "Y")) + 
  labs(x = "t", y = "value", color = "Values") + 
  scale_color_manual(values = colors)

#Create linear model (Y as a function of X)

lm1 <- lm(Y ~ X, data = df)
summary(lm1)

predict(lm1, newdata = list(X = 0.5))
pred <- predict(lm1, newdata = list(X = 0.5), interval = "predict", level = 0.9)
X_new = c(X, 0.5)
Y_new = c(Y, predict(lm1, newdata = list(X = 0.5)))
df_new = data.frame(t = 1:length(X_new), X = X_new, Y_new)
ggplot(df_new, aes(x = t)) + geom_line(aes(y = X_new, color = "X")) +
  geom_line(aes(y = Y_new, color = "Y")) +
  geom_segment(aes(x = 8, y = pred[2], xend = 8, yend = pred[3]), color = "red", 
               size = 1) +
  labs(x = "t", y = "value", color = "Values") +
  scale_color_manual(values = colors)
#Create linear model (Y as a function of t)

lm2 <- lm(Y ~ t, data = df)
summary(lm2)
pred_t <- predict(lm2, newdata = list(t = 8), interval = "predict", level = 0.9)
pred_t
Y_new = c(Y, pred_t[1])
df_new <- data.frame(t = 1:8, Y = Y_new)
ggplot(df_new, aes(x = t, y = Y_new)) + geom_line() + 
  geom_segment(aes(x = 8, y = pred_t[2], xend = 8, yend = pred_t[3]), size = 1)
