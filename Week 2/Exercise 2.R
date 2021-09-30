#Load packages
require(ggplot2)

#Load data set
data = c(4.4, 3.4, 3.3, 2.5, 7.3, 4.9, 4.8, 4.4)
#Log transform data
data <- log(data)
anemo_loc = c(rep(0, 4), rep(1, 4))
df = data.frame(t = 1:length(data), roof = anemo_loc, Y = data)
df$roof <- as.factor(df$roof)

ggplot(df, aes(x = t, y = Y, color = roof)) + geom_point()

#Define linear model
lm1 <- lm(Y ~ t + roof, data = df)
summary(lm1)

new_data = data.frame(t = 9, roof = as.factor(0))
pred <- predict(lm1, newdata = new_data, interval = "pred", level = 0.9)

#Add predicted value to data frame and visualize the result
df_new <- rbind(df, data.frame(t = 9, roof = as.factor(0), Y = pred[1]))
ggplot(df_new, aes(x = t, y = exp(Y), color = roof)) + geom_point() +
  scale_x_continuous(breaks = seq(1, 9, 1), minor_breaks = seq(1, 9, 1)) +
  ylim(c(1, 8)) + ylab("Wind speed") + xlab("Hour") +
  ggtitle("Copenhagen airport wind speed") + 
  theme(plot.title = element_text(hjust = 0.5))
